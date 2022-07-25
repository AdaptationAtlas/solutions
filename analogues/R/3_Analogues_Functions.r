#' Wrapper to run analogues function over ERA data
#' 
#' @export
#' @importFrom raster extract
# Points <=10 - Run practices in parallel (gets clogged up when some cores have a practice with large numbers of sites) =====
    run_points <- function(i, pr_df, ERA_data, cimdir, Threshold, Year, Scenario, vr, etype='pos', DoLite=F,Outcome) {
        
        ERA_data<-data.table(ERA_data)
        
        # Get practice and product name
        Practice<-pr_df[i,PrName]
        Product<-pr_df[i,Product.Simple]
        
        # Restructure data and classify points-----
        pdata <- ERA_data[PrName == Practice & Product.Simple == Product & Out.SubInd == Outcome
                         ][!(is.na(Lat) & is.na(Lon)),c("Lon","Lat","RR")]]        
       
        #positive yield impact  =====
        in_data <- pdata[RR >= Threshold
                        ][,ID:=paste("s",1:.N,sep="")]
        
        #output directory  =====
        pr_odir <- paste(cimdir,"/T",Threshold,"_v",vr,"/",Product,"/",Year,"/",gsub("[.]","_",Scenario),"/",gsub(" ", "_", Practice, fixed=T),"_v",vr,sep="")
        if (!file.exists(pr_odir)) {dir.create(pr_odir,recursive=T)}
        
        #verbose what i'm running
        cat("processing practice=", Practice, i, "of", nrow(pr_df), "/ crop=", Product, "/ n=", nrow(in_data),"\n")
        
        #loop through points for practice
        allstk <- sim1stk <- sim2astk <- sim2bstk <- c()
        for (j in 1:nrow(in_data)) {
          #cat("processing site=",j,"\n")
          if (!file.exists(paste(pr_odir,"/out_",etype,"_",in_data$ID[j],".RData",sep=""))) {
            #1. mean climate ####
            #cat("...mean climate\n")
            par1 <- createParameters(x=in_data$Lon[j], y=in_data$Lat[j], vars=c("prec","tmean"),weights=c(0.75,0.25),
                                     ndivisions=c(12,12),growing.season=c(1,12),
                                     rotation="prec",threshold=1,env.data.ref=list(wc_prec,wc_tmean), 
                                     env.data.targ=if(is.na(Year)){list(wc_prec,wc_tmean)}else{list(wc_prec_fut,wc_tmean_fut)},outfile=pr_odir,
                                     fname=NA,writefile=F)
            sim1 <- calc_similarity(par1)
            if (!inMemory(sim1)) {sim1 <- readAll(sim1)}
            
            #2. each soil variable, then combine in one by taking the median ####
            #cat("...soils ORCDRC, PHIHOX, CLYPPT, SNDPPT\n")
            simsol <- c()
            for (svar in c("CLYPPT","ORCDRC")) {
                #print(svar)
                if(!DoLite){soil_data <- stack(soilstk[[svar]])} else {soil_data <- soilstk2[[svar]]}
                parx <- createParameters(x=in_data$Lon[j], y=in_data$Lat[j], vars=c(svar),weights=1,
                                         ndivisions=c(3),growing.season=c(1,3),rotation="none",
                                         threshold=1,env.data.ref=list(soil_data), 
                                         env.data.targ=list(soil_data),outfile=pr_odir,
                                         fname=NA,writefile=F)
                simsol <- c(simsol,calc_similarity(parx))
            }
            sim2a <- simsol[[1]]
            if (!inMemory(sim2a)) {sim2a <- readAll(sim2a)}
            
            sim2b <- simsol[[2]]
            if (!inMemory(sim2b)) {sim2b <- readAll(sim2b)}
            
            rm(soil_data)
            gc()
            
            #4. combine all in a sensible layer and save individual layers and output as .RData ####
            #soil weight 50%, and climate indicators are divided equally over the remaining %
            #sim_i <- sim1 * 0.25 + sim2 * 0.5 + sim3 * 0.25
            #cat("...minimum similarity across all factors\n")
            sim_i <- stack(c(sim1, sim2a, sim2b)) %>%
              min(., na.rm=TRUE)
            if (!inMemory(sim_i)) {sim_i <- readAll(sim_i)}
            
            #5. put results in stack ####
            simstk <- stack(sim1, sim2a, sim2b, sim_i)
            names(simstk) <- c("meanclim","soil1","soil2","overall")
            if (!inMemory(simstk)) {simstk <- readAll(simstk)}
            
            #6. save results  ####
            #cat("...saving\n")
            save(list=c("simstk"), 
                 file=paste(pr_odir,"/out_",etype,"_",in_data$ID[j],".RData",sep=""),
                 compress="xz",compression_level=9)
            
            #clean-up
            gc(full=TRUE, verbose=FALSE)
          } else {
            #cat("...already processed, hence loading\n")
            load(paste(pr_odir,"/out_",etype,"_",in_data$ID[j],".RData",sep=""))
          }
          allstk <- c(allstk, simstk[["overall"]])
          sim1stk <- c(sim1stk, simstk[["meanclim"]])
          sim2astk <- c(sim2astk, simstk[["soil1"]])
          sim2bstk <- c(sim2bstk, simstk[["soil2"]])
          rm(simstk)
        }
        
        #maximum values  - see original code for more information
        if(nlayers(stack(allstk)) > 1){
          #overall
          maxsim <- max(stack(allstk), na.rm=TRUE) %>%
                    raster::writeRaster(., paste(pr_odir,"/max_similarity_",etype,".tif",sep=""), overwrite=T)
          #climate
          maxsim1 <- max(stack(sim1stk), na.rm=TRUE) %>%
                    raster::writeRaster(., paste(pr_odir,"/max_similarity_clim_",etype,".tif",sep=""), overwrite=T)
          #soil 1
          maxsim2a <- max(stack(sim2astk), na.rm=TRUE) %>%
                    raster::writeRaster(., paste(pr_odir,"/max_similarity_soil1_",etype,".tif",sep=""), overwrite=T)
          #soil 2
          maxsim2b <- max(stack(sim2bstk), na.rm=TRUE) %>%
                    raster::writeRaster(., paste(pr_odir,"/max_similarity_soil2_",etype,".tif",sep=""), overwrite=T)
           
          #combine into a single maximum similarity value
          gamma <- 0.5
          fsim <- maxsim1^(1-gamma) * (mean(stack(maxsim2a, maxsim2b), na.rm=TRUE))^(gamma)
          raster::writeRaster(fsim, paste(pr_odir,"/max_similarity_final_",etype,".tif",sep=""), overwrite=T) 
          
          #values at points for threshold
          in_data <- in_data %>%
                 dplyr::mutate(score = raster::extract(fsim, .[,c("Lon", "Lat")]))
          thresh <- min(c(0.7, min(in_data$score, na.rm=TRUE)))
          
          #make layer binary (option 1)
          fsim_bin <- fsim
          fsim_bin[which(fsim[] < thresh)] <- 0
          fsim_bin[which(fsim[] >= thresh)] <- 1
          raster::writeRaster(fsim_bin, paste(pr_odir,"/max_similarity_final_binary_",etype,".tif",sep=""), overwrite=T)
           
          #make layer binary (option 2)
          fsim_bin2 <- fsim
          fsim_bin2[which(fsim[] < 0.5)] <- 0
          fsim_bin2[which(fsim[] >= 0.5)] <- 1
          raster::writeRaster(fsim_bin2, paste(pr_odir,"/max_similarity_final_binary_0.5_",etype,".tif",sep=""), overwrite=T)
        }else{
          #overall, old approach
          maxsim <- allstk[[1]] %>%
                    raster::writeRaster(., paste(pr_odir,"/max_similarity_",etype,".tif",sep=""), overwrite=T)
           
          #climate
          maxsim1 <- sim1stk[[1]] %>%
                    raster::writeRaster(., paste(pr_odir,"/max_similarity_clim_",etype,".tif",sep=""), overwrite=T)
          #soil 1
          maxsim2a <- sim2astk[[1]] %>%
                    raster::writeRaster(., paste(pr_odir,"/max_similarity_soil1_",etype,".tif",sep=""), overwrite=T)
          #soil 2
          maxsim2b <- sim2bstk[[1]] %>%
                    raster::writeRaster(., paste(pr_odir,"/max_similarity_soil2_",etype,".tif",sep=""), overwrite=T)
           
          #combine into a single maximum similarity value
          gamma <- 0.5
          fsim <- maxsim1^(1-gamma) * (mean(stack(maxsim2a, maxsim2b), na.rm=TRUE))^(gamma)
          raster::writeRaster(fsim, paste(pr_odir,"/max_similarity_final_",etype,".tif",sep=""), overwrite=T) 
          
          #values at points for threshold
          in_data <- in_data %>%
                 dplyr::mutate(score = raster::extract(fsim, .[,c("Lon", "Lat")]))
          thresh <- min(c(0.7, min(in_data$score, na.rm=TRUE)), na.rm=TRUE)
          
          #make layer binary (option 1)
          fsim_bin <- fsim
          fsim_bin[which(fsim[] < thresh)] <- 0
          fsim_bin[which(fsim[] >= thresh)] <- 1
          raster::writeRaster(fsim_bin, paste(pr_odir,"/max_similarity_final_binary_",etype,".tif",sep=""), overwrite=T)
           
          #make layer binary (option 2)
          fsim_bin2 <- fsim
          fsim_bin2[which(fsim[] < 0.5)] <- 0
          fsim_bin2[which(fsim[] >= 0.5)] <- 1
          raster::writeRaster(fsim_bin2, paste(pr_odir,"/max_similarity_final_binary_0.5_",etype,".tif",sep=""), overwrite=T)
        }
        
        #verbose what i just did
        cat("completed practice=",Practice," ",i,"of",nrow(pr_df),"/n=",nrow(in_data),"\n")
        
        #return object
        return(maxsim)
    }