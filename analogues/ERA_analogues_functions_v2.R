#load data
load_ERA_data <- function(cimdir, DoLite, Year, Scenario) {
    #load soil data
    #load Soils Data -----
    if (DoLite) {
        load(paste(cimdir,"/input_data/soils2.rda",sep=""), envir=.GlobalEnv)
    } else {
        load(paste(cimdir,"/input_data/soils.rda",sep=""), envir=.GlobalEnv)
    }
    
    #load monthly climate data -----
    #climate: load baseline ####
    load(paste(cimdir,"/input_data/wc_prec.rda",sep=""), envir=.GlobalEnv); wc_prec <<- stack(wc_prec)
    load(paste(cimdir,"/input_data/wc_tmean.rda",sep=""), envir=.GlobalEnv); wc_tmean <<- stack(wc_tmean)
    
    #climate: load future ####
    #precipitation
    if (!is.na(Year)) {
        wc_prec_fut <<- load.Rdata2(filename=paste0("prec_",gsub("[.]","_",Scenario),"_",Year,"_","Resamp.rda"), path=paste(cimdir,"/input_data",sep=""))
    }
    
    #temperature
    if (!is.na(Year)) {
        wc_tmean_fut <<- load.Rdata2(filename=paste0("tmean_",gsub("[.]","_",Scenario),"_",Year,"_","Resamp.rda"), path=paste(cimdir,"/input_data",sep=""))
    }
}

# Points <=10 - Run practices in parallel (gets clogged up when some cores have a practice with large numbers of sites) =====
run_points <- function(pr_i, pr_df, data_sites, cimdir, Threshold, Year, Scenario, vr, etype='pos', DoLite=FALSE) {
    #get practice and product name
    prname<-pr_df[pr_i,PrName]
    Product<-pr_df[pr_i,Product.Simple]
    
    #Restructure data and classifiy points-----
    pdata <- data_sites[which(data_sites$PrName == prname & data_sites$Product.Simple == Product & data_sites$Out.SubInd == "Crop Yield"),]
    pdata <- pdata[which(!is.na(pdata$Lat)),]
    pdata <- pdata[which(!is.na(pdata$Lon)),]
    pdata <- droplevels(pdata)
    pdata <- pdata[,c("Lon","Lat","RR")]
    
    #cleanup any data points outside African continent
    pdata$isvalid <- raster::extract(msk, pdata[,c("Lon","Lat")])
    pdata <- pdata[which(!is.na(pdata$isvalid)),]
    pdata$isvalid <- NULL
    
    #positive yield impact  =====
    in_data <- pdata[which(pdata$RR >= Threshold),]
    in_data$ID <- paste("s",1:nrow(in_data),sep="")
    
    #output directory  =====
    pr_odir <- paste(cimdir,"/T",Threshold,"_",vr,"/",Product,"/",Year,"/",gsub("[.]","_",Scenario),"/",gsub(" ", "_", prname, fixed=T),"_v",vr,sep="")
    if (!file.exists(pr_odir)) {dir.create(pr_odir,recursive=T)}
    
    #verbose what i'm running
    cat("processing practice=", prname, pr_i, "of", nrow(pr_df), "/ crop=", Product, "/ n=", nrow(in_data),"\n")
    
    #loop through points for practice
    allstk <- sim1stk <- sim2astk <- sim2bstk <- c()
    for (i in 1:nrow(in_data)) {
      #cat("processing site=",i,"\n")
      if (!file.exists(paste(pr_odir,"/out_",etype,"_",in_data$ID[i],".RData",sep=""))) {
        #1. mean climate ####
        #cat("...mean climate\n")
        par1 <- createParameters(x=in_data$Lon[i], y=in_data$Lat[i], vars=c("prec","tmean"),weights=c(0.75,0.25),
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
            parx <- createParameters(x=in_data$Lon[i], y=in_data$Lat[i], vars=c(svar),weights=1,
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
             file=paste(pr_odir,"/out_",etype,"_",in_data$ID[i],".RData",sep=""),
             compress="xz",compression_level=9)

        #clean-up
        gc(full=TRUE, verbose=FALSE)
      } else {
        #cat("...already processed, hence loading\n")
        load(paste(pr_odir,"/out_",etype,"_",in_data$ID[i],".RData",sep=""))
      }
      allstk <- c(allstk, simstk[["overall"]])
      sim1stk <- c(sim1stk, simstk[["meanclim"]])
      sim2astk <- c(sim2astk, simstk[["soil1"]])
      sim2bstk <- c(sim2bstk, simstk[["soil2"]])
      rm(simstk)
    }
    
    #maximum values  - see original code for more information
    if(nlayers(stack(allstk))>1){
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
      #overall
      maxsim <- allstk[[1]] %>%
                raster::writeRaster(., paste(pr_odir,"/max_similarity_",etype,".tif",sep=""),
                                    overwrite=T)
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
    cat("completed practice=",prname," ",pr_i,"of",nrow(pr_df),"/n=",nrow(in_data),"\n")
    
    #return object
    return(maxsim)
}

