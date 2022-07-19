#JRV - ERA analyses for adaptation atlas

#load libraries & create functions ----
require(analogues)
require(rgdal)
require(raster)
require(maptools); data(wrld_simpl)
require(data.table)
require(Hmisc)
require(tidyverse)
require(ggplot2)
require(metR)

require(rworldmap)
require(doSNOW)
require(miceadds)
require(parallel)
require(foreach)

#analysis version
vr <- 5
# Set number of cores for parallel processing ====
Cores<-10
# Run full or streamlined analysis?
DoLite<-T


#set directories - MAKE SURE YOU START CONSOLE FROM ANALOGUES FOLDER-----
wd <- "~/work/ONECGIAR/Atlas_MVP/adaptation_options"
if(!dir.exists(wd)){dir.create(wd)}
cimdir <- paste(wd,"/ERA_analogues",sep="")
if(!dir.exists(cimdir)){dir.create(cimdir)}

#get Africa shapefile -----
sh_ctry <- readOGR("~/work/ONECGIAR/Data/Africa_shp/African_continet.shp")
sh_ctry <- spTransform(sh_ctry, crs("+proj=longlat +ellps=WGS84 +no_defs"))
sh_xt <- extent(sh_ctry)
sh_xt@xmin <- sh_xt@xmin-1; sh_xt@ymin <- sh_xt@ymin-1; sh_xt@xmax <- sh_xt@xmax+1; sh_xt@ymax <- sh_xt@ymax+1

#load Soils Data -----
if(DoLite){
  load(paste(cimdir,"/input_data/soils2.rda",sep=""))
}else{
  load(paste(cimdir,"/input_data/soils.rda",sep=""))
}

# Create a Mask- ---
load(paste(cimdir,"/input_data/wc_prec.rda",sep=""))
msk <- wc_prec[[1]]; rm(wc_prec)
msk[which(!is.na(msk[]))] <- 1

#read in ERA data =====
all_data <- read.csv(paste(cimdir,"/ERA_Test_Data.csv",sep=""))
all_data <- droplevels(all_data)

#aggregate data up to site level =====
rnd <- 4
agg_by <- c("Site.ID","PrName","Out.SubInd","Product.Simple","Product.Type")

#determine outliers =====
outlier_calc <- function(x){
  return((x < quantile(x)[2] - 3 *  IQR(x)  | x > quantile(x)[4] + 3 * IQR(x)))
}

all_data <- as.data.table(all_data)
outliers <- unlist(all_data[,R:=1:nrow(all_data)][,list(outliers=list(R[outlier_calc(yi)])), by=agg_by][,outliers])

#Calculate Weighted mean effect size by practice and site combination  =====
wt_grp <- unique(c("Code",agg_by))
data_sites <- all_data[!outliers
                       ][,N.Obs.Study:=.N,by=wt_grp # Recalculate Weightings by study within obervations grouping 
                         ][,Weight.Study:=(Rep^2/(2*Rep))/N.Obs.Study # Recalculate Weightings
                           ][,list(Observations=.N,
                                   Studies=length(unique(Code)),
                                   Country=unique(Country),
                                   AEZ16=unique(AEZ16simple),
                                   Lat=mean(Latitude),
                                   Lon=mean(Longitude),
                                   RR=round(weighted.mean(yi,Weight.Study,na.rm=T),rnd),
                                   RR.sd=suppressWarnings(round(abs(wtd.var(yi,Weight.Study,na.rm=T))^0.5,rnd))),
                             by=agg_by  # this 
                             ][,pc:=round(100*exp(RR)-100,rnd-1)
                               ][,pc.sd:=round(100*exp(RR.sd)-100,rnd-1)]
data_sites$Label <- paste(data_sites$Site.ID, data_sites$PrName)
data_sites<-data_sites[!(is.na(Lat) | is.na(Lon))]

#back to data.frame
data_sites[,PrName:=as.character(PrName)]
data_sites <- as.data.frame(data_sites)
data_sites$Npracs<-unlist(lapply(strsplit(data_sites$PrName,"-"),length))


#Create Scenarios x Years x Tresholds Loop ####
Scenarios<-c("rcp4.5","rcp8.5")
Years<-c(2030,2050)
Thresholds<-c(0.0,0.15,0.27,0.41)
Vars<-expand.grid(Years=Years,Scenarios=Scenarios,Threshold=Thresholds)
Vars$Scenarios<-as.character(Vars$Scenarios)
Vars<-rbind(Vars,expand.grid(Years=NA,Scenarios="baseline",Threshold=Thresholds))

#options
DoNeg<-F # Produce negative suitability?
DoLite<-T # To save time average the soil rasters across depths (rather than using multiple depths) and cut out min class and quantiles from run_points function

ExcludeProducts<-c("Fodder Legume","Fodder Tree","Okra","Olive","Other Bean","Pepper","Pumpkin","Tomato","Watermelon","African Yam Bean","Amaranth",
                   "Amaranth Grain","Apple","Cabbage","Capsicum","Carrot & Parsnip","Chickpea","Chili","Cucumber","Date","Eggplant","Fava Bean","Firewood",
                   "Fodder Tree","Fonio","Garlic","Grape","Grapefruit & Pomelo","Jatropha","Jujube","Lablab","Melon","Napier Grass","Onion","Other Leafy Green",
                   "Other Spice","Other Veg","Peas","Spinach","Turnip","Zucchini")
MinSites<-1
MaxPracs<-1 # Max number of practices to consider


for(k in 1:nrow(Vars)){
    Scenario<-Vars$Scenarios[k]
    Year<-Vars$Years[k]
    Variable<-Vars$Vars[k]
    Threshold<-Vars$Threshold[k]
    print(paste0("Running: Scenario = ",Vars$Scenarios[k]," | Year = ",Vars$Years[k]," | Threshold = ",Vars$Threshold[k]))
    
    #load monthly climate data -----
    #climate: load baseline ####
    load(paste(cimdir,"/input_data/wc_prec.rda",sep="")); wc_prec <- stack(wc_prec)
    load(paste(cimdir,"/input_data/wc_tmean.rda",sep="")); wc_tmean <- stack(wc_tmean)
    
    #climate: load future ####
    File<-paste0(cimdir,"/input_data/prec_",gsub("[.]","_",Scenario),"_",Year,"_","Resamp.rda")
    if (Scenario != 'baseline') {
       if(!file.exists(File)){
           wc_prec_fut<-load.Rdata2(filename=paste0("prec_",gsub("[.]","_",Scenario),"_",Year,"_","2.5min.RData"),path=cimdir) 
           wc_prec_fut<-resample (wc_prec_fut,msk, method = 'bilinear')
           wc_prec_fut<-round(wc_prec_fut,0)
           wc_prec_fut<-stack(wc_prec_fut)
           if(!inMemory(wc_prec_fut)){wc_prec_fut<-readAll(wc_prec_fut)}
           save(wc_prec_fut,file=File)
       } else {
           wc_prec_fut<-load.Rdata2(filename=paste0("prec_",gsub("[.]","_",Scenario),"_",Year,"_","Resamp.rda"),path=paste0(cimdir,"/input_data"))
       }
    }
    
    File<-paste0(cimdir,"/input_data/tmean_",gsub("[.]","_",Scenario),"_",Year,"_","Resamp.rda")
    if (Scenario != 'baseline') {
       if(!file.exists(File)){
           wc_tmean_fut<-load.Rdata2(filename=paste0("tmean_",gsub("[.]","_",Scenario),"_",Year,"_","2.5min.RData"),path=cimdir) 
           wc_tmean_fut<-resample (wc_tmean_fut,msk, method = 'bilinear')
           wc_tmean_fut<-round(wc_tmean_fut)
           wc_tmean_fut<-stack(wc_tmean_fut)
           if(!inMemory(wc_tmean_fut)){wc_tmean_fut<-readAll(wc_tmean_fut)}
           save(wc_tmean_fut,file=File)
       } else {
           wc_tmean_fut<-load.Rdata2(filename=paste0("tmean_",gsub("[.]","_",Scenario),"_",Year,"_","Resamp.rda"),path=paste0(cimdir,"/input_data")) 
       }
    }
    
    ############################################################
    ############################################################
    #select practice and organize practice data -----
    
    #calculate similarity (both temp and precip for positive sites) - Function -----
    #adjust so that for each site
    #1. similarity for mean climate
    #2. similarity for each soil variable
    #3. combine all in a sensible layer and save individual layers and output as .RData
    
    #Subset Era Data ====
    Y<-data.table(data_sites)
    
    #mulch-reduced tillage interaction
    M.RT<-Y[Product.Type=="Plant Product" &  
              Out.SubInd == "Crop Yield" & 
              RR>=Threshold &!PrName=="" & 
              Product.Simple!="" & 
              PrName == "Mulch-Reduced Tillage" & 
              !Product.Simple %in% ExcludeProducts,
            list(N.Sites=length(unique(Site.ID)),
                 N.Countries=length(unique(Country)),
                 N.AEZ16=length(unique(AEZ16))),
            by=c("PrName","Product.Simple","Out.SubInd")
            ][N.Sites >= MinSites]
    
    #individual practices
    Y<-Y[Product.Type=="Plant Product" &  Out.SubInd == "Crop Yield" & RR>=Threshold &!PrName=="" & !Product.Simple=="" & Npracs<=MaxPracs & !Product.Simple %in% ExcludeProducts,
         list(N.Sites=length(unique(Site.ID)),N.Countries=length(unique(Country)),N.AEZ16=length(unique(AEZ16))),
         by=c("PrName","Product.Simple","Out.SubInd")
         ][N.Sites >= MinSites]
    
    #combine both tables
    Y<-rbind(Y,M.RT)
    Y[,Product.Simple:=as.character(Product.Simple)]
    
    # Points <=10 - Run practices in parallel (gets clogged up when some cores have a practice with large numbers of sites) =====
    run_points <- function(i, pr_df, data_sites, cimdir, Threshold, Year, Scenario, vr, etype='pos', DoLite=F) {
        #get practice and product name
        prname<-pr_df[i,PrName]
        Product<-pr_df[i,Product.Simple]
        
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
        pr_odir <- paste(cimdir,"/T",Threshold,"_v",vr,"/",Product,"/",Year,"/",gsub("[.]","_",Scenario),"/",gsub(" ", "_", prname, fixed=T),"_v",vr,sep="")
        if (!file.exists(pr_odir)) {dir.create(pr_odir,recursive=T)}
        
        #verbose what i'm running
        cat("processing practice=", prname, i, "of", nrow(pr_df), "/ crop=", Product, "/ n=", nrow(in_data),"\n")
        
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
        cat("completed practice=",prname," ",i,"of",nrow(pr_df),"/n=",nrow(in_data),"\n")
        
        #return object
        return(maxsim)
    }
    
    #practice list with only two fields
    X<-Y[,c("PrName","Product.Simple")]
    
    #run in parallel if 1 practice or more
    if(nrow(X)>0){
        xres <- parallel::mclapply(1:nrow(X), run_points, pr_df = X, data_sites, cimdir, 
                                   Threshold, Year, Scenario, vr, etype = "pos", DoLite,
                                   mc.cores = Cores, mc.preschedule = FALSE)
    }

    #clean-up
    rm(X,Y)
    gc()
}

#xrs <- run_points(i=17, pr_df = X, data_sites = data_sites, cimdir, Threshold, Year, Scenario, vr, etype='pos', DoLite=F)