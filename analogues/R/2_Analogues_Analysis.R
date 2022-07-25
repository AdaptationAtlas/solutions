# Packages
require(data.table)
require(parallel)
require(stringr)
require(terra)

# Analysis version
vr <- 6

# Set number ofcores for parallel processing
Cores<-parallel::detectCores()-1

# Run full or streamlined analysis?
DoLite<-T

# Set working directory ####
# Go up a level if we are in the the "/R" directory
if(substr(getwd(),nchar(getwd())-1,nchar(getwd()))=="/R"){
    setwd(substr(getwd(),1,nchar(getwd())-2))
}

# Set save location of intermediate datasets
IntDir<-"/home/jovyan/common_data/atlas/interim/"

# Create analysis save folder
cimdir <- paste0(IntDir,"analogues/")

# Africa map ####
BoundIntDir<-paste0(IntDir,"0_boundaries/")
sh_africa<-terra::vect(paste0(BoundIntDir,"gadml0_4326_agg.shp"))

# Mask ####
msk<-terra::rast(paste0(IntDir,"0_boundaries/msk.tif"))

# ERA ####
data_sites<-data.table::fread(paste0(cimdir,"analogues_ERA.csv"))

# Exclude products
# CHANGE THIS TO BE MAPSPAM crops to include?
ExcludeProducts<-c("Amaranth Grain","Apple","Cabbage","Capsicum","Carrot & Parsnip","Chickpea","Chili","Cucumber","Date","Eggplant","Fava Bean","Firewood",
                   "Fodder Tree","Fonio","Garlic","Grape","Grapefruit & Pomelo","Jatropha","Jujube","Lablab","Melon","Napier Grass","Onion","Other Leafy Green",
                   "Other Spice","Other Veg","Peas","Spinach","Turnip","Zucchini")

MinSites<-1
MaxPracs<-1 # Max number of practices in combination to consider

# Subset ERA data
data_sites<-data_sites[(PrName == "Mulch-Reduced Tillage" | NPracs<=MaxPracs) & !Product.Simple %in% ExcludeProducts]
                           
# Worldclim ####
# Set parameters to include in analysis
Scenarios<-c("ssp126","ssp245","ssp370","ssp585")
Variables<-c("tmin","tmax","prec")
Period<-"2041-2060"
Resolution<-"2.5m"

# Worldclim historic ####
WCDirInt<-paste0(IntDir,"worldclim/")

wc_data<-lapply(Variables,FUN=function(VAR){
      File<-paste0(WCDirInt,"wc2.1_",Resolution,"_",VAR,"_masked.tif") 
      terra::rast(File)
      })

names(wc_data)<-Variables

# Worldclim future####
Var_x_Scen<-expand.grid(Variables,Scenarios,Period,Resolution)

wc_future_data<-lapply(1:nrow(Var_x_Scen),FUN=function(i){
    VAR<-Var_x_Scen[i,1]
    SCENARIO<-Var_x_Scen[i,2]
    PERIOD<-Var_x_Scen[i,3]
    RESOLUTION<-Var_x_Scen[i,4]

    File<-paste0(WCDirInt,"wc2.1_",RESOLUTION,"_",VAR,"_",SCENARIO,"_",PERIOD,".tif",sep="")   
    terra::rast(File)
})

names(wc_future_data)<-apply(Var_x_Scen[,1:2],1,paste,collapse="-")

# Soilgrids ####
SoilIntDir<-paste0(IntDir,"soilgrids/")

#Parameters<-c("bdod","cec","clay","sand","silt","soc","phh2o")
Parameters<-c("sand","phh2o")
Depths<-c("0-5","5-15","15-30","30-60","60-100")

soilstk<-lapply(Parameters,FUN=function(PAR){
    File<-paste0(SoilIntDir,PAR,".tif")    
    terra::rast(File)
})

names(soilstk)<-Parameters                     

#Create Scenarios x Years x Tresholds Loop ####
Scenarios<-c("ssp126","ssp245","ssp370","ssp585")
Years<-c(2030,2050)
Thresholds<-c(0.0,0.15,0.27,0.41)
Vars<-expand.grid(Years=Years,Scenarios=Scenarios,Threshold=Thresholds)
Vars$Scenarios<-as.character(Vars$Scenarios)
Vars<-rbind(Vars,expand.grid(Years=NA,Scenarios="baseline",Threshold=Thresholds))

#options
DoNeg<-F # Produce negative suitability?
DoLite<-T # To save time average the soil rasters across depths (rather than using multiple depths) and cut out min class and quantiles from run_points function
                       
for(k in 1:nrow(Vars)){
    Scenario<-Vars$Scenarios[k]
    Year<-Vars$Years[k]
    Threshold<-Vars$Threshold[k]
    
    print(paste0("Running: Scenario = ",Vars$Scenarios[k]," | Year = ",Vars$Years[k]," | Threshold = ",Vars$Threshold[k]))
    
    #load monthly climate data -----
    #climate: load baseline ####
    wc_prec <- wc_data$prec
    lwc_tmin <- wc_data$tmin
    lwc_tmax <- wc_data$tmax
    
    #climate: load future ####
    
    if (Scenario != 'baseline') {
       wc_prec_fut<-wc_future_data[[paste0("prec-",Scenario)]]
       wc_tmin_fut<-wc_future_data[[paste0("tmin-",Scenario)]]
       wc_tmax_fut<-wc_future_data[[paste0("tmax-",Scenario)]]
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
    
    Y<-data_sites[RR>=Threshold &!PrName=="",
            list(N.Sites=length(unique(Site.ID)),
                 N.Countries=length(unique(Country)),
                 N.AEZ16=length(unique(AEZ16))),
            by=c("PrName","Product.Simple","Out.SubInd")
            ][N.Sites >= MinSites]
  
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