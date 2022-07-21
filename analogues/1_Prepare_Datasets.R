# Set save location of intermediate datasets
IntDir<-"/home/jovyan/common_data/atlas/interim/"
if(!dir.exists(IntDir)){
    dir.create(IntDir,recursive=T)
    }

# Set save location of raw datasets
RawDir<-"/home/jovyan/common_data/atlas/raw/"
if(!dir.exists(IntDir)){
    dir.create(IntDir,recursive=T)
    }

# Load africa map ####
BoundIntDir<-paste0(IntDir,"0_boundaries/")
if(!dir.exists(BoundIntDir)){
    dir.create(BoundIntDir)
    }

# Dissolve borders
if(!file.exists(paste0(BoundIntDir,"gadml0_4326_agg.shp"))){
    sh_ctry<-terra::vect("/home/jovyan/common_data/atlas/raw/0_boundaries/gadml0_4326.shp")
    sh_ctry <- terra::project(sh_ctry, "+proj=longlat +ellps=WGS84 +no_defs")
    sh_africa<-terra::aggregate(sh_ctry)
    terra::writeVector(sh_africa,paste0(BoundIntDir,"gadml0_4326_agg.shp"))
}else{
    sh_africa<-terra::vect(paste0(BoundIntDir,"gadml0_4326_agg.shp"))
}

# Get worldclim climate data ####
WCDir<-paste0(RawDir,"worldclim/")
if(!dir.exists(WCDir)){
    dir.create(WCDir)
}

WCDirInt<-paste0(IntDir,"worldclim/")
if(!dir.exists(WCDirInt)){
    dir.create(WCDirInt)
}
      
# Resolution = c("10m","5m","2.5m","30s")
# Variable = c("tmin","tmax","tavg","prec","srad","wind","vapr")
GetWorldClim<-function(Variable,Resolution,SaveDir){
     if(!dir.exists(SaveDir)){
      dir.create(SaveDir,recursive=T)
    }
    
    for(VAR in Variable){
        URL<-paste0("https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_",Resolution,"_",VAR,".zip")
        destfile<-paste0(SaveDir,"wc2.1_",Resolution,"_",VAR,".zip")
        # Display progress
        cat('\r                                                ')
        cat('\r',paste0("Downloading file: ",VAR))
        flush.console()
        
        if(!file.exists(destfile)){
            download.file(URL, destfile)
        } 
    }
}

Resolution<-"2.5m"
Variables<-c("tmin","tmax","tavg","prec")

GetWorldClim(Variable=Variables,Resolution=Resolution,SaveDir=WCDir)
      
wc_data<-lapply(Variables,FUN=function(VAR){
  
  if (!file.exists(paste0(WCDirInt,"wc2.1_",Resolution,"_",VAR,"01_masked.tif",sep=""))) {
    
      unzip(paste0(WCDir,"wc2.1_",Resolution,"_",VAR,".zip"),exdir=WCDir)
    
      Files <- paste0(WCDir,unzip(paste0(WCDir,"wc2.1_",Resolution,"_",VAR,".zip"),list=T,exdir=WCDir)$Name)
      wc_data <- terra::rast(Files[!grepl("readme",Files)])     
      wc_data <- terra::mask(terra::crop(wc_data, sh_africa),sh_africa)
                
      lapply(names(wc_data),FUN=function(LAYER){
        terra::writeRaster(wc_data[LAYER],paste0(WCDirInt,LAYER,"_masked.tif"))
      })            
      unlink(Files)
      wc_data
      
  }else{
      
      Files<-list.files(WCDirInt,VAR,full.names=T) 
      Files<-Files[!grepl("zip",Files)]
      terra::rast(Files)
      }
})

names(wc_data)<-Variables

# Create a mask ####
msk <-wc_data[["prec"]][[1]]
msk[which(!is.na(msk[]))] <- 1

# Load and prepare soilgrids ####
SoilDir<-"/home/jovyan/common_data/soilgrids/raw/"
SoilIntDir<-paste0(IntDir,"soilgrids/")
if(!dir.exists(SoilIntDir)){
    dir.create(SoilIntDir)
    }
                                    
#Parameters<-c("bdod","cec","clay","sand","silt","soc","phh2o")
Parameters<-c("sand","phh2o")
Depths<-c("0-5","5-15","15-30","30-60","60-100")

soilstk<-lapply(Parameters,FUN=function(PAR){
    Files<-paste0(SoilIntDir,PAR,"_",Depths,".tif")    
    if(!all(file.exists(Files))){
    stk<-terra::rast(paste0(SoilDir,PAR,"_",Depths,".vrt"))
    stk<-terra::resample(stk,msk)
    stk<-terra::mask(terra::crop(stk,msk),msk)
    
    lapply(names(stk),FUN=function(LAYER){
    terra::writeRaster(stk[LAYER],paste0(SoilIntDir,LAYER,".tif"))
    })
        
    }
    
    terra::rast(Files)
})

names(soilstk)<-Parameters