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
if(!file.exists(paste0(BoundIntDir,"gadml0_4326_agg.shp")){
    sh_ctry<-terra::vect("/home/jovyan/common_data/atlas/raw/0_boundaries/gadml0_4326.shp")
    sh_ctry <- terra::project(sh_ctry, "+proj=longlat +ellps=WGS84 +no_defs")
    sh_africa<-terra::aggregate(sh_ctry)
    terra::writeVector(sh_africa,paste0(BoundIntDir,"gadml0_4326_agg.shp"))
}else{
    sh_africa<-terra::vect(paste0(BoundIntDir,"gadml0_4326_agg.shp"))
}

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
    stk<-terra::rast(paste0(Path,PAR,"_",Depths,".vrt"))
    stk<-terra::mask(terra::crop(stk,sh_africa),sh_africa)
    
    lapply(names(stk),FUN=function(LAYER){
    terra::writeRaster(stk[LAYER],paste0(SoilIntDir,LAYER,".tif"))
    })
        
    }
    
    terra::rast(Files)
})

names(soilstk)<-Parameters

# Get worldclim climate data ####
WCDir<-paste0(RawDir,"worldclim/")
if(!dir.exists(WCDir)){
    dir.create(WCDir)
}
   
for (tvar in c("prec","tmin","tmean","tmax")) {
  #tvar <- "prec"
  cat("downloading",tvar,"\n")
  if (!file.exists(paste(WCDir,"wc_",tvar,".rda",sep=""))) {
    wc_data <- raster::getData('worldclim', var=tvar, res=2.5, path=WCDir)
    wc_data <- terra::rast(wc_data)
    wc_data <- terra::mask(terra::crop(wc_data, sh_africa),sh_africa)
    wc_data <- raster::raster(wc_data)
    wc_data <- readAll(wc_data)
    assign(paste("wc_",tvar,sep=""), wc_data)
    rm(wc_data)
    save(list=c(paste("wc_",tvar,sep="")), file=paste(WCDir,"wc_",tvar,".rda",sep=""), compress="xz",compression_level=9)
    unlink(paste(WCDir,"wc2-5",sep=""),recursive=T,force=T)
  }
}