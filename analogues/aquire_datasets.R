# Set save location of intermediate datasets
IntDir<-"/home/jovyan/common_data/atlas/interim/"
if(!dir.exists(IntDir)){
    dir.create(IntDir,recursive=T)
    }

# Load africa map ####
sh_ctry<-terra::vect("/home/jovyan/common_data/atlas/raw/0_boundaries/gadml0_4326.shp")
sh_ctry <- terra::project(sh_ctry, "+proj=longlat +ellps=WGS84 +no_defs")
sh_africa<-terra::aggregate(sh_ctry)

# Load and prepare soilgrids ####
SoilDir<-"/home/jovyan/common_data/soilgrids/raw/"
#Parameters<-c("bdod","cec","clay","sand","silt","soc","phh2o")
Parameters<-c("sand","phh2o")
Depths<-c("0-5","5-15","15-30","30-60","60-100")

soilstk<-lapply(Parameters,FUN=function(PAR){
terra::rast(paste0(SoilDir,PAR,"_",Depths,".vrt"))
})

stk<-terra::rast(paste0(SoilDir,PAR,"_",Depths,".vrt"))
stk<-terra::mask(terra::crop(stk,sh_africa),sh_africa)
terra::plot(stk)

soilstk<-lapply(Parameters,FUN=function(PAR){
    Files<-paste0(IntDir,"soilgrids/",PAR,"_",Depths,".tif")    
    if(!all(file.exists(Files))){
    stk<-terra::rast(paste0(Path,PAR,"_",Depths,".vrt"))
    stk<-terra::mask(terra::crop(stk,sh_africa),sh_africa)
    
    lapply(names(stk),FUN=function(LAYER){
    terra::writeRaster(stk[LAYER],paste0(IntDir,"soilgrids/",LAYER,".tif"))
    })
        
    }
    
    terra::rast(Files)
})

names(soilstk)<-Parameters


