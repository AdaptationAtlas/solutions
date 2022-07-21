# Set save location of intermediate datasets
IntDir<-"/home/jovyan/common_data/atlas/interim/"

# Set save location of raw datasets
RawDir<-"/home/jovyan/common_data/atlas/raw/"

# Africa map ####
BoundIntDir<-paste0(IntDir,"0_boundaries/")
sh_africa<-terra::vect(paste0(BoundIntDir,"gadml0_4326_agg.shp"))

# Worldclim ####
WCDirInt<-paste0(IntDir,"worldclim/")
Resolution<-"2.5m"
Variables<-c("tmin","tmax","tavg","prec")

wc_data<-lapply(Variables,FUN=function(VAR){
      Files<-list.files(WCDirInt,VAR,full.names=T) 
      Files<-Files[!grepl("zip",Files)]
      terra::rast(Files)
      })

names(wc_data)<-Variables

# Soilgrids ####
SoilIntDir<-paste0(IntDir,"soilgrids/")

#Parameters<-c("bdod","cec","clay","sand","silt","soc","phh2o")
Parameters<-c("sand","phh2o")
Depths<-c("0-5","5-15","15-30","30-60","60-100")

soilstk<-lapply(Parameters,FUN=function(PAR){
    Files<-paste0(SoilIntDir,PAR,"_",Depths,".tif")    
    terra::rast(Files)
})

names(soilstk)<-Parameters