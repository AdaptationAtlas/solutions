require(terra)
require(sp)
require(sf)

# Set save location of intermediate datasets
IntDir<-"/home/jovyan/common_data/atlas/intermediate/analogues/"
if(!dir.exists(IntDir)){
    dir.create(IntDir,recursive=T)
    }

# Get Africa Map
sh_ctry<-terra::vect("/home/jovyan/common_data/atlas/raw/0_boundaries/gadml0_4326.shp")
sh_ctry <- terra::project(sh_ctry, "+proj=longlat +ellps=WGS84 +no_defs")
sh_africa<-terra::aggregate(sh_ctry)

Parameters<-c("bdod","cec","clay","sand","silt","soc","phh20")
Depths<-c("0-5","5-15","15-30","30-60","60-100")

SoilDir<-("/home/jovyan/common_data/atlas/raw/soilgrids/")
dir.create(SoilDir)

# This function can be used to download soilgrids data
devtools::source_url("https://github.com/EiA2030/source_data/blob/main/R/soilgrids250_download.R?raw=TRUE")

bb<-as.vector(terra::ext(sh_africa))

for(PAR in Parameters){
  for(DEPTH in Depths){
      if(!file.exists(paste0(SoilDir,PAR,"_",DEPTH,".tif"))){
          soilgrids250_download(par = PAR, 
                          depth = DEPTH, 
                          xmin = floor(bb["xmin"]), 
                          ymin = floor(bb["ymin"]), 
                          xmax = ceiling(bb["xmax"]),
                          ymax = ceiling(bb["ymax"]),                               
                          path = paste0(SoilDir),
                          lonlat=NULL)
      
          unlink(list.files(SoilDir,"tmp_",full.names=T))
    }
  }
}

