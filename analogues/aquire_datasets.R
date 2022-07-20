devtools::source_url("https://github.com/EiA2030/source_data/blob/main/R/soilgrids250_download.R?raw=TRUE")
soilgrids250_download(par = "soc", depth = "5-15", path = tempdir(), lonlat=NULL)

c("BLDFIE","CECSOL","CLYPPT","SNDPPT","SLTPPT","ORCDRC","PHIHOX","AWCh3")

Path<-"/home/jovyan/common_data/soilgrids/raw/"
Vars<-c("bdod","cec","clay","sand","silt","soc","phh20")
Depths<-c("0-5","5-15","15-30","30-60","60-100")

lapply(Vars,FUN=function(VAR){
terra::rast(paste0(Path,VAR,"_",Depths,".vrt"))
})


