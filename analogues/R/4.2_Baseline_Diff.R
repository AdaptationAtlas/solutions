# Set working directory ####
# Go up a level if we are in the the "/R" directory
if(substr(getwd(),nchar(getwd())-1,nchar(getwd()))=="/R"){
  setwd(substr(getwd(),1,nchar(getwd())-2))
}

# Packages & functions ####
require(terra)
require(data.table)

# Set number ofcores for parallel processing
Cores<-parallel::detectCores(logical=T)-1

# Analysis version
Version <- 6

# Root of file structure
DataDir<-"/home/jovyan/common_data"

# Location of interim analysis files
cimdir_vr<-paste0(DataDir,"/atlas_analogues/intermediate/v",Version)

# Location of weighted mean files
results_dir<-paste0(cimdir_vr,"/results_mean")

# Save directory for classified files
save_dir<-paste0(cimdir_vr,"/results_mean_difference")
if(!dir.exists(save_dir)){
  dir.create(save_dir,recursive=T)
}

# Read in admin0 for subsaharan africa
adm1_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadm41_ssa_1.shp"))
adm0_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadm41_ssa_0.shp"))

# Read in a base raster
base_raster<-terra::rast(paste0(DataDir,"/mapspam_2017/raw/spam2017V2r1_SSA_H_YAMS_S.tif"))
base_raster<-terra::crop(base_raster,adm1_africa)

# Read in waterbodies to create mask ####
waterbodies<-terra::vect(paste0(DataDir,"/atlas_surfacewater/raw/waterbodies_africa.shp"))
water_mask<-terra::rasterize(waterbodies,base_raster)
water_mask[!is.na(water_mask)]<-0
water_mask[is.na(water_mask)]<-1
water_mask[water_mask==0]<-NA
water_mask<-terra::mask(terra::crop(water_mask,adm1_africa),adm1_africa)

# Read in mapspam data to create crop masks ####
msfiles<-list.files(paste0(DataDir,"/mapspam_2017/raw"),"_A_",full.names=T)
msfiles<-grep("_A.tif",msfiles,value=T)
ms_crops_area<-terra::rast(msfiles)
ms_crops_area<-terra::mask(terra::crop(ms_crops_area,adm1_africa),adm1_africa)
ms_crops_area<-sum(ms_crops_area)
ms_crops_area<-100*ms_crops_area/terra::cellSize(ms_crops_area,unit="ha")

# Create MapSPAM crop mask
Threshold<-0.01
generic_crop_mask<-terra::classify(ms_crops_area,cbind(c(0,Threshold),c(Threshold,9999),c(0,1)))


# Set values each yield threshold will take, make sure thresholds are in ascending order
Tvals<-data.frame(Val=c(0,1,2,3,4,-9999),
                  Lab=c("<0%","0-10%","10-25%","25-50%",">50%","NoData"))


Files<-list.files(results_dir,".tif")
X<-as.numeric(gsub(".tif","",unlist(lapply(strsplit(Files,"-"),tail,1))))

Files<-Files[X>=10]

BaseFiles<-Files[grepl("baseline",Files)]
SSPFiles<-Files[!grepl("baseline",Files)]


for(i in 1:length(BaseFiles)){
  
  cat('\r                                 ')
  cat("Processing ", i, "of", length(BaseFiles))
  flush.console()
  
  SSPFiles<-Files[!grepl("baseline",Files) & grepl(gsub("baseline-","",BaseFiles[1]),Files)]
  
  for(j in 1:length(SSPFiles)){
    
    if(!file.exists())

    Rast<-terra::rast(paste0(results_dir,"/",Files[i]))
    Rast<-terra::resample(Rast,base_raster)
    
    Rast<-terra::classify(Rast,as.matrix(data.frame(from=log(c(0.00001,1,1.1,1.25,1.50)),
                                                    to=log(c(1,1.1,1.25,1.5,9999)),
                                                    val=c(0,1,2,3,4))))
    
    Rast[is.na(Rast) & !is.na(generic_crop_mask)]<--9999
    
    # Assign levels to the raster
    levels(Rast)<-Tvals
    
    terra::writeRaster(Rast,paste0(save_dir,"/",Files[i]))
  
  }
}



