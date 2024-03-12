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

# Create MapSPAM crop mask (%)
Threshold<-5


# Save directory for classified files
save_dir<-paste0(cimdir_vr,"/results_mean_class_crop",Threshold)
if(!dir.exists(save_dir)){
  dir.create(save_dir,recursive=T)
}

# Use a mask derived from all mapspam crops present in ERA data, or crop specific data
use_generic_mask<-F

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
ms_crops_area<-100*ms_crops_area/terra::cellSize(ms_crops_area,unit="ha")

# harmonize crop names
era2ms<-data.table::fread("https://github.com/AdaptationAtlas/solutions/blob/main/analogues/Data/ERA2MapSPAM.csv?raw=T")
names(ms_crops_area)<-gsub("spam2017V2r1_SSA_A_","",names(ms_crops_area))
names(ms_crops_area)<-gsub("_A","",names(ms_crops_area))
names(ms_crops_area)<-era2ms[match(names(ms_crops_area),MapSPAM_code),ERA]
ms_crops_area<-ms_crops_area[[names(ms_crops_area)!="NA"]]

# All crops summed
generic_crop_mask<-terra::classify(sum(ms_crops_area),cbind(c(0,Threshold),c(Threshold,9999),c(NA,1)))

ms_crops_area<-terra::classify(ms_crops_area,cbind(c(0,Threshold),c(Threshold,9999),c(NA,1)))


# Set values each yield threshold will take, make sure thresholds are in ascending order
Tvals<-data.frame(Val=c(0,1,2,3,4,-9999),
                  Lab=c("<0%","0-10%","10-25%","25-50%",">50%","NoData"))


Files<-list.files(results_dir,".tif")
X<-as.numeric(gsub(".tif","",unlist(lapply(strsplit(Files,"-"),tail,1))))

Files<-Files[X>=10]


# MVP practices and renaming conventions
MVP_Pracs<-c(Agroforestry="Alleycropping",
             `Inorganic Fertilizers`="Inorganic_Fertilizer",
             `Organic Fertilizers`="Organic_Fertilizer",
             Intercropping="Intercropping",
              Mulch="Mulch",
             `Mulch Reduced Tillage` = "Mulch_Reduced_Tillage",
             `Reduced Tillage`="Reduced_Tillage",
             Irrigation="Supplemental_Irrigation",
             `Water Harvesting`="Water_Harvesting")

# Make vector of practice names
Practices<-rep("",length(Files))
Practices[grepl("baseline",Files)]<-unlist(tstrsplit(Files[grepl("baseline",Files)],"-",keep=2))
Practices[!grepl("baseline",Files)]<-unlist(tstrsplit(Files[!grepl("baseline",Files)],"-",keep=3))

# Create data.table of file info
Files<-data.table(Files=Files,Practice=Practices)

# Subset to MVP practices
Files<-Files[Practice %in% MVP_Pracs]

# Add rename column
Files$Practice_new<-names(MVP_Pracs)[match(Files$Practice,MVP_Pracs)]


# Make vector of crop names to link to map spam area by crop
Crop<-rep("",length(Files$Files))
Crop[grepl("baseline",Files$Files)]<-unlist(tstrsplit(Files$Files[grepl("baseline",Files$Files)],"-",keep=3))
Crop[!grepl("baseline",Files$Files)]<-unlist(tstrsplit(Files$Files[!grepl("baseline",Files$Files)],"-",keep=4))
Crop[grepl("baseline-Mulch-Red",Files$Files)]<-unlist(tstrsplit(Files$Files[grepl("baseline-Mulch-Red",Files$Files)],"-",keep=4))
Crop[!grepl("baseline-Mulch-Red",Files$Files) & grepl("Mulch-Red",Files$Files)]<-unlist(tstrsplit(Files$Files[!grepl("baseline-Mulch-Red",Files$Files)  & grepl("Mulch-Red",Files$Files)],"-",keep=5))
Crop<-gsub("_"," ",Crop)

Files$Crop<-Crop

# Create new file save name
Files[,SaveName:=gsub(Practice,Practice_new,Files),by=Files]

# Overwrite?
Overwrite<-T

for(i in 1:nrow(Files)){
  
  cat('\r                                 ')
  cat("Processing ", i, "of", nrow(Files))
  flush.console()
  
  if((!file.exists(paste0(save_dir,"/",Files$SaveName[i])))|Overwrite){
    Rast<-terra::rast(paste0(results_dir,"/",Files$Files[i]))
    Rast<-terra::resample(Rast,base_raster)
    
    Rast<-terra::classify(Rast,as.matrix(data.frame(from=log(c(0.00001,1,1.1,1.25,1.50)),
                                                    to=log(c(1,1.1,1.25,1.5,9999)),
                                                    val=c(0,1,2,3,4))))
    
      if(use_generic_mask){
          Rast[is.na(Rast) & !is.na(generic_crop_mask)]<--9999
          }else{
           Rast[is.na(Rast) & !is.na(ms_crops_area[[Files$Crop[i]]])]<--9999
      }
    
    # Assign levels to the raster
    levels(Rast)<-Tvals
    
    terra::writeRaster(Rast,paste0(save_dir,"/",Files$SaveName[i]),overwrite=Overwrite)
  }
}



