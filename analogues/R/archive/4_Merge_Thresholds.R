# Set working directory ####
# Go up a level if we are in the the "/R" directory
if(substr(getwd(),nchar(getwd())-1,nchar(getwd()))=="/R"){
    setwd(substr(getwd(),1,nchar(getwd())-2))
}

# Packages & functions ####
require(terra)
require(data.table)
require(future.apply)

# Set number ofcores for parallel processing
Cores<-parallel::detectCores(logical=T)-1

# Analysis version
Version <- 6

# Root of file structure
DataDir<-"/home/jovyan/common_data"

# Location of interim analysis files
cimdir_vr<-paste0(DataDir,"/atlas_analogues/intermediate/v",Version)

# Read in admin0 for subsaharan africa
adm1_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadm41_ssa_1.shp"))
adm0_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadm41_ssa_0.shp"))

# Read in a base raster
base_raster<-terra::rast(paste0(DataDir,"/mapspam_2017/raw/spam2017V2r1_SSA_H_YAMS_S.tif"))
base_raster<-terra::crop(base_raster,adm1_africa)

# Read in waterbodies to create mask
waterbodies<-terra::vect(paste0(DataDir,"/atlas_surfacewater/raw/waterbodies_africa.shp"))
water_mask<-terra::rasterize(waterbodies,base_raster)
water_mask[!is.na(water_mask)]<-0
water_mask[is.na(water_mask)]<-1
water_mask[water_mask==0]<-NA
water_mask<-terra::mask(terra::crop(water_mask,adm1_africa),adm1_africa)

# Set the suitability threshold for the binary classification of each yield threshold layer
suit_threshold<-0.5

# Min data
minstudies<-3

# Set save location for merged files
SaveDir<-paste0(cimdir_vr,"/results_merged_",suit_threshold,"_",minstudies)
#unlink(SaveDir,recursive=T)
if(!dir.exists(SaveDir)){
    dir.create(SaveDir)
    }

# Read in mapspam data to create crop masks
MSdir<-paste0(DataDir,"/mapspam_2017/raw")
ms_meta<-data.table::fread(paste0(DataDir,"/mapspam_2017/raw/mapspam_meta.csv"))
era2ms<-data.table::fread("https://github.com/AdaptationAtlas/solutions/blob/main/analogues/Data/ERA2MapSPAM.csv?raw=T")

# Tabulate the combinations available (using the file names)
Files<-list.files(paste0(cimdir_vr,"/results"))
Files<-gsub("Mulch-Reduced","Mulch_Reduced",Files)
Files<-gsub("baseline","baseline-NA",Files)
Combos <- data.table(do.call(rbind, strsplit(Files, "-", fixed=TRUE)))
colnames(Combos)<-c("Threshold","Year","Scenario","Practice","Product","Outcome","N.Studies","Method")
Combos[,Method:=gsub(".tif","",Method)][,N.Studies:=as.numeric(N.Studies)]
Combos[,Max.N.Studies:=max(N.Studies),by=list(Year,Scenario,Practice,Product,Outcome,Method)] 
Combos$Path<-list.files(paste0(cimdir_vr,"/results"),full.names=T)  

# Cast the data so that we have thresholds as columns which contain the number of studies for each threshold
Combos2<-dcast(Combos,Practice+Product+Outcome+Max.N.Studies~Threshold,value.var="N.Studies",fun=mean)

# Subset combinations to those with a minimum amount of data
Combos_ss<-Combos2[tall>=minstudies]

# Subset to MVP practices
MVP_Pracs<-c(Agroforestry="Alleycropping",
             `Inorganic Fertilizers`="Inorganic_Fertilizer",
             `Organic Fertilizers`="Organic_Fertilizer",
             Intercropping="Intercropping",
             Mulch="Mulch",
             `Mulch Reduced Tillage` = "Mulch_Reduced_Tillage",
             `Reduced Tillage`="Reduced_Tillage",
             Irrigation="Supplemental_Irrigation",
             `Water Harvesting`="Water_Harvesting")

Combos_ss<-Combos_ss[Practice %in% MVP_Pracs]
# Subset to MapSPAM crops
Combos_ss<-Combos_ss[Product %in% era2ms[!is.na(ERA),ERA]]

# Create MapSPAM crop mask
crops<-Combos_ss[,unique(Product)]
crops<-era2ms[ERA %in% crops,MapSPAM_code]

msfiles<-list.files(paste0(DataDir,"/mapspam_2017/raw"),".tif",full.names=T)
msfiles<-grep("_A.tif",msfiles,value=T)
msfiles<-grep("_H_",msfiles,value=T)
msfiles<-grep(paste0(crops,collapse="|"),msfiles,value=T)

ms_crops_area<-terra::rast(msfiles)
ms_crops_area<-terra::mask(terra::crop(ms_crops_area,adm1_africa),adm1_africa)
ms_crops_area<-ms_crops_area/terra::cellSize(ms_crops_area,unit="ha")

Threshold<-0.00001
crop_masks<-terra::classify(ms_crops_area,cbind(c(0,Threshold),c(Threshold,9999),c(0,1)))
crops<-unlist(data.table::tstrsplit(names(crop_masks),"_",keep=4))

names(crop_masks)<-era2ms[MapSPAM_code %in% crops,ERA]
plot(crop_masks)

generic_crop_mask<-crop_masks[[1]]
generic_crop_mask[!is.na(generic_crop_mask)]<-1

# Loop through all the rows in the subset combinations for each analogues method, climate scenario and time period.
# Classify each analogues map according to the suitability threshold selected. Values are assigned to each layer according to the
# Tvals data.frame below. Layers are then stacked and the maximum cell value for the stack is taken and the resulting raster saved.
Overwrite<-T

write.csv(Combos_ss,paste0(SaveDir,"/Summary.csv"))

lapply(1:nrow(Combos_ss),FUN=function(i){

    # Set values each yield threshold will take, make sure thresholds are in ascending order
    Tvals<-data.frame(Threshold=c("tall","t0","t10","t25","t50"),
                      Val=c(0,1,2,3,4),
                      Lab=c("<0%","0-10%","10-25%","25-50%",">50%"))
    
    Prac<-Combos_ss[i,Practice]
    Out<-Combos_ss[i,Outcome]
    Prod<-Combos_ss[i,Product]    
    
    MVP_Prac<-gsub(" ","_",names(MVP_Pracs[MVP_Pracs==Prac]))
                         
    # Loop through methods, scenarios and years
    for(Meth in unique(Combos$Method)){
        for(Yr in unique(Combos$Year)){
            for(Scen in unique(Combos$Scenario[Combos$Year==Yr])){

                print(paste(c(i,"|",Yr,Scen,Prac,Prod,Out,Meth),collapse=" "))
                
                # Subset the data
                Data<-Combos[Combos$Practice==Prac & 
                             Combos$Outcome==Out & 
                             Combos$Product==Prod & 
                             Combos$Method==Meth & 
                             Combos$Scenario==Scen &
                             Combos$Year==Yr &
                             Combos$Threshold %in% Tvals$Threshold]
                
                # Create save path & file name
                Filename<-paste0(SaveDir,"/",paste(c(paste(Data$Threshold,collapse="_"),Yr,Scen,MVP_Prac,Prod,Out,Meth,paste(Data$N.Studies,collapse="_")),collapse="-"),".tif")
                           
                
                if(!file.exists(Filename)|Overwrite){
                    Thresholds<-Data$Threshold
                    Vals<-Tvals$Val[match(Data$Threshold,Tvals$Threshold)]
                                    
                    # For each yield threshold classify suitability on suit_threshold and assign value, stack the results
                    Files<-Data$Path
                    Vals<-Vals[file.exists(Files)]
                    Files<-Files[file.exists(Files)]
                    
                    Stack<-terra::rast(lapply(1:length(Files),FUN=function(j){
                        Rast<-terra::rast(Files[j])
                        Rast<-terra::resample(Rast,base_raster)
                        Rast[Rast<suit_threshold]<-NA
                        Rast[Rast>=suit_threshold]<-Vals[j]
                        Rast
                    }))
                    
                    # Take maximum value of each cell in the stack
                    Stack_mx<-max(Stack,na.rm=T)
                                                     
                    # Mask out waterbodies
                    Stack_mx<-Stack_mx*water_mask*generic_crop_mask
                    
                    # Assign levels to the raster
                    levels(Stack_mx)<-Tvals$Lab
                    
                    names(Stack_mx)<-paste(c(Yr,Scen,Prac,Prod,Out,Meth),collapse="-")
                                        
                    # Save results
                    suppressWarnings(terra::writeRaster(Stack_mx,file=Filename,overwrite=T))
                    
                    gc()
                }
            }
        }
    }
})


# Save .png maps for validation
basefiles<-list.files(SaveDir,"baseline",full.names=T)
basefiles<-basefiles[!grepl("aux",basefiles)]

plotdir<-paste0(SaveDir,"/plots")
if(!dir.exists(plotdir)){
    dir.create(plotdir)
    }

plotfun<-function(Index,basefiles,adm0_africa,lwd,plotdir,Overwrite){
    
    addfun<-function(){terra::plot(adm0_africa,add=T,lwd=lwd)}
    
    FILE<-basefiles[Index]
    X<-terra::rast(FILE)
   
    Filename<-unlist(strsplit(FILE,"/"))
    Filename<-Filename[length(Filename)]
    Filename<-gsub(".tif",".png",Filename)
    Filename<-gsub("t0_","",Filename)
    Filename<-gsub("t10_","",Filename)
    Filename<-gsub("t25_","",Filename)
    Filename<-gsub("t50_","",Filename)
    Filename<-gsub("tall-","",Filename)
    Filename<-gsub("-NA","",Filename)
    Filename<-gsub("-Crop_Yield","",Filename)
    
    print(Filename)
    
   if((!file.exists(paste0(plotdir,"/",Filename)))|Overwrite){
        png(filename = paste0(plotdir,"/",Filename),
            width = 600,
            height = 600,
            units = "px", 
            pointsize = 4,
            bg = "white", 
            res = 300)

        terra::plot(X,
                    col=viridis::viridis(n=length(unique(X[]))-1,option ="viridis",direction=-1,alpha=0.9),
                    colNA="white",
                    fun=addfun,
                    main=gsub(".png","",Filename),
                    lwd=lwd)
        dev.off()
   }
}


lapply(1:length(basefiles),
         plotfun,
         basefiles=basefiles,
         adm0_africa=adm0_africa,
         lwd=0.3,
         plotdir=plotdir,
         Overwrite=T
    ) 


# Does not appear to work in parallel, saves blank maps
if(F){
# Set number ofcores for parallel processing
Cores<-parallel::detectCores(logical=T)-1

 plan(multisession, workers = 10)

 future.apply::future_lapply(1:length(basefiles),
                             plotfun, 
                             basefiles=basefiles,
                             adm0_africa=adm0_africa,
                             lwd=0.3,
                             plotdir=plotdir,
                             future.packages="terra",
                             future.seed=T
                            ) 
    }

plan(multisession, gc = TRUE)
