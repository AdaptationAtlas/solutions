require(data.table)
require(terra)
require(spatstat.geom)
require(diagis)
require(Hmisc)
require(lmerTest)
require(stats)

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

# Set working directory ####
# Go up a level if we are in the the "/R" directory
if(substr(getwd(),nchar(getwd())-1,nchar(getwd()))=="/R"){
    setwd(substr(getwd(),1,nchar(getwd())-2))
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

# Get historic worldclim climate data ####
WCDir<-paste0(RawDir,"worldclim/")
if(!dir.exists(WCDir)){
    dir.create(WCDir)
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

# Process historic worldclim data ####
WCDirInt<-paste0(IntDir,"worldclim/")
if(!dir.exists(WCDirInt)){
    dir.create(WCDirInt)
}

wc_data<-lapply(Variables,FUN=function(VAR){
        
  File<-paste0(WCDirInt,"wc2.1_",Resolution,"_",VAR,"_masked.tif")
  if (!file.exists(File)) {
      # Display progress
      cat('\r                                                           ')
      cat('\r',paste0("Processing: ",VAR))
      flush.console()
    
      unzip(paste0(WCDir,"wc2.1_",Resolution,"_",VAR,".zip"),exdir=WCDir)
    
      Files <- paste0(WCDir,unzip(paste0(WCDir,"wc2.1_",Resolution,"_",VAR,".zip"),list=T,exdir=WCDir)$Name)
      wc_data <- terra::rast(Files[!grepl("readme",Files)])     
      wc_data <- terra::mask(terra::crop(wc_data, sh_africa),sh_africa)
                
      terra::writeRaster(wc_data,File)
         
      unlink(Files)
      wc_data
      
  }else{
     # Display progress
      cat('\r                                                           ')
      cat('\r',paste0("Loading: ",VAR))
      flush.console()

      terra::rast(File)
      }
})

names(wc_data)<-Variables

# Create a mask
msk<-paste0(IntDir,"0_boundaries/msk.tif")
if(!file.exists(msk)){
    msk<-wc_data$prec[[1]]
    msk[!is.na(msk)]<-0
    terra::writeRaster(msk,)
}else{
    msk<-terra::rast(msk)
}

# Get worldclim CMIP6 ####
WC_CMIPDir<-paste0(RawDir,"worldclim_CIMP6/")
if(!dir.exists(WC_CMIPDir)){
    dir.create(WC_CMIPDir)
}

GetWorldClimGCMs<-function(Variable,GCMs,Scenarios,Periods,Resolutions,SaveDir){
     if(!dir.exists(SaveDir)){
      dir.create(SaveDir,recursive=T)
    }
          
    for(VAR in Variable){
        for(SCENARIO in Scenarios){
            for(GCM in GCMs){ 
                for(PERIOD in Periods){
                    for(RESOLUTION in Resolutions){
                        URL<-paste0("https://geodata.ucdavis.edu/cmip6/",RESOLUTION,"/",GCM,"/",SCENARIO,"/wc2.1_",RESOLUTION,"_",VAR,"_",GCM,"_",SCENARIO,"_",PERIOD,".tif")
                        destfile<-paste0(SaveDir,"wc2.1_",RESOLUTION,"_",VAR,"_",GCM,"_",SCENARIO,"_",PERIOD,".tif")
                        # Display progress
                        cat('\r                                                           ')
                        cat('\r',paste0("Downloading file: ",VAR,"-",SCENARIO,"-",GCM))
                        flush.console()

                        # Some model x scenario combinations are missing
                        if(!(GCM == "GFDL-ESM4" & SCENARIO %in% c("ssp245","ssp585")) & 
                           !(GCM == "FIO-ESM-2-0" & SCENARIO == "ssp370") &
                           !(GCM == "HadGEM3-GC31-LL" & SCENARIO == "ssp370")){
                            if(!file.exists(destfile)){
                                download.file(URL, destfile)
                                }
                        }
                    } 
                }
            }
        }
    }
}

#GCMs<-c("ACCESS-CM2","ACCESS-ESM1-5","BCC-CSM2-MR","CanESM5","CanESM5-CanOE","CMCC-ESM2","CNRM-CM6-1","CNRM-CM6-1-HR","CNRM-ESM2-1","EC-Earth3-Veg","EC-Earth3-Veg-LR","FIO-ESM-2-0","GFDL-ESM4","GISS-E2-1-G","GISS-E2-1-H","HadGEM3-GC31-LL","INM-CM4-8","INM-CM5-0","IPSL-CM6A-LR","MIROC-ES2L","MIROC6","MPI-ESM1-2-HR","MPI-ESM1-2-LR","MRI-ESM2-0","UKESM1-0-LL")
GCMs<-c("ACCESS-ESM1-5","EC-Earth3-Veg","INM-CM5-0","MPI-ESM1-2-HR","MRI-ESM2-0")
Scenarios<-c("ssp126","ssp245","ssp370","ssp585")
Variables<-c("tmin","tmax","prec")
Period<-c("2021-2040","2041-2060")
Resolution<-"2.5m"

options(timeout=480)
GetWorldClimGCMs(Variable=Variables,Scenarios=Scenarios,GCMs=GCMs,Periods=Period,Resolutions=Resolution,SaveDir=WC_CMIPDir)

# Process worldclim CMIP6 data ####
Var_x_Scen<-expand.grid(Variables,Scenarios,Period,Resolution)

wc_future_data<-lapply(1:nrow(Var_x_Scen),FUN=function(i){
    VAR<-Var_x_Scen[i,1]
    SCENARIO<-Var_x_Scen[i,2]
    PERIOD<-Var_x_Scen[i,3]
    RESOLUTION<-Var_x_Scen[i,4]

    File<-paste0(WCDirInt,"wc2.1_",RESOLUTION,"_",VAR,"_",SCENARIO,"_",PERIOD,".tif",sep="")
    
    # Display progress
    cat('\r',paste0("Processing: ",VAR,"-",SCENARIO,"-",PERIOD,"-",RESOLUTION))
    
  if (!file.exists(File)){
     
      Files <- paste0(WC_CMIPDir,"wc2.1_",RESOLUTION,"_",VAR,"_",GCMs,"_",SCENARIO,"_",PERIOD,".tif")
      Files<-Files[file.exists(Files)]
      
      wc_data <- lapply(1:length(Files),FUN=function(i){
          FILE<-Files[i]
          cat('\r                                                           ')
          cat('\r',paste0("Crop & Mask File: ",i,"/",length(Files)))
          flush.console()
          
          suppressWarnings(terra::mask(terra::crop(terra::rast(FILE),sh_africa),sh_africa))
          })
      
      Layers<-names(wc_data[[1]])
      
      wc_data<-terra::rast(lapply(1:12,FUN=function(i){
          cat('\r                                                           ')
          cat('\r',paste0("Averaging GCMs for month: ",i))
          flush.console()
          Data<-terra::rast(lapply(wc_data,"[[",i))
          Data<-terra::mean(Data)
          Data
          }))
      
      names(wc_data)<-Layers
                
      terra::writeRaster(wc_data,File)
           
      wc_data
      
  }else{
      terra::rast(File)
      }
})

names(wc_data)<-apply(Var_x_Scen[,1:2],1,paste,collapse="-")

# Load and prepare soilgrids ####
SoilDir<-"/home/jovyan/common_data/soilgrids/raw/"
SoilIntDir<-paste0(IntDir,"soilgrids/")
if(!dir.exists(SoilIntDir)){
    dir.create(SoilIntDir)
    }
                                    
#Parameters<-c("bdod","cec","clay","sand","silt","soc","phh2o")
Parameters<-c("sand","phh2o")
Depths<-c("0-5","5-15","15-30","30-60","60-100")

# NOTE TERRA CAN SIMPLY SAVE THE ENTIRE STACK USING WRITE RASTER!
soilstk<-lapply(Parameters,FUN=function(PAR){
    
    File<-paste0(SoilIntDir,PAR,".tif")
    
    if(!file.exists(File)){
        
        cat('\r                                                           ')
        cat('\r',paste0("Processing: ",PAR))
        flush.console()
        
        Files<-paste0(SoilIntDir,PAR,"_",Depths,".tif")    
          
        stk<-terra::rast(paste0(SoilDir,PAR,"_",Depths,".vrt"))
        stk<-terra::resample(stk,msk)
        stk<-terra::mask(terra::crop(stk,msk),msk)
        
        terra::writeRaster(stk,File)
        stk
        
    }else{   
        
        cat('\r                                                           ')
        cat('\r',paste0("Loading: ",PAR))
        flush.console()
        
        terra::rast(File)        
    }
})

# Could this step be done using assign? or "%>%"
names(soilstk)<-Parameters

# ERA #####
# Load data (in future data should be publically available from the ERAg package)
load("Data/ERA_Derived.rda")
OutcomeCodes<-data.table::fread("Data/Outcomes.csv")
PracticeCodes<-data.table::fread("Data/Practices.csv")
EUCodes<-data.table::fread("Data/EU.csv")

# Source functions to prepare and analyse data (in future these should be publically available from the ERAg package)
source("R/PrepareERA.R")
source("R/ERAAnalyze.R")
source("R/OutCalc.R")

# Set analysis aggregation level to site, practice, subindicator and product.simple
agg_by <- c("Site.ID","Country","Latitude","Longitude","AEZ16simple","PrName","Out.SubInd","Product.Simple","Product.Type")

# Subset data to crop yield
ERA_Derived<-ERA_Derived[Out.SubInd=="Crop Yield"]

# Prepare ERA (see function documentation for more information)
ERAPrepared<-PrepareERA(data.table::copy(ERA_Derived),
                        DoCombinations=F,
                        CombineAll = F, 
                        PLevel = "Practice",
                        OutcomeCodes = OutcomeCodes,
                        PracticeCodes = PracticeCodes,
                        EUCodes = EUCodes)

# Analyze ERA (see function documentation for more information)
data_sites<-ERAAnalyze(Data=data.table::copy(ERAPrepared),Aggregate.By=agg_by,rmOut=T,Fast=T)

data_sites<-data_sites[,Label:=paste(Site.ID, PrName)
                      ][!(is.na(Latitude) | is.na(Longitude))
                       ][,NPracs:=stringr::str_count(PrName, "-")
                        ][!PrName=="" & !Product.Simple=="" & Product.Type!="Plant Product"
                         ][,Product.Simple:=as.character(Product.Simple)]

cimdir <- paste0(IntDir,"analogues/")

if(!dir.exists(cimdir)){
dir.create(cimdir)
    }

# Cleanup any data points outside mask
data.table::setnames(data_sites,c("Longitude","Latitude"),c("Lon","Lat"))

data_sites<-data_sites[,isvalid:=terra::extract(msk, data_sites[,c("Lon","Lat")])[,2]
                      ][!is.na(isvalid)
                       ][,isvalid:=NULL]

# Save ERA dataset
data.table::fwrite(data_sites,paste0(cimdir,"/analogues_ERA.csv"))
