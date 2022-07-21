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

# Process worldclim data
WCDirInt<-paste0(IntDir,"worldclim/")
if(!dir.exists(WCDirInt)){
    dir.create(WCDirInt)
}

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

# Get worldclim CMIP6 ####
WC_CMIPDir<-paste0(RawDir,"worldclim/CIMP6/")
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
                        URL<-paste0("https://geodata.ucdavis.edu/cmip6/",Resolution,"/",GCM,"/",SCENARIO,"/wc2.1_",RESOLUTION,"_",VAR,"_",GCM,"_",SCENARIO,"_",PERIOD,".tif")
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

GCMs<-c("ACCESS-CM2","ACCESS-ESM1-5","BCC-CSM2-MR","CanESM5","CanESM5-CanOE","CMCC-ESM2","CNRM-CM6-1","CNRM-CM6-1-HR","CNRM-ESM2-1","EC-Earth3-Veg","EC-Earth3-Veg-LR","FIO-ESM-2-0","GFDL-ESM4","GISS-E2-1-G","GISS-E2-1-H","HadGEM3-GC31-LL","INM-CM4-8","INM-CM5-0","IPSL-CM6A-LR","MIROC-ES2L","MIROC6","MPI-ESM1-2-HR","MPI-ESM1-2-LR","MRI-ESM2-0","UKESM1-0-LL")
Scenarios<-c("ssp126","ssp245","ssp370","ssp585")
Variables<-c("tmin","tmax","prec")

options(timeout=120)
GetWorldClimGCMs(Variable=Variables,Scenarios=Scenarios,GCMs=GCMs,Periods="2041-2060",Resolutions="2.5m",SaveDir=WC_CMIPDir)

# Process worldclim CMIP6 data
WC_CIMPDirInt<-paste0(IntDir,"worldclim/CMIP6/")
if(!dir.exists(WC_CIMPDirInt)){
    dir.create(WC_CIMPDirInt)
}

Var_x_Scen<-expand.grid(Variables,Scenarios,"2041-2060","2.5m")

wc_future_data<-lapply(1:nrow(Var_x_Scen),FUN=function(i){
    VAR<-Var_x_Scen[i,1]
    SCENARIO<-Var_x_Scen[i,2]
    PERIOD<-Var_x_Scen[i,3]
    RESOLUTION<-Var_x_Scen[i,4]

    File<-paste0(WC_CIMPDirInt,"wc2.1_",RESOLUTION,"_",VAR,"_",SCENARIO,"_",PERIOD,".tif",sep="")
    
  if (!file.exists(File)){
     
      Files <- paste0(WC_CMIPDir,"wc2.1_",RESOLUTION,"_",VAR,"_",GCMs,"_",SCENARIO,"_",PERIOD,".tif")
      Files<-Files[file.exists(Files)]
      
      wc_data <- lapply(Files,FUN=function(FILE){
           terra::mask(terra::crop(terra::rast,FILE),sh_africa),sh_africa)
          })
      
      wc_data<-lapply(1:12,FUN=function(i){
          Data<-terra::rast(lapply(wc_data,"[[",1))
          Data<-terra::mean(Data)
          Data
          })
    
      wc_data <- terra::mask(terra::crop(wc_data, sh_africa),sh_africa)
      
                
      terra::writeRaster(wc_data[LAYER],paste0(WCDirInt,LAYER,"_masked.tif"))
           
      unlink(Files)
      wc_data
      
  }else{
      
      Files<-list.files(WCDirInt,VAR,full.names=T) 
      Files<-Files[!grepl("zip",Files)]
      terra::rast(Files)
      }
})

names(wc_data)<-Variables

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