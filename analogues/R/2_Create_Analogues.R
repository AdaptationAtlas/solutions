# Set working directory ####
# Go up a level if we are in the the "/R" directory
if(substr(getwd(),nchar(getwd())-1,nchar(getwd()))=="/R"){
    setwd(substr(getwd(),1,nchar(getwd())-2))
}


# Packages & functions ####
require(data.table)
require(parallel)
require(stringr)
require(terra)
if(!require(analogues)){
devtools::install_github("CIAT-DAPA/analogues")    
}

# Functions
source("R/2.1_Analogues_Functions.R")

# Parameters ####
# Analysis version
Version <- 6

# Set number ofcores for parallel processing
Cores<-parallel::detectCores()-1

# Run full or streamlined analysis?
DoLite<-T

# Set save location of intermediate datasets
IntDir<-"/home/jovyan/common_data/atlas/interim/"

# Create analysis save folder
cimdir <- paste0(IntDir,"analogues/")
if(!dir.exists(paste0(cimdir,"v",Version)){
    dir.create(paste0(cimdir,"v",Version),recursive=T)
}
# Africa map ####
BoundIntDir<-paste0(IntDir,"0_boundaries/")
sh_africa<-terra::vect(paste0(BoundIntDir,"gadml0_4326_agg.shp"))

# Mask ####
msk<-terra::rast(paste0(IntDir,"0_boundaries/msk.tif"))

# ERA ####
data_sites<-data.table::fread(paste0(cimdir,"analogues_ERA.csv"))

# Products tp include
# Link to MapSPAM names?

IncludeProducts<-c("Sorghum","Groundnut","Pearl Millet","Maize","Cotton","Wheat","Cowpea","Tomato","Cassava","Pigeon Pea","Rice","Soybean","Chickpea","Sunflower","Arabica","Peas","Common Bean","Sweet Potato","Banana","Robusta","Rape","Yam","Potato","Tea","Barley","Fava Bean","Canola")

# Subset ERA data
   # Consider moving all subsetting to combine analogues script
data_sites[grepl("Banana",Product.Simple),Product.Simple:="Banana"]             
data_sites<-data_sites[(PrName == "Mulch-Reduced Tillage" | NPracs<=MaxPracs) & Product.Simple %in% IncludeProducts
                      ][,ID:=paste0("s",Lat,"_",Lon)] 

data.table::fwrite(data_sites,paste0(cimdir,"v",Version,"/analogues_ERA.csv"))

pdata<-unique(data.table(data_sites)[Out.SubInd == "Crop Yield" & !(is.na(Lat) & is.na(Lon)),c("Lon","Lat","ID")])

# Worldclim ####
# Set parameters to include in analysis
Scenarios<-c("ssp126","ssp245","ssp370","ssp585")
Variables<-c("tmin","tmax","prec")
Period<-c("2021-2040","2041-2060")
Resolution<-"2.5m"

# Worldclim baseline ####
WCDirInt<-paste0(IntDir,"worldclim/")

wc_data<-lapply(Variables,FUN=function(VAR){
      File<-paste0(WCDirInt,"wc2.1_",Resolution,"_",VAR,"_masked.tif") 
      terra::rast(File)
      })

names(wc_data)<-Variables

wc_prec <- raster::stack(wc_data$prec)
wc_tmin <- wc_data$tmin
wc_tmax <- wc_data$tmax
wc_tmean<-raster::stack((wc_tmin+wc_tmax)/2)

rm(wc_tmin,wc_tmax)

# Worldclim future####
Var_x_Scen<-expand.grid(Variables,Scenarios,Period,Resolution)

wc_future_data<-lapply(1:nrow(Var_x_Scen),FUN=function(i){
    VAR<-Var_x_Scen[i,1]
    SCENARIO<-Var_x_Scen[i,2]
    PERIOD<-Var_x_Scen[i,3]
    RESOLUTION<-Var_x_Scen[i,4]

    File<-paste0(WCDirInt,"wc2.1_",RESOLUTION,"_",VAR,"_",SCENARIO,"_",PERIOD,".tif",sep="")   
    terra::rast(File)
})

names(wc_future_data)<-apply(Var_x_Scen[,1:3],1,paste,collapse="-")

# Soilgrids ####
SoilIntDir<-paste0(IntDir,"soilgrids/")

SoilPars<-c("bdod","cec","clay","sand","silt","soc","phh2o")
#Parameters<-c("sand","phh2o")
Depths<-c("0-5","5-15","15-30","30-60","60-100")

soilstk<-lapply(SoilPars,FUN=function(PAR){
    File<-paste0(SoilIntDir,PAR,".tif")    
    terra::rast(File)
})

names(soilstk)<-SoilPars                     

# Calculate climate analogues ####

# Set scenarios and timescales
Scenarios<-c("ssp126","ssp245","ssp370","ssp585")
Years<-c(2030,2050)
Vars<-expand.grid(Years=Years,Scenarios=Scenarios)
Vars$Scenarios<-as.character(Vars$Scenarios)
Vars<-rbind(Vars,expand.grid(Years=NA,Scenarios="baseline"))
data.table::fwrite(Vars,paste0(cimdir,"v",Version,"/scenarios_x_years.csv"))

for(k in 1:nrow(Vars)){
    Scenario<-Vars$Scenarios[k]
    Year<-Vars$Years[k]
    
    cat(paste0("Running: Scenario = ",Vars$Scenarios[k]," | Year = ",Vars$Years[k]))
    
    if(!is.na(Year)){
        if(Year==2030){Period<-"2021-2040"}
        if(Year==2050){Period<-"2041-2060"}
    }
    
    #load future climate data -----

    if (Scenario != 'baseline') {
       wc_prec_fut<-raster::stack(wc_future_data[[paste0("prec-",Scenario,"-",Period)]])
       wc_tmin_fut<-wc_future_data[[paste0("tmin-",Scenario,"-",Period)]]
       wc_tmax_fut<-wc_future_data[[paste0("tmax-",Scenario,"-",Period)]]
       wc_tmean_fut<-raster::stack((wc_tmin_fut+wc_tmax_fut)/2)
        
       rm(wc_tmin_fut,wc_tmax_fut)
    }else{
        wc_prec_fut<-wc_prec
        wc_tmean_fut<-wc_tmean
    }
    
    #output directory  =====
    if(Scenario=="baseline"){
        SaveDir <- paste0(cimdir,"v",Version,"/baseline")
    }else{
        SaveDir <- paste0(cimdir,"v",Version,"/",Year,"/",gsub("[.]","_",Scenario))
      }  
    
    if(!dir.exists(SaveDir)){
        dir.create(SaveDir,recursive=T)
        }

   # For each point calculate climate analogue-----
    
     parallel::mclapply(1:nrow(pdata),
                        run_points_climate, 
                        Data=pdata, 
                        SaveDir=SaveDir, 
                        wc_prec=wc_prec,
                        wc_tmean=wc_tmean,
                        wc_prec_fut=wc_prec_fut,
                        wc_tmean_fut=wc_tmean_fut,
                        Verbose=F,
                        mc.cores = Cores, 
                        mc.preschedule = FALSE)
         
    #clean-up
    gc()
 
}

# Calculate soil analagues ####
SaveDir <- paste0(cimdir,"v",Version)
if(!dir.exists(SaveDir)){
    dir.create(SaveDir,recursive=T)
}

parallel::mclapply(1:nrow(pdata),
                   run_points_soil,
                   Data=pdata,
                   SaveDir,     
                   soilstk,
                   Verbose=F,
                   DoAll=T,
                   mc.cores = Cores, 
                   mc.preschedule = FALSE)