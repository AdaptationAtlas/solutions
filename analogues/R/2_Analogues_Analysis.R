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
source("R/3_Analogues_Functions.R")

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

# Africa map ####
BoundIntDir<-paste0(IntDir,"0_boundaries/")
sh_africa<-terra::vect(paste0(BoundIntDir,"gadml0_4326_agg.shp"))

# Mask ####
msk<-terra::rast(paste0(IntDir,"0_boundaries/msk.tif"))

# ERA ####
data_sites<-data.table::fread(paste0(cimdir,"analogues_ERA.csv"))

# Exclude products
# Link to MapSPAM names?

IncludeProducts<-c("Sorghum","Groundnut","Pearl Millet","Maize","Cotton","Wheat","Cowpea","Tomato","Cassava","Pigeon Pea","Rice","Soybean","Chickpea","Sunflower","Arabica","Peas","Common Bean","Sweet Potato","Banana","Robusta","Rape","Yam","Potato","Tea","Barley","Fava Bean","Canola")

MinSites<-1
MaxPracs<-1 # Max number of practices in combination to consider

# Subset ERA data
data_sites[grepl("Banana",Product.Simple),Product.Simple:="Banana"]             
data_sites<-data_sites[(PrName == "Mulch-Reduced Tillage" | NPracs<=MaxPracs) & Product.Simple %in% IncludeProducts
                      ][,ID:=paste0("s",Lat,"_",Lon)] 

pdata<-unique(data.table(data_sites)[Out.SubInd == "Crop Yield" & !(is.na(Lat) & is.na(Lon)),c("Lon","Lat","ID")])

Y<-data_sites[RR>=Threshold &!PrName=="",
        list(N.Sites=length(unique(Site.ID)),
             N.Countries=length(unique(Country)),
             N.AEZ16=length(unique(AEZ16simple))),
        by=c("PrName","Product.Simple","Out.SubInd")
        ][N.Sites >= MinSites]    #practice list with only two fields

X<-Y[,c("PrName","Product.Simple")]

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

#Parameters<-c("bdod","cec","clay","sand","silt","soc","phh2o")
Parameters<-c("sand","phh2o")
Depths<-c("0-5","5-15","15-30","30-60","60-100")

soilstk<-lapply(Parameters,FUN=function(PAR){
    File<-paste0(SoilIntDir,PAR,".tif")    
    terra::rast(File)
})

names(soilstk)<-Parameters                     

#Create Scenarios x Years x Tresholds Loop ####
Scenarios<-c("ssp126","ssp245","ssp370","ssp585","baseline")
Years<-c(2030,2050)
Vars<-expand.grid(Years=Years,Scenarios=Scenarios)
Vars$Scenarios<-as.character(Vars$Scenarios)
Vars<-rbind(Vars,expand.grid(Years=NA,Scenarios="baseline"))

Thresholds<-c(0.0,0.15,0.27,0.41)

#options
DoNeg<-F # Produce negative suitability?
DoLite<-T # To save time average the soil rasters across depths (rather than using multiple depths) and cut out min class and quantiles from run_points function         

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
       }    
    
   # For each point calculate climate analogue-----
    
     parallel::mclapply(1:nrow(pdata), 
                        run_points_climate, 
                        Data=pdata, 
                        cimdir, 
                        Year, 
                        Scenario, 
                        vr=Version,
                        wc_prec=wc_prec,
                        wc_tmean=wc_tmean,
                        wc_prec_fut=if(is.na(Year)){wc_prec}else{wc_prec_fut},
                        wc_tmean_fut=if(is.na(Year)){wc_tmean}else{wc_tmean_fut},
                        mc.cores = Cores, 
                        mc.preschedule = FALSE)
         
    #clean-up
    gc()
}


