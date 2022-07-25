# Packages
require(data.table)
require(parallel)
require(stringr)
require(terra)

# Analysis version
Version <- 6

# Set number ofcores for parallel processing
Cores<-parallel::detectCores()-1

# Run full or streamlined analysis?
DoLite<-T

# Set working directory ####
# Go up a level if we are in the the "/R" directory
if(substr(getwd(),nchar(getwd())-1,nchar(getwd()))=="/R"){
    setwd(substr(getwd(),1,nchar(getwd())-2))
}

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
data_sites<-data_sites[(PrName == "Mulch-Reduced Tillage" | NPracs<=MaxPracs) & Product.Simple %in% IncludeProducts]
                           
# Worldclim ####
# Set parameters to include in analysis
Scenarios<-c("ssp126","ssp245","ssp370","ssp585")
Variables<-c("tmin","tmax","prec")
Period<-c("2021-2040","2041-2060")
Resolution<-"2.5m"

# Worldclim historic ####
WCDirInt<-paste0(IntDir,"worldclim/")

wc_data<-lapply(Variables,FUN=function(VAR){
      File<-paste0(WCDirInt,"wc2.1_",Resolution,"_",VAR,"_masked.tif") 
      terra::rast(File)
      })

names(wc_data)<-Variables

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
Scenarios<-c("ssp126","ssp245","ssp370","ssp585")
Years<-c(2030,2050)
Thresholds<-c(0.0,0.15,0.27,0.41)
Vars<-expand.grid(Years=Years,Scenarios=Scenarios,Threshold=Thresholds)
Vars$Scenarios<-as.character(Vars$Scenarios)
Vars<-rbind(Vars,expand.grid(Years=NA,Scenarios="baseline",Threshold=Thresholds))

#options
DoNeg<-F # Produce negative suitability?
DoLite<-T # To save time average the soil rasters across depths (rather than using multiple depths) and cut out min class and quantiles from run_points function
                       
for(k in 1:nrow(Vars)){
    Scenario<-Vars$Scenarios[k]
    Year<-Vars$Years[k]
    if(Year==2030){Period<-"2021-2040"}
    if(Year==2050){Period<-"2041-2060"}
    Threshold<-Vars$Threshold[k]
    
    print(paste0("Running: Scenario = ",Vars$Scenarios[k]," | Year = ",Vars$Years[k]," | Threshold = ",Vars$Threshold[k]))
    
    #load monthly climate data -----
    #climate: load baseline ####
    wc_prec <- wc_data$prec
    lwc_tmin <- wc_data$tmin
    lwc_tmax <- wc_data$tmax
    
    #climate: load future ####
    
    if (Scenario != 'baseline') {
       wc_prec_fut<-wc_future_data[[paste0("prec-",Scenario,"-",Period)]]
       wc_tmin_fut<-wc_future_data[[paste0("tmin-",Scenario,"-",Period)]]
       wc_tmax_fut<-wc_future_data[[paste0("tmax-",Scenario,"-",Period)]]
       }    
    
    ############################################################
    ############################################################
    #select practice and organize practice data -----
    
    #calculate similarity (both temp and precip for positive sites) - Function -----
    #adjust so that for each site
    #1. similarity for mean climate
    #2. similarity for each soil variable
    #3. combine all in a sensible layer and save individual layers and output as .RData
    
    #Subset Era Data ====
    
    Y<-data_sites[RR>=Threshold &!PrName=="",
            list(N.Sites=length(unique(Site.ID)),
                 N.Countries=length(unique(Country)),
                 N.AEZ16=length(unique(AEZ16simple))),
            by=c("PrName","Product.Simple","Out.SubInd")
            ][N.Sites >= MinSites]    #practice list with only two fields
    
    X<-Y[,c("PrName","Product.Simple")]
     
        
    #run in parallel if 1 practice or more
    if(nrow(X)>0){
        xres <- parallel::mclapply(1:nrow(X), 
                                   run_points, 
                                   pr_df = X, 
                                   ERA_data = data_sites, 
                                   cimdir = cimdir, 
                                   Threshold =  Threshold, 
                                   Year =  Year, 
                                   Scenario =  Scenario, 
                                   vr = Version, 
                                   etype = "pos", 
                                   DoLite = DoLite,
                                   mc.cores = Cores, 
                                   mc.preschedule = FALSE,
                                   Outcome = "Crop Yield")
    }

    #clean-up
    rm(X,Y)
    gc()
}