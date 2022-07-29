# Set working directory ####
# Go up a level if we are in the the "/R" directory
if(substr(getwd(),nchar(getwd())-1,nchar(getwd()))=="/R"){
    setwd(substr(getwd(),1,nchar(getwd())-2))
}

# Packages & functions ####
require(terra)
require(data.table)
require(parallel)
source("R/2.1_Analogues_Functions.R")

# Set number ofcores for parallel processing
Cores<-parallel::detectCores(logical=T)-1

# Analysis version
Version <- 6

# Create folder for interim analysis files
IntDir<-"/home/jovyan/common_data/atlas/interim/"
cimdir_vr<-paste0(IntDir,"analogues_v",Version)
if(!dir.exists(cimdir_vr)){
    dir.create(cimdir_vr,recursive=T)
}

# Options

# Min number of sites per practice
MinSites<-1
# Max number of practices in combination to consider
MaxPracs<-1 

# Load analyzed ERA data from script 1 ####
data_sites<-data.table::fread(paste0(cimdir_vr,"/analogues_ERA.csv"))

# Subset data according to option values
Y<-data_sites[PrName!="",c("N.Sites","N.Countries","N.AEZ16"):=list(length(unique(Site.ID)),length(unique(Country)),length(unique(AEZ16simple))),
        by=c("PrName","Product.Simple","Out.SubInd")
        ][N.Sites >= MinSites
         ][(PrName == "Mulch-Reduced Tillage" | NPracs<=MaxPracs)]    


X<-unique(Y[,list(PrName,Product.Simple,Out.SubInd,N.Sites,N.Countries)])

# Generate combinations of scenarios, timescales, yield thresholds ####
Scenarios<-c("ssp126","ssp245","ssp370","ssp585")
Years<-c(2030,2050)
Thresholds<-c("all","m50","m25","m10",0,10,25,50) # Percentage increase in yield
Vars<-expand.grid(Years=Years,Scenarios=Scenarios,Thresholds=Thresholds)
Vars$Scenarios<-as.character(Vars$Scenarios)
Vars<-rbind(Vars,expand.grid(Years=NA,Scenarios="baseline",Thresholds=Thresholds))

# Repeat scen x time x threshold combinations for each product x practice x outcome from ERA and combine two datasets
Combinations<-data.table(Vars[rep(1:nrow(Vars),each=nrow(X)),],X[rep(1:nrow(X),nrow(Vars))]) 
                                                                 
if(F){
    lapply(Combinations[,which(!N.Sites>=5)],FUN=function(i){
    combine_analogues(Index=i,
                      Data=data_sites,
                      Combinations=Combinations,
                      SaveDir=paste0(cimdir_vr,"/results"),
                      overwrite=F,
                      cimdir=cimdir_vr,
                      gamma=0.5,
                      SoilDir=paste0(cimdir_vr,"/all"))
    })
    }

# Split processing between practices x crops with less and more data. Inorganic fertilizer requires stacking 327 rasters and uses up a LOT of ram.

# Practices x crops with less than 100 sites
CombosA<-Combinations[!N.Sites>=100][409:418]

# Update to future::apply at some point
if(F){
    library(future.apply)
    plan(multisession, workers = Cores)

 future.apply::future_lapply(Combinations[,which(!N.Sites>=100)],
                    combine_analogues, 
                    Data=data_sites,
                    Combinations=Combinations, 
                    SaveDir=paste0(cimdir_vr,"/results"), 
                    overwrite=F,
                    cimdir=cimdir_vr,
                    gamma=0.5,
                    SoilDir=paste0(cimdir_vr,"/all"),
                    future.seed=T
                    ) 
}

 parallel::mclapply(Combinations[,which(!N.Sites>=100)],
                    combine_analogues, 
                    Data=data_sites,
                    Combinations=Combinations, 
                    SaveDir=paste0(cimdir_vr,"/results"), 
                    overwrite=F,
                    cimdir=cimdir_vr,
                    gamma=0.5,
                    SoilDir=paste0(cimdir_vr,"/all"),  
                    mc.cores = Cores*2, 
                    mc.preschedule = FALSE)

 # Practices x crops with 100+ sites
 parallel::mclapply(1:nrow(Combinations[N.Sites>=100]),
                    combine_analogues, 
                    Data=data_sites,
                    Combinations=Combinations[Combinations[N.Sites>=100]], 
                    SaveDir=paste0(cimdir_vr,"/results"), 
                    overwrite=F,
                    cimdir=cimdir_vr,
                    gamma=0.5,
                    SoilDir=paste0(cimdir_vr,"/all"),  
                    mc.cores = 5, 
                    mc.preschedule = FALSE)
        
