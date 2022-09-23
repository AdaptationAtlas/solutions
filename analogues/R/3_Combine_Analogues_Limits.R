# Set working directory ####
# Go up a level if we are in the the "/R" directory
if(substr(getwd(),nchar(getwd())-1,nchar(getwd()))=="/R"){
    setwd(substr(getwd(),1,nchar(getwd())-2))
}

# Packages & functions ####
require(terra)
require(data.table)
require(future.apply)
source("R/2.1_Analogues_Functions.R")

# Set number ofcores for parallel processing
Cores<-parallel::detectCores(logical=T)-1

# Analysis version
Version <- 6

# Location of interim analysis files
cimdir_vr<-paste0("/home/jovyan/common_data/atlas_analogues/intermediate/v",Version)

# Options

# Min number of sites per practice
MinSites<-1
# Max number of practices in combination to consider
MaxPracs<-1 
# Max spatial uncertainty (m)
MaxSpatError<-20000

# Load analyzed ERA data from script 1 ####
data_sites<-data.table::fread(paste0(cimdir_vr,"/analogues_ERA_subset.csv"))

# Subset data according to option values
Y<-data_sites[PrName!="",c("N.Sites","N.Countries","N.AEZ16"):=list(length(unique(Site.ID)),length(unique(Country)),length(unique(AEZ16simple))),
        by=c("PrName","Product.Simple","Out.SubInd")
        ][N.Sites >= MinSites & Buffer<=MaxSpatError
         ][(PrName == "Mulch-Reduced Tillage" | NPracs<=MaxPracs)]    

X<-unique(Y[,list(PrName,Product.Simple,Out.SubInd,N.Sites,N.Countries)])

# Generate combinations of scenarios, timescales, yield thresholds ####
Scenarios<-c("ssp126","ssp245","ssp370","ssp585")
Years<-c(2030,2050)
Vars<-expand.grid(Years=Years,Scenarios=Scenarios)
Vars$Scenarios<-as.character(Vars$Scenarios)
Vars<-rbind(Vars,expand.grid(Years=NA,Scenarios="baseline"))

# Similarity cut-off threshold when averaging RR/PC within a cell (RR/PC values that correspond to similarities of <minsim are not used to calculate the weighted mean)
minsim<-0.5

# Repeat scen x time x threshold combinations for each product x practice x outcome from ERA and combine two datasets
Combinations<-data.table(Vars[rep(1:nrow(Vars),each=nrow(X)),],X[rep(1:nrow(X),nrow(Vars))]) 

# lapply version of function for debugging                                                                 
if(F){
   # lapply(1:nrow(Combinations),FUN=function(i){
    lapply(Combinations[,which(N.Sites>=100)],FUN=function(i){
        print(i)
        X<-combine_analogues_limits(Index=i,
                          Data=data_sites,
                          Combinations=Combinations,
                          SaveDir=paste0(cimdir_vr,"/results_mean"),
                          overwrite=F,
                          cimdir=cimdir_vr,
                          minsim=minsim,
                          doRR=T,                                    
                          SoilDir=paste0(cimdir_vr,"/all"))
        i
    })
    }

# Split processing between practices x crops with less and more data. Inorganic fertilizer requires stacking 327 rasters and uses up a LOT of ram.
# Note whilst this was necessary with mcapply, it may not be with future apply.

 plan(multisession, workers = Cores)

 future.apply::future_lapply(Combinations[,which(!N.Sites>=100)],
                    combine_analogues_limits, 
                    Data=as.data.frame(data_sites),
                    Combinations=Combinations, 
                    SaveDir=paste0(cimdir_vr,"/results_mean"), 
                    overwrite=F,
                    cimdir=cimdir_vr,
                    minsim=minsim,
                    doRR=T,   
                    SoilDir=paste0(cimdir_vr,"/all"),
                    future.packages="terra",
                    future.seed=T
                    ) 

 # Practices x crops with 100+ sites
 plan(multisession, workers = 3)

 future.apply::future_lapply(Combinations[,which(N.Sites>=100)],
                    combine_analogues_limits, 
                    Data=data_sites,
                    Combinations=Combinations, 
                    SaveDir=paste0(cimdir_vr,"/results_mean"), 
                    overwrite=F,
                    cimdir=cimdir_vr,
                    minsim=minsim,
                    doRR=T, 
                    SoilDir=paste0(cimdir_vr,"/all"),
                    future.packages=c("data.table","terra"),
                    future.seed=T
                    ) 

# unlink(paste0("/home/jovyan/common_data/atlas_analogues/intermediate/v6/results",recursive=T))