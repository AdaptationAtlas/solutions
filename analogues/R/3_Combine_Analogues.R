# Set working directory ####
# Go up a level if we are in the the "/R" directory
if(substr(getwd(),nchar(getwd())-1,nchar(getwd()))=="/R"){
    setwd(substr(getwd(),1,nchar(getwd())-2))
}

# Packages & functions ####
require(terra)
require(data.table)
source("R/2.1_Analogues_Functions.R")


# Set number ofcores for parallel processing
Cores<-10

# Analysis version
Version <- 6

# Set save location of intermediate datasets
IntDir<-"/home/jovyan/common_data/atlas/interim/"

# Create analysis save folder
cimdir <- paste0(IntDir,"analogues/")


#options
DoNeg<-F # Produce negative suitability?
DoLite<-T # To save time average the soil rasters across depths (rather than using multiple depths) and cut out min class and quantiles from run_points function     

SxY<-data.table::fread(paste0(cimdir,"v",Version,"/scenarios_x_years.csv"))


MinSites<-1
MaxPracs<-1 # Max number of practices in combination to consider

data_sites<-data.table::fread(paste0(cimdir,"v",Version,"/analogues_ERA.csv"))


i<-1

Y<-data_sites[PrName!="",c("N.Sites","N.Countries","N.AEZ16"):=list(length(unique(Site.ID)),length(unique(Country)),length(unique(AEZ16simple))),
        by=c("PrName","Product.Simple","Out.SubInd")
        ][N.Sites >= MinSites]    #practice list with only two fields


X<-unique(Y[,list(PrName,Product.Simple,Out.SubInd,N.Sites,N.Countries)])

# Set scenarios and timescales
Scenarios<-c("ssp126","ssp245","ssp370","ssp585")
Years<-c(2030,2050)
Thresholds<-c("all","m50","m25","m10",0,10,25,50) # Percentage increase in yield
Vars<-expand.grid(Years=Years,Scenarios=Scenarios,Thresholds=Thresholds)
Vars$Scenarios<-as.character(Vars$Scenarios)
Vars<-rbind(Vars,expand.grid(Years=NA,Scenarios="baseline",Thresholds=Thresholds))

Combinations<-data.table(Vars[rep(1:nrow(Vars),each=nrow(X)),],X[rep(1:nrow(X),nrow(Vars))])

SoilDir<-paste0(cimdir,"v",Version,"/all")
SaveDir<-paste0(cimdir,"v",Version,"/results")
if(!dir.exists(SaveDir)){
    dir.create(SaveDir,recursive=T)
}

 parallel::mclapply(1:nrow(Combinations),
                    combine_analogues, 
                    Combinations=Combinations, 
                    SaveDir=SaveDir, 
                    overwrite=F,
                    cimdir=cimdir,
                    Version=Version,   
                    mc.cores = Cores, 
                    mc.preschedule = FALSE)
        

