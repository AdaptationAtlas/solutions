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

# Location of interim analysis files
cimdir_vr<-paste0("/home/jovyan/common_data/atlas_analogues/intermediate/v",Version)

# Load the ERA data
ERA_subset<-data.table::fread(paste0(cimdir_vr,"/analogues_ERA_subset.csv"))

# Load the ERA combinations used
Combinations<-data.table::fread(paste0(cimdir_vr,"/analogues_combinations.csv"))

Combos<-Combinations[,gsub(" ","_",paste0(Years,"-",Scenarios,"-",PrName,"-",Product.Simple,"-",Out.SubInd))]
Combos<-gsub("NA-","",Combos)
Combo<-Combos[1]

Files<-list.files(paste0(cimdir_vr,"/results"),Combo)

Data <- data.table(do.call(rbind, strsplit(Files, "-", fixed=TRUE)))
colnames(Data)<-c("Threshold","Year","Scenario","Practice","Product","Outcome","N.Studies","Method")
Paths<-list.files(paste0(cimdir_vr,"/results"),Combo,full.names=T)
Data[,Method:=gsub(".tif","",Method)]
Data$Path<-Paths

Methods<-c("fsim","maxsim")
Thresholds<-list(c("tall","t0","t10","t25","t50"),c("tm10","tm25","tm50"))
RasterVals<-list(c(0,1,2,3,4),c(-1,-2,-3))
names(Thresholds)<-c("positive","negative")

Meth<-Methods[1]
Thresh<-Thresholds[[1]]
Val<-RasterVals[[1]]

Stack<-terra::rast(lapply(1:length(Thresh),FUN=function(i){
            Rast<-terra::rast(Data[Method==Meth & Threshold==Thresh[i],Path])
            Rast[Rast<0.5]<-NA
            Rast[Rast>=0.5]<-Val[i]
            Rast
            }))

Stack<-max(Stack)
