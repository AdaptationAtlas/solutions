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

# Set save location for merged files
SaveDir<-paste0(cimdir_vr,"/results_merged")
#unlink(SaveDir,recursive=T)
if(!dir.exists(SaveDir)){
    dir.create(SaveDir)
    }

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

# Set the suitability threshold for the binary classification of each yield threshold layer
suit_threshold<-0.5

# Subset combinations to those with a minimum amount of data (in this case t50 needs 5 or more studies)
Combos_ss<-Combos2[t50>=5]

# Loop through all the rows in the subset combinations for each analogues method, climate scenario and time period.
# Classify each analogues map according to the suitability threshold selected. Values are assigned to each layer according to the
# Tvals data.frame below. Layers are then stacked and the maximum cell value for the stack is taken and the resulting raster saved.
Combos_ss<-Combos_ss
Overwrite<-T

lapply(1:nrow(Combos_ss),FUN=function(i){

    # Set values each yield threshold will take, make sure thresholds are in ascending order
    Tvals<-data.frame(Threshold=c("tall","t0","t10","t25","t50"),
                      Val=c(0,1,2,3,4),
                      Lab=c("<0%",">0%",">10%",">25%",">50%"))
    
    Prac<-Combos_ss[i,Practice]
    Out<-Combos_ss[i,Outcome]
    Prod<-Combos_ss[i,Product]                
                       
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
                Filename<-paste0(SaveDir,"/",paste(c(paste(Data$Threshold,collapse="_"),Yr,Scen,Prac,Prod,Out,Meth,paste(Data$N.Studies,collapse="_")),collapse="-"),".tif")
            
                if(!file.exists(Filename)|Overwrite){
                    Thresholds<-Data$Threshold
                    Vals<-Tvals$Val[match(Data$Threshold,Tvals$Threshold)]
                                    
                    # For each yield threshold classify suitability on suit_threshold and assign value, stack the results
                    Stack<-terra::rast(lapply(1:nrow(Data),FUN=function(i){
                        Rast<-terra::rast(Data$Path[i])
                        Rast[Rast<suit_threshold]<-NA
                        Rast[Rast>=suit_threshold]<-Vals[i]
                        Rast
                    }))
                    
                    # Take maximum value of each cell in the stack
                    Stack_mx<-max(Stack,na.rm=T)
                    # Assign levels to the raster
                    levels(Stack_mx)<-Tvals$Lab
                    # Save results
                    suppressWarnings(terra::writeRaster(Stack_mx,file=Filename,overwrite=T))
                }
            }
        }
    }
})

X<-list.files(SaveDir,full.names=T)
X<-X[!grepl("aux",X)][1]
X<-terra::rast(X)
terra::plot(X,col=c("grey",sample(viridis::viridis(n=length(unique(X[]))-1,option ="viridis"),replace=F)))
