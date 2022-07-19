#JRV - ERA analyses for adaptation atlas, check output and rerun

#load libraries
require(data.table)
require(Hmisc)
require(tidyverse)
require(metR)
require(miceadds)
require(rgdal)
require(raster)
require(maptools); data(wrld_simpl)

#analysis version
vr <- 5

#set directories
wd <- "~/work/ONECGIAR/Atlas_MVP/adaptation_options"
cimdir <- paste(wd,"/ERA_analogues",sep="")

#create Scenarios x Years x Tresholds Loop ####
Scenarios <- c("rcp4.5", "rcp8.5")
Years <- c(2030, 2050)
Thresholds <- c(0.0, 0.15, 0.27, 0.41)
Vars <- expand.grid(Years=Years, Scenarios=Scenarios, Threshold=Thresholds)
Vars$Scenarios <- as.character(Vars$Scenarios)
Vars <- rbind(Vars,expand.grid(Years=NA, Scenarios="baseline", Threshold=Thresholds))

#read in ERA data =====
all_data <- read.csv(paste(cimdir,"/ERA_Test_Data.csv",sep=""))
all_data <- droplevels(all_data)

#aggregate data up to site level =====
rnd <- 4
agg_by <- c("Site.ID","PrName","Out.SubInd","Product.Simple","Product.Type")

#determine outliers =====
outlier_calc <- function(x){
  return((x < quantile(x)[2] - 3 *  IQR(x)  | x > quantile(x)[4] + 3 * IQR(x)))
}

all_data <- as.data.table(all_data)
outliers <- unlist(all_data[,R:=1:nrow(all_data)][,list(outliers=list(R[outlier_calc(yi)])), by=agg_by][,outliers])

#Calculate Weighted mean effect size by practice and site combination  =====
wt_grp <- unique(c("Code",agg_by))
data_sites <- all_data[!outliers
                       ][,N.Obs.Study:=.N,by=wt_grp # Recalculate Weightings by study within obervations grouping 
                         ][,Weight.Study:=(Rep^2/(2*Rep))/N.Obs.Study # Recalculate Weightings
                           ][,list(Observations=.N,
                                   Studies=length(unique(Code)),
                                   Country=unique(Country),
                                   AEZ16=unique(AEZ16simple),
                                   Lat=mean(Latitude),
                                   Lon=mean(Longitude),
                                   RR=round(weighted.mean(yi,Weight.Study,na.rm=T),rnd),
                                   RR.sd=suppressWarnings(round(abs(wtd.var(yi,Weight.Study,na.rm=T))^0.5,rnd))),
                             by=agg_by  # this 
                             ][,pc:=round(100*exp(RR)-100,rnd-1)
                               ][,pc.sd:=round(100*exp(RR.sd)-100,rnd-1)]
data_sites$Label <- paste(data_sites$Site.ID, data_sites$PrName)
data_sites <- data_sites[!(is.na(Lat) | is.na(Lon))]

#back to data.frame
data_sites[, PrName:=as.character(PrName)]
data_sites <- as.data.frame(data_sites)
data_sites$Npracs <- unlist(lapply(strsplit(data_sites$PrName,"-"),length))

#ERA product to exclude
ExcludeProducts <- c("Fodder Legume","Fodder Tree","Okra","Olive","Other Bean","Pepper","Pumpkin","Tomato","Watermelon","African Yam Bean","Amaranth",
                     "Amaranth Grain","Apple","Cabbage","Capsicum","Carrot & Parsnip","Chickpea","Chili","Cucumber","Date","Eggplant","Fava Bean","Firewood",
                     "Fodder Tree","Fonio","Garlic","Grape","Grapefruit & Pomelo","Jatropha","Jujube","Lablab","Melon","Napier Grass","Onion","Other Leafy Green",
                     "Other Spice","Other Veg","Peas","Spinach","Turnip","Zucchini")
MinSites <- 1 #Min # sites to run analysis
MaxPracs <- 1 # Max number of practices to consider

#verify that output exists
for(k in 1:nrow(Vars)) {
    #get run details
    #k <- 1
    Scenario <- Vars$Scenarios[k]
    Year <- Vars$Years[k]
    Variable <- Vars$Vars[k]
    Threshold <- Vars$Threshold[k]
    print(paste0("Running: Scenario = ",Vars$Scenarios[k]," | Year = ",Vars$Years[k]," | Threshold = ",Vars$Threshold[k]))
    
    #Subset ERA Data ====
    Y<-data.table(data_sites)
    
    #mulch-reduced tillage interaction
    M.RT<-Y[Product.Type=="Plant Product" &  Out.SubInd == "Crop Yield" & RR>=Threshold &!PrName=="" & !Product.Simple=="" & PrName == "Mulch-Reduced Tillage" & !Product.Simple %in% ExcludeProducts,
            list(N.Sites=length(unique(Site.ID)),N.Countries=length(unique(Country)),N.AEZ16=length(unique(AEZ16))),
            by=c("PrName","Product.Simple","Out.SubInd")][N.Sites >= MinSites]
    
    #individual practices
    Y<-Y[Product.Type=="Plant Product" &  Out.SubInd == "Crop Yield" & RR>=Threshold &!PrName=="" & !Product.Simple=="" & Npracs<=MaxPracs & !Product.Simple %in% ExcludeProducts,
         list(N.Sites=length(unique(Site.ID)),N.Countries=length(unique(Country)),N.AEZ16=length(unique(AEZ16))),
         by=c("PrName","Product.Simple","Out.SubInd")
         ][N.Sites >= MinSites]
    
    #combine both tables
    Y<-rbind(Y,M.RT)
    Y[,Product.Simple:=as.character(Product.Simple)]
    
    #loop practices
    for (i in 1:nrow(Y)) {
        #i <- 1
        #practice and product names
        prname<-Y[i,PrName]
        Product<-Y[i,Product.Simple]
        cat("...removing RData for practice=", prname, "/ crop=", Product, "\n")
        
        #folder and file
        pr_odir <- paste(cimdir,"/T",Threshold,"_v",vr,"/",Product,"/",Year,"/",gsub("[.]","_",Scenario),"/",gsub(" ", "_", prname, fixed=T),"_v",vr,sep="")
        
        #set working directory
        setwd(pr_odir)
        
        #list of files
        rm_flist <- list.files(, pattern="\\.RData")
        if (length(rm_flist) != 0) {x1 <- lapply(rm_flist, FUN=function(fl) {unlink(fl)})}
        
        #load final output
        rsc <- raster("max_similarity_final_binary_pos.tif")
        if (i == 1) {rstot <- rsc} else {rstot <- rstot + rsc}
        
        setwd(wd)
    }
    rstot <- writeRaster(rstot, paste(cimdir,"/allprac_k",k,"_v",vr,".tif",sep=""), overwrite=TRUE)
}

#tar.bz2 all new output
setwd(cimdir)
if (!file.exists("T0_v5_light.tar.bz2")) system("tar -cjvf T0_v5_light.tar.bz2 T0_v5")
if (!file.exists("T0.15_v5_light.tar.bz2")) system("tar -cjvf T0.15_v5_light.tar.bz2 T0.15_v5")
if (!file.exists("T0.27_v5_light.tar.bz2")) system("tar -cjvf T0.27_v5_light.tar.bz2 T0.27_v5")
if (!file.exists("T0.41_v5_light.tar.bz2")) system("tar -cjvf T0.41_v5_light.tar.bz2 T0.41_v5")

