#JRV - ERA analyses for adaptation atlas, check output and rerun

#load libraries
require(analogues)
require(rgdal)
require(raster)
require(maptools); data(wrld_simpl)
require(data.table)
require(Hmisc)
require(tidyverse)
require(ggplot2)
require(metR)
require(rworldmap)
require(miceadds)

#source functions
source("~/work/Repositories/Gates-smallholder-adaptation/adaptation/ERA_analogues_functions_v2.R")

#analysis version
vr <- 5

# Run full or streamlined analysis?
DoLite <- T

#set directories - MAKE SURE YOU START CONSOLE FROM ANALOGUES FOLDER-----
wd <- "~/work/ONECGIAR/Atlas_MVP/adaptation_options"
cimdir <- paste(wd,"/ERA_analogues",sep="")

#get Africa shapefile -----
sh_ctry <- readOGR("~/work/ONECGIAR/Data/Africa_shp/African_continet.shp")
sh_ctry <- spTransform(sh_ctry, crs("+proj=longlat +ellps=WGS84 +no_defs"))
sh_xt <- extent(sh_ctry)
sh_xt@xmin <- sh_xt@xmin-1; sh_xt@ymin <- sh_xt@ymin-1; sh_xt@xmax <- sh_xt@xmax+1; sh_xt@ymax <- sh_xt@ymax+1

# Create a Mask- ---
load(paste(cimdir,"/input_data/wc_prec.rda",sep=""))
msk <- wc_prec[[1]]; rm(wc_prec)
msk[which(!is.na(msk[]))] <- 1

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

#create Scenarios x Years x Thresholds Loop ####
Scenarios <- c("rcp4.5", "rcp8.5")
Years <- c(2030, 2050)
Thresholds <- c(0.0, 0.15, 0.27, 0.41)
Vars <- expand.grid(Years=Years, Scenarios=Scenarios, Threshold=Thresholds)
Vars$Scenarios <- as.character(Vars$Scenarios)
Vars <- rbind(Vars,expand.grid(Years=NA, Scenarios="baseline", Threshold=Thresholds))

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
    
    #load ERA climate hazard and soil data
    cat("...loading ERA hazards and soil input data\n")
    load_ERA_data(cimdir, DoLite, Year, Scenario)
    
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
        
        #folder and file
        pr_odir <- paste(cimdir,"/T",Threshold,"_v",vr,"/",Product,"/",Year,"/",gsub("[.]","_",Scenario),"/",gsub(" ", "_", prname, fixed=TRUE),"_v",vr,sep="")
        pr_ofil <- paste(pr_odir,"/max_similarity_final_pos.tif",sep="")
        if (!file.exists(pr_ofil)) {
            cat("...practice=", prname, "does not have an output, so deleting and redoing\n")
            cat("...dir=", pr_odir, "\n")
            
            #remove directory if it exists
            if (file.exists(pr_odir)) {unlink(pr_odir, recursive=TRUE, force=TRUE)}
            
            #run practice
            xres <- run_points(i, pr_df=Y, data_sites, cimdir, Threshold, Year, Scenario, vr, 
                               etype='pos', DoLite)
        } else {
            cat("...practice=", prname, "seems to be ok (-:\n")
        }
    }
}
