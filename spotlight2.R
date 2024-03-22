# Install and load packages ####
load_and_install_packages <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package)
      library(package, character.only = TRUE)
    }
  }
}

# List of packages to be loaded
packages <- c("data.table","metafor","ggplot2","terra","readxl")

# Call the function to install and load packages
load_and_install_packages(packages)

# Attempt to load ERAg
if (!require(ERAg, quietly = TRUE)) {
  # If ERAg is not available, then check for devtools and install it if necessary
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
  }
  
  # Now that devtools is ensured to be installed, load it
  library(devtools)
  
  # Install ERAg from GitHub
  devtools::install_github("EiA2030/ERAg")
  
  # Load ERAg after installation
  library(ERAg)
  
  message("ERAg has been successfully installed from GitHub and loaded.")
} else {
  message("ERAg is already installed and loaded.")
}

# source weighted mean function, until added to ERAg package
source("R/era_analyze_wm.R")

# 0) Set analysis parameters ####

# Should practices be aggregated (so practice alone and in combination with other practices?)
practice_aggregation<-T

# Set min studies
min_studies<-3

# Set columns to subset by and new names for them for use with output tables (Figure 1)
rename_cols<-c(Practice="PrName",
               Outcome="Out.SubInd",
               Product="Product.Simple",
               Observations="Observations",
               Studies="Studies",
               Value="RR.pc.jen",
               Value="estimate",
               Value.se="se.model",
               Sig="RR.Pr(>|t|)",
               Sig="sig",
               CIlow="RR.pc.jen.CIlow",
               CIhigh="RR.pc.jen.CIhigh",
               CIlow="ci.low.lmer",
               CIhigh="ci.high.lmer"
               )
new_cols<-unique(names(rename_cols))

# Rename and subset columns for Figures 2 and 3
rename_cols2<-c(rename_cols,Mean_T="wmean_t",Mean_C="wmean_c",AEZ_Class_FAO="AEZ_Class_FAO")
names(rename_cols2)[names(rename_cols2)=="Value"]<-"Mean_Difference"
names(rename_cols2)[names(rename_cols2)=="Value.se"]<-"Se"
names(rename_cols2)[names(rename_cols2)=="Studies"]<-"N_Pub"
names(rename_cols2)[names(rename_cols2)=="Observations"]<-"N_Obs"
names(rename_cols2)[names(rename_cols2)=="CIlow"]<-"Lower"
names(rename_cols2)[names(rename_cols2)=="CIhigh"]<-"Upper"
names(rename_cols2)[names(rename_cols2)=="Product"]<-"Crop"
new_cols2<-unique(names(rename_cols2))


# Choose practices to include
pracs_include<-PracticeCodes[!(Theme %in% c("Animals","Energy","Non-CSA","Postharvest")|grepl("h",Code)),unique(Practice)]

# Choose practices to exclude
pracs_exclude<-c("Crop Residue","Intercropping or Rotation","Improved Rice Management","Other Agroforestry")

pracs_include<-pracs_include[!pracs_include %in% pracs_exclude]

# 1) Prepare data  ####
  # 1.1) Subset ERA.Compiled on practices ####
  # Subset to included practices
  data<-ERA.Compiled[grepl(paste0(pracs_include,collapse="|"),PrName) & Out.SubInd=="Crop Yield"]
  
  # Remove excluded practices
  # Note this will work for crop residue, but any practice at the end of a bundle may cause an issue 
  if(!practice_aggregation){
    data<-data[!PrName %in% pracs_exclude][!grepl(paste0(paste0(pracs_exclude,"-"),collapse="|"),PrName)]
  }
  
  # Remove aggregated or blank products
  data<-data[!(grepl("-",Product.Simple)|Product.Simple=="")]
  
  # 1.2) Prepare data  ####
  data_p<-PrepareERA(data,
                     CombineAll = if(practice_aggregation==T){T}else{F},
                     DoCombinations = if(practice_aggregation==T){T}else{F},
                     Perc.Neg = 0.5,
                     RmNeg=F,
                     PLevel="Practice",
                     Cols = c("Code","Country","Latitude","Longitude","Site.Type","ID","Site.ID","Rep","Diversity","Tree","Variety","Duration","M.Year","EU","EUlist",
                              "Outcode","MeanC","USD2010.C","USD2010.T","MeanC.Error","MeanT","MeanT.Error","Mean.Error.Type","Units","TID","CID","MeanFlip","plist","base.list","Product","Product.Type","Product.Subtype",
                              "Product.Simple","Out.Pillar","Out.SubPillar","Out.Ind","Out.SubInd","SubPrName","PrName","Theme","SubPrName.Base","PrName.Base",
                              "T.Descrip", "C.Descrip", "C.NI","C.NO", "T.NI", "T.NO","Partial.Outcome.Name","Partial.Outcome.Code","DataLoc","AEZ16simple"))
  
  if(practice_aggregation){
    data_p<-data_p$Data.Combos
    data_p<-data_p[!PrName %in% pracs_exclude]
  }
  
  # 1.3) Rename practices ####
  data_p[,PrName:=gsub("Mulch$","Mulch (Herbs)",PrName)
         ][,PrName:=gsub("Agroforestry Pruning","Mulch (Trees)",PrName)
           ][,PrName:=gsub("-","+",PrName)]
  
  # 1.4) Harmonize units  ####
  data_p[,unique(Units)]
  
  data_p[grep("Mg/ha|t/ha",Units),MeanC:=MeanC*1000
  ][grep("Mg/ha|t/ha",Units),MeanT:=MeanT*1000
  ][grep("Mg/ha|t/ha",Units),MeanC.Error:=MeanC.Error*1000
  ][grep("Mg/ha|t/ha",Units),MeanT.Error:=MeanT.Error*1000
  ][grep("Mg/ha|t/ha",Units),Units:=gsub("Mg/ha|t/ha","kg/ha",Units)]
  
  data_p[grep("kg/m2",Units),MeanC:=MeanC*100^2
  ][grep("kg/m2",Units),MeanT:=MeanT*100^2
  ][grep("kg/m2",Units),MeanC.Error:=MeanC.Error*100^2
  ][grep("kg/m2",Units),MeanT.Error:=MeanT.Error*100^2
  ][grep("kg/m2",Units),Units:=gsub("kg/m2","kg/ha",Units)]
  
  data_p[grep("g/m2",Units),MeanC:=MeanC*100^2/1000
  ][grep("g/m2",Units),MeanT:=MeanT*100^2/1000
  ][grep("g/m2",Units),MeanC.Error:=MeanC.Error*100^2/1000
  ][grep("g/m2",Units),MeanT.Error:=MeanT.Error*100^2/1000
  ][grep("g/m2",Units),Units:=gsub("g/m2","kg/ha",Units)]
  
  data_p[grep("kg/acre",Units),MeanC:=MeanC*2.47105
  ][grep("kg/acre",Units),MeanT:=MeanT*2.47105
  ][grep("kg/acre",Units),MeanC.Error:=MeanC.Error*2.47105
  ][grep("kg/acre",Units),MeanT.Error:=MeanT.Error*2.47105
  ][grep("kg/acre",Units),Units:=gsub("kg/acre","kg/ha",Units)]
  
  data_p[grep("USD/acre",Units),MeanC:=MeanC*2.47105
  ][grep("USD/acre",Units),MeanT:=MeanT*2.47105
  ][grep("USD/acre",Units),MeanC.Error:=MeanC.Error*2.47105
  ][grep("USD/acre",Units),MeanT.Error:=MeanT.Error*2.47105
  ][grep("USD/acre",Units),Units:=gsub("USD/acre","USD/ha",Units)]
  
  data_p[grep("kg/fed",Units),MeanC:=MeanC*2.381
  ][grep("kg/fed",Units),MeanT:=MeanT*2.381
  ][grep("kg/fed",Units),MeanC.Error:=MeanC.Error*2.381
  ][grep("kg/fed",Units),MeanT.Error:=MeanT.Error*2.381
  ][grep("kg/fed",Units),Units:=gsub("kg/fed","kg/ha",Units)]
  
  data_p[grep("ton/fed|Mg/fed",Units),MeanC:=MeanC*2.381*1000
  ][grep("ton/fed|Mg/fed",Units),MeanT:=MeanT*2.381*1000
  ][grep("ton/fed|Mg/fed",Units),MeanC.Error:=MeanC.Error*2.381*1000
  ][grep("ton/fed|Mg/fed",Units),MeanT.Error:=MeanT.Error*2.381*1000
  ][grep("ton/fed|Mg/fed",Units),Units:=gsub("ton/fed|Mg/fed","kg/ha",Units)]
  
  data_p[Units %in% c("","0"),Units:=NA]
  
  # 1.5) Remove yield observations that are not kg/ha  ####
  data_p[Out.SubInd=="Crop Yield",unique(Units)]
  data_p<-data_p[!(Units!="kg/ha" & Out.SubInd=="Crop Yield")]
  
  # 1.6) Convert standard errors to standard deviation  ####
  data_p[Mean.Error.Type=="SE (Standard Error)",MeanT.Error:=MeanT.Error*Rep^0.5
  ][Mean.Error.Type=="SE (Standard Error)",MeanC.Error:=MeanC.Error*Rep^0.5
  ][Mean.Error.Type=="SE (Standard Error)",Mean.Error.Type:="SD (Standard Deviation)"
  ][Mean.Error.Type!="SD (Standard Deviation)",c("MeanT.Error","MeanC.Error","Mean.Error.Type"):=NA]
  
  # 1.7) Remove error labels where errors do not exist  ####
  data_p[Mean.Error.Type=="SD (Standard Deviation)" & (is.na(MeanC.Error)|is.na(MeanT.Error)),Mean.Error.Type:=NA]
  # 1.8) Ensure missing error value is NA  ####
  data_p[Mean.Error.Type=="",Mean.Error.Type:=NA]
  # 1.9) Calculate CV  ####
  data_p[,MeanT.CV:=MeanT.Error/MeanT][,MeanC.CV:=MeanC.Error/MeanC]
  
# 2) Summarize data ####
# Total Number of studies and observations
data_p[,list(Observations=.N,Studies=length(unique(Code)))]

# Explore products
data_p[,list(Studies=length(unique(Code))),by=Product][order(Studies,decreasing = T)]

# Observations and studies by practice
summary<-rbindlist(lapply(1:length(pracs_include),FUN=function(i){
  prac<-pracs_include[i]
  data.table(practice=prac,
             observations=data_p[grepl(prac,PrName),.N],
             studies=data_p[grepl(prac,PrName),length(unique(Code))])[studies!=0]
}))

(summary<-summary[order(studies,decreasing = T)])

# Observations and studies by practice
summary<-data_p[grepl(paste0(pracs_include,collapse="|"),PrName),list(observations=.N,studies=length(unique(Code))),by=list(PrName)
       ][order(studies,decreasing = T)
         ][,PrName:=gsub("-","+",PrName)]

(summary)

# Include base practices
data_p[grepl(paste0(pracs_include,collapse="|"),PrName),list(Observations=.N,Studies=length(unique(Code))),by=list(PrName,PrName.Base)
][order(Studies,decreasing = T)][1:30]

# Raw data
(data_p_summ<-data_p[,list(Code,Country,Site.ID,PrName,PrName.Base,Tree,Diversity,Product.Simple,Out.SubInd,MeanT,MeanC,Units)
][,list(MeanT=mean(MeanT,na.rm=T),MeanC=mean(MeanC,na.rm=T)),by=list(Code,Country,Site.ID,PrName,PrName.Base,Tree,Diversity,Product.Simple,Out.SubInd,Units)])

# 3) Notebook 3 plots & data ####
if(!dir.exists("data/spotlight4")){
  dir.create("data/spotlight4")
}
# 3.1) Figure 1 ####
# Analyze data
figure1_dat<-era_analyze_wm(data=data_p,rmOut=T,aggregate_by=c("PrName","Out.SubInd"),rounding=5)
# Include results which meet minimum data requirements
figure1_dat<-figure1_dat[Studies>=min_studies][,Product:="all"]
# Rename and subset columns
setnames(x=figure1_dat,
         old=rename_cols,
         new=names(rename_cols),
         skip_absent = T)

figure1_dat<-figure1_dat[,..new_cols][order(Studies,decreasing=T)]

figure1_dat<-figure1_dat[,list(Practice,Outcome,Product,Observations,Studies,Value,Sig,Value.se,CIlow,CIhigh)]

figure1_dat[,Value:=Value/1000
            ][,Value.se:=Value.se/1000
              ][,CIlow:=CIlow/1000
                ][,CIhigh:=CIhigh/1000
                  ][,Units:="Mg/ha"]

# Save data
fwrite(figure1_dat,file="data/spotlight2/spotlight2_Extended Table 2.csv")

# Plot 
ggplot(figure1_dat[order(Observations,decreasing=T)], 
       aes(x=reorder(Practice, Observations), 
           y=Observations,
           fill=Observations)) + 
  geom_bar(stat="identity") + 
  labs(x="Practice", y="Observations") + 
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
        panel.grid.minor.y = element_blank())+
  coord_flip() +# This will flip the axes so the practices are on the y-axis like in your image
  scale_fill_gradient(low = "yellow", high = "darkgreen",guide="none")   # Color gradient from yellow to dark green
  
# 3.2) Figure 2: Mean difference aggregated across cr####

figure2_dat<-era_analyze_wm(data=data_p,rmOut=T,aggregate_by=c("PrName","Out.SubInd","Product.Simple"),rounding=5)

figure2_dat<-figure2_dat[Studies>min_studies][order(Studies,decreasing = T)]

# Rename and subset columns
setnames(x=figure2_dat,
         old=rename_cols2,
         new=names(rename_cols2),
         skip_absent = T)

new_cols2a<-new_cols2[!grepl("AEZ",new_cols2)]

figure2_dat<-figure2_dat[,..new_cols2a][order(N_Pub,decreasing=T)]

figure2_dat<-figure2_dat[,list(Practice,Crop,N_Obs,N_Pub,Mean_T,Mean_C,Mean_Difference,Se,Lower,Upper)]

figure2_dat[,Mean_Difference:=Mean_Difference/1000
            ][,Se:=Se/1000
              ][,Lower:=Lower/1000
                ][,Upper:=Upper/1000
                  ][,Mean_T:=Mean_T/1000
                    ][,Mean_C:=Mean_C/1000]

# Save data
fwrite(figure2_dat,file="data/spotlight2/figure_2_data.csv")

# Plot 
ggplot(figure2_dat[Crop=="Maize" & N_Pub >4][order(N_Obs,decreasing=T)], 
       aes(x=reorder(Practice, Mean_Difference), y=Mean_Difference,color=Mean_Difference)) + 
  geom_point(aes(size=N_Pub ))+ 
  scale_color_gradient2(low = "yellow2",mid="greenyellow", high = "darkgreen",guide="none") +  # Color gradient from yellow to dark green
  geom_hline(yintercept = 0, linetype = "dashed") +  # Add a dashed vertical line (since coord_flip will be used)
  labs(x="Practice", y="Mean difference in yield (kg/ha)") + 
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +  # Add horizontal error bars
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
        panel.grid.minor.y = element_blank()) +
  geom_text(aes(label = N_Pub , y = 4500), hjust = 0) +  # Add text labels to the right
  coord_flip() # This will flip the axes so the practices are on the y-axis like in your image



# 3.3) Figure 3: Mean difference by agroecology zone ####
# Add AEZ to dataset
aez<-terra::rast("data/aez/aez_v9v2red_5m_CRUTS32_Hist_8110_100_avg.tif")

crs(aez)<- "+proj=longlat +datum=WGS84 +no_defs"

# Since the extent needs to be -180 to +180 and -90 to +90, 
ext(aez) <- c(-180, 180, -90, 90)

# Read in class values
aez_meta<-readxl::read_excel("data/aez/Mean_yield_difference.xlsx",sheet="AEZ_Classes")
aez_names<-aez_meta$AEZ_Class_FAO

cls<-data.frame(id=c(1:length(aez_names)),AEZ_Class_FAO=aez_names)
levels(aez)<-cls

terra::writeRaster(aez,filename = "data/aez/aez_v9v2red_5m_CRUTS32_Hist_8110_100_avg_v2.tif")

# Create points layer for era data
data_p[,ID:=1:.N]
data_p[,list(ID,Latitude,Longitude)]

pts <- vect(data_p, geom=c("Longitude", "Latitude"), crs="+proj=longlat +datum=WGS84")
pts_ext<-extract(aez,pts)

data_p<-merge(data_p,pts_ext)

# Analyze weighted means by agroecological zone
figure3_dat<-era_analyze_wm(data=data_p,rmOut=T,aggregate_by=c("PrName","Out.SubInd","Product.Simple","AEZ_Class_FAO"),rounding=5)
figure3_dat<-figure3_dat[Studies>min_studies][order(Studies,decreasing = T)]

setnames(x=figure3_dat,
         old=rename_cols2,
         new=names(rename_cols2),
         skip_absent = T)

figure3_dat<-figure3_dat[,..new_cols2][order(N_Pub,decreasing=T)][,Outcome:=NULL]

# Convert to Mg/ha
figure3_dat[,Mean_Difference:=Mean_Difference/1000
            ][,Se:=Se/1000
              ][,Lower:=Lower/1000
                ][,Upper:=Upper/1000
                  ][,Mean_T:=Mean_T/1000
                    ][,Mean_C:=Mean_C/1000]

figure3_dat<-figure3_dat[,list(Practice,Crop,AEZ_Class_FAO,N_Obs,N_Pub,Mean_T,Mean_C,Mean_Difference,Se,Lower,Upper)]

fwrite(figure3_dat,file = "data/spotlight2/Mean_yield_difference.csv")

# 4) Impute missing standard deviations ####
# We are following the All Cases approach detailed in Nakagawa, S., et al. (2022). "A robust and readily implementable method for the meta-analysis of response ratios with and without missing standard deviations." Ecol Lett. doi: 10.1111/ele.14144
# Nagakawa 2022 use  Taylor expansion proposed by Lajeunesse 2011 to calculate the log response ratio (equation 6 in their publication) and the sampling variance 
# for log response ratio (equation 7 in their publication).

# Products

error_prods<-unique(data_p[!is.na(MeanT.Error)  & Out.SubInd=="Crop Yield",list(Product,Code)
][,error_studies:=length(unique(Code)),by=list(Product)
][,Code:=NULL
][order(error_studies,decreasing=T)])

era_dat_imputed<-rbindlist(pbapply::pblapply(1:length(error_prods),FUN=function(i){
  prod<-error_prods[i,Product]
  
  era_test<-data_p[Product %in% prod]
  
  era_test[,hist(unlist(c(MeanC,MeanT)))]
  
  
  if(any(prod %in% c("Cassava or Yuca"))){
    max_yield<-60000
    era_test[Out.SubInd=="Crop Yield" & (MeanT>max_yield|MeanC>max_yield) & Product %in% c("Cassava or Yuca"),list(Code,MeanC,MeanT,DataLoc)]
    era_test<-era_test[!(Out.SubInd=="Crop Yield" & (MeanT>max_yield|MeanC>max_yield) & Product %in% c("Cassava or Yuca"))]
  }
  
  if(any(prod %in% c("Potato","Yam"))){
    max_yield<-50000
    era_test[Out.SubInd=="Crop Yield" & (MeanT>max_yield|MeanC>max_yield) & Product %in% c("Yam","Potato"),list(Code,MeanC,MeanT,DataLoc)]
    era_test<-era_test[!(Out.SubInd=="Crop Yield" & (MeanT>max_yield|MeanC>max_yield) %in% c("Yam","Potato"))]
  }
  
  if(any(prod %in% c("Soybean","Teff"))){
    max_yield<-6000
    era_test[Out.SubInd=="Crop Yield" & (MeanT>max_yield|MeanC>max_yield) & Product %in% c("Soybean","Teff"),list(Code,MeanC,MeanT,DataLoc)]
    era_test<-era_test[!(Out.SubInd=="Crop Yield" & (MeanT>max_yield|MeanC>max_yield) & Product %in% c("Soybean","Teff"))]
  }
  
  if(any(prod %in% c("Cowpea","Groundnut or Peanut"))){
    max_yield<-4000
    era_test[Out.SubInd=="Crop Yield" & (MeanT>max_yield|MeanC>max_yield) & Product %in% c("Cowpea","Groundnut or Peanut") & EU!="h8.1",list(Code,MeanC,MeanT,DataLoc)]
    era_test<-era_test[!(Out.SubInd=="Crop Yield" & (MeanT>max_yield|MeanC>max_yield) & Product %in% c("Cowpea","Groundnut or Peanut") & EU!="h8.1")]
  }
  
  if(any(prod %in% c("Common Bean"))){
    max_yield<-7000
    era_test[Out.SubInd=="Crop Yield" & (MeanT>max_yield|MeanC>max_yield) & Product %in% c("Common Bean"),list(Code,MeanC,MeanT,DataLoc)]
    era_test<-era_test[!(Out.SubInd=="Crop Yield" & (MeanT>max_yield|MeanC>max_yield) & Product %in% c("Common Bean"))]
  }
  
  
  if(any(prod %in% c("Sorghum","Pearl Millet","Barley"))){
    max_yield<-10000
    era_test[Out.SubInd=="Crop Yield" & (MeanT>max_yield|MeanC>max_yield) & Product %in% c("Sorghum","Pearl Millet","Barley"),list(Code,MeanC,MeanT,DataLoc)]
    era_test<-era_test[!(Out.SubInd=="Crop Yield" & (MeanT>max_yield|MeanC>max_yield) & Product %in% c("Sorghum","Pearl Millet","Barley"))]
  }
  
  if(any(prod %in% c("Rice"))){
    max_yield<-12000
    era_test[Out.SubInd=="Crop Yield" & (MeanT>max_yield|MeanC>max_yield) & Product %in% c("Rice"),list(Code,MeanC,MeanT,DataLoc)]
    era_test<-era_test[!(Out.SubInd=="Crop Yield" & (MeanT>max_yield|MeanC>max_yield) & Product %in% c("Rice"))]
  }
  
  if(any(prod %in% c("Maize","Wheat"))){
    max_yield<-15000
    era_test[Out.SubInd=="Crop Yield" & (MeanT>max_yield|MeanC>max_yield) & Product %in% c("Maize","Wheat"),list(Code,MeanC,MeanT,DataLoc)]
    era_test<-era_test[!(Out.SubInd=="Crop Yield" & (MeanT>max_yield|MeanC>max_yield) & Product %in% c("Maize","Wheat"))]
  }
  
  era_test[,RR_naka_yield:=ERAg::lnrr_naka(m1=MeanT,
                                            m2=MeanC,
                                            n1=Rep,
                                            n2=Rep,
                                            n1_data=era_test[Mean.Error.Type=="SD (Standard Deviation)" & Out.SubInd=="Crop Yield",Rep],
                                            n2_data=era_test[Mean.Error.Type=="SD (Standard Deviation)" & Out.SubInd=="Crop Yield",Rep],
                                            CV1_data=era_test[Mean.Error.Type=="SD (Standard Deviation)" & Out.SubInd=="Crop Yield",MeanT.CV],
                                            CV2_data=era_test[Mean.Error.Type=="SD (Standard Deviation)" & Out.SubInd=="Crop Yield",MeanC.CV])]
  
  era_test[,vRRimputed:=v_lnrr_naka(n1=Rep,
                                     n2=Rep,
                                     n1_data=era_test[Mean.Error.Type=="SD (Standard Deviation)" & Out.SubInd=="Crop Yield",Rep],
                                     n2_data=era_test[Mean.Error.Type=="SD (Standard Deviation)" & Out.SubInd=="Crop Yield",Rep],
                                     CV1_data=era_test[Mean.Error.Type=="SD (Standard Deviation)" & Out.SubInd=="Crop Yield",MeanT.CV],
                                     CV2_data=era_test[Mean.Error.Type=="SD (Standard Deviation)" & Out.SubInd=="Crop Yield",MeanC.CV])]
  
  era_test[,RR:=log(MeanT/MeanC)
  ][,vRR:=((MeanC.CV^2)/Rep)+((MeanT.CV^2)/Rep)
  ][,vRR_laj:=((MeanC.CV^2)/Rep)+((MeanT.CV^2)/Rep)+((MeanC.CV^4)/(2*Rep^2))+((MeanT.CV^2)/(2*Rep^2))][,c("yi","pc"):=NULL]
  
  era_test[,cv_t_pooled:=era_test[Mean.Error.Type=="SD (Standard Deviation)" & Out.SubInd=="Crop Yield",sum(Rep*MeanT.CV)/sum(Rep)]
  ][,cv_c_pooled:=era_test[Mean.Error.Type=="SD (Standard Deviation)" & Out.SubInd=="Crop Yield",sum(Rep*MeanC.CV)/sum(Rep)]
  ][,SD_t_imputed:=((cv_t_pooled*MeanT)^2)^0.5
  ][,SD_c_imputed:=((cv_c_pooled*MeanC)^2)^0.5]
  
  era_test
}))

era_dat_imputed<-era_dat_imputed[(is.infinite(RR)|is.na(RR)),c("RR","vRR","vRR_laj","vRRimputed","RR_naka_yield"):=NA]

# Check distribution
# % of effect sizes that fail Lajeunesse (2015) test for assumption of normality.
era_dat_imputed[!is.na(MeanC.CV),paste0(round(100*sum(((1/MeanC.CV)*((4*Rep^(3/2))/(1+4*Rep)))<3)/.N,2),"%"),by=Out.SubInd]
era_dat_imputed[!is.na(MeanT.CV),paste0(round(100*sum(((1/MeanT.CV)*((4*Rep^(3/2))/(1+4*Rep)))<3)/.N,2),"%"),by=Out.SubInd]

# Where Rep==1 add one for SMD calcs (otherwise escalc yield an error)
era_dat_imputed[,Rep_a1:=Rep][Rep==1,Rep_a1:=Rep+1]

# 5) Claculate standardized mean difference ####
era_dat_imputed <- data.table(escalc(measure = "SMDH", 
                                     m1i= MeanT, 
                                     m2i= MeanC, 
                                     sd1i= SD_t_imputed,
                                     sd2i= SD_c_imputed,
                                     n1i= Rep_a1, 
                                     n2i= Rep_a1, 
                                     data= era_dat_imputed, 
                                     var.names=c("SMD","vSMD"),
                                     vtype="LS",
                                     digits=4))

era_dat_imputed[,Rep_a1:=NULL]

# 6) Run 3-level models ####
# Create unique observation ID
era_dat_imputed[,ES_ID:=as.character(1:.N)]

UT<-era_dat_imputed[,unique(unlist(strsplit(Theme,"-")))]
rbindlist(lapply(UT,FUN=function(THEME){
  X<-era_dat_imputed[grepl(THEME,Theme),
                     list(Countries=length(unique(Country)),
                          Studies=length(unique(Code)),
                          Observations=.N)]
  X$Theme<-THEME
  X
  
}))

Control<-list(optimizer="Nelder-Mead")
min_studies<-3

# Is variable RR or SMD? If latter replace RR with SMD
Variable<-"RR"

results_raw<-lapply(c(T,F),FUN=function(rmOut){
    
    if(Variable == "RR"){
    data<-era_dat_imputed[!is.na(RR)]
    }
  
    if(Variable == "SMD"){
      data<-era_dat_imputed[!is.na(SMD)]
    }
    
    Practices<-data[,list(Studies=length(unique(Code))),by=PrName][order(Studies,decreasing=T)][Studies>=min_studies,unique(PrName)]
    
    
    results<-lapply(Practices,FUN=function(Practice){
      
      cat('\r                                                                                                              ')
      cat('\r',paste0(Practice," | Outlier Removal = ",rmOut," | Variable = ",Variable))
      flush.console()
      
      data_sub<-data[PrName==Practice
      ][,N.Obs.Study:=.N,by=Code
      ][,Weight.Study:=((Rep*Rep)/(Rep+Rep))/N.Obs.Study
      ][,Weight.Study2:=((Rep*Rep)/(Rep+Rep))] # Recalculate Weightings
      
    
      if(Variable == "SMD"){
        data_sub[,RR_naka_yield:=SMD][,vRRimputed:=vSMD]
      }
      
      # Remove Outliers
      if(rmOut){
        data_sub[,Outliers:=OutCalc(RR_naka_yield)]
        data_sub<-data_sub[Outliers==F][,Outliers:=NULL]
      }
      
      #REML(REstricted Maximum Likelihood estimation method): method is superior to other methods (see, Hox, 2010; Viechtbauer, 2005), but has restrictions (see Cheung, 2014; Van den Noortgate et al., 2013).
      #Estimate the overall mean effect size by fitting an intercept-only model.
      #RESULTS: sigma^2.1 (estim): variance between ES within studies (level 2) if it is small indicates that the ES are similar within studies (Cheung 2014)
      #RESULTS: sigma^2.2(estim): variance between studies (level 3): if it is large, indicates the population effect sizes vary across Level 3, so study characteristics can be included to explain the heterogeneity at level 3 (Cheung 2014)
      #RESULTS: Test for heterogeneity: p-val<0.001 significant variation between all effect sizes in the data set.
      results<-list()
      

      overall_mod <- rma.mv(y= RR_naka_yield, V=vRRimputed, random = list(~ 1 | ES_ID, ~ 1 | Code), 
                            tdist= TRUE, data=data_sub, method="REML",verbose = F,control=Control)
      
      results[["overall_mod"]][["model"]]<-overall_mod
      results[["overall_mod"]][["summary"]]<-summary(overall_mod, digits=3)
      
      
      # Test simple approach that does not use measurement variance
      if(data_sub[,length(unique(Code))==.N]){
        results[["overall.novar"]][["model"]]<-NULL
        results[["overall.novar"]][["summary"]]<-NULL
        
        results[["overall.novar2"]][["model"]]<-NULL
        results[["overall.novar2"]][["summary"]]<-NULL
      }else{
        overall.novar<-lmer(data=data_sub,RR_naka_yield~1 + (1|Code),weights=Weight.Study)
        overall.novar2<-lmer(data=data_sub,RR_naka_yield~1 + (1|Code),weights=Weight.Study2)
        
        results[["overall.novar"]][["model"]]<-overall.novar
        results[["overall.novar"]][["summary"]]<-summary(overall.novar, digits=3)
        
        results[["overall.novar2"]][["model"]]<-overall.novar2
        results[["overall.novar2"]][["summary"]]<-summary(overall.novar2, digits=3)
      }
      

        #####Heterogeneity of within-study variance (level 2)###
        #Build a two-level model without within-study variance.
        #If the test results provide support for rejecting the null hypothesis, we can conclude that the fit of the original three-level model is statistically better than the fit of the two-level model, and consequently, that there is significant variability between effect sizes within studies.
        #sigma2=c(0,NA) = the argument is taken by the rma.mv function when the user wants to fix a specific variance component to a user-defined value. The first parameter (0) states that the within-study variance is fixed to zero (i.e., no within-study variance will be modeled), and the second parameter (NA) states that the between-study variance is estimated.
        #The variance at the first level (sampling variance) was not included in the model, because it is assumed to be known.
        modelnovar2 <- rma.mv(y=RR_naka_yield, V=vRRimputed, random = list(~ 1 | ES_ID, ~ 1 | Code), 
                              sigma2=c(0,NA), tdist=TRUE, data=data_sub, method="REML",verbose=F,control=Control)
        
        results[["modelnovar2"]][["model"]]<-modelnovar2
        results[["modelnovar2"]][["summary"]]<-summary(modelnovar2, digits=3)
        
        #Full= represents the three-level model stored in the object overall_mod
        #Reduced= represents the two-level model stored in the object modelnovar2
        # If LRT<pval there is significant variability between effect sizes within studies
        results[["anova_overall_vs_novar2"]]<-anova(overall_mod,modelnovar2) 
        
        #Heterogeneity of between-study variance (level 3)
        # Build a two-level model without between-study variance;
        modelnovar3 <- rma.mv(y=RR_naka_yield, V=vRRimputed, random = list(~ 1 | ES_ID, ~ 1 | Code), 
                              sigma2=c(NA,0), tdist=TRUE, data=data_sub, method="REML",verbose=F,control=Control)
        
        results[["modelnovar3"]][["model"]]<-modelnovar3
        results[["modelnovar3"]][["summary"]]<-summary(modelnovar3, digits=3)
        
        # If the null hypothesis should be rejected based on the test results, we can conclude that the fit of the original three-level model is statistically better than the fit of the two-level model, and consequently, that there is significant variability between studies.
        # sigma2=c(NA,0): Since we want to fix the between-study variance to zero and freely estimate the within-study variance
        # If LRT<pval there is significant variability between studies
        results[["anova_overall_vs_novar3"]]<-anova(overall_mod,modelnovar3) 
        
        # Build a two-level model without between-study variance;
        modelnovar4 <- rma.mv(y=RR_naka_yield, V=vRRimputed, random = list(~ 1 | ES_ID, ~ 1 | Code), 
                              sigma2=c(NA,NA), tdist=TRUE, data=data_sub, method="REML",verbose=F,control=Control)
        
        results[["modelnovar4"]][["model"]]<-modelnovar4
        results[["modelnovar4"]][["summary"]]<-summary(modelnovar4, digits=3)
        
        ###The distribution of the variance over the three levels of the meta-analytic model
        #Recall that three different sources of variance are modeled in our meta-analytic model: sampling variance at the first level; within-study variance at the second level; and between-study variance at the third level.
        #To determine how much variance can be attributed to differences between effect sizes within studies (level 2) and to differences between studies (level 3), formulas given by Cheung (2014 - formula 14 on page 2015) can be used to determine how the total variance is distributed over the three levels of the meta-analytic model;
        #Print the results in percentages on screen.
        estimated.sampling.variance<-estimated.sampling.variance.func(data_sub[!is.na(vRRimputed),vRRimputed])
        
        results[["estimated.sampling.variance"]]<- estimated.sampling.variance
        
        ###Each of the three variance components (I2_1, I2_2, I2_3) is divided by the total amount of variance, so that a proportional estimate of each variance component is stored in an object.
        ###overall_mod$sigma2[1]: refers to the amount of within-study variance in the object overall_mod 
        ###overall_mod$sigma2[2]: refers to the amount of between-study variance in the object overall_mod
        ###The proportional estimates of the three variance components are multiplied by 100 (%), so that a percentage 
        #estimate of each variance component is stored in an object
        
        #Sampling variance (Amount of variance at level 1) 
        results[["sampling.variance"]]<- ((estimated.sampling.variance)/(overall_mod$sigma2[1]+overall_mod$sigma2[2]+estimated.sampling.variance))*100
        
        #Within-study variance (Amount of variance at level 2) 
        results[["sampling.variance.within.studies"]]<- ((overall_mod$sigma2[1]) / (overall_mod$sigma2[1] + overall_mod$sigma2[2] + estimated.sampling.variance))*100
        
        #Between-study variance (Amount of variance at level 3)
        results[["sampling.variance.between.studies"]]<- ((overall_mod$sigma2[2]) / (overall_mod$sigma2[1] + overall_mod$sigma2[2] + estimated.sampling.variance))*100
      
      results[["data.availability"]]<-data_sub[,list(Studies=length(unique(Code)),Observations=.N,Countries=length(unique(Country)))]
      
      results[["variable"]]<-Variable
      
      results[["outliers"]]<-if(rmOut){"Removed"}else{"Retained"}
      
      results
      
    })
    
    names(results)<-Practices
    results
  })
  

# Remove outcomes with no data
results_raw<-lapply(results_raw,FUN=function(results){results[sapply(results,length)!=0]})

names(results_raw)<-paste0("outliers_removed_",c(T,F))

