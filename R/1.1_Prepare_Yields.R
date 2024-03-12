require(data.table)

PrepareData<-function(X){
  # Harmonize Units ####
  Units2MgHa<-function(Data){
    
    Data<-data.table::data.table(Data)
    
    Data[,Original.Units:=Units
    ][,Original.MeanC:=MeanC
    ][,Original.MeanT:=MeanT
    ][Units %in% c("kg/ha","kg/ha/yr"),MeanC:=MeanC/1000
    ][Units %in% c("kg/ha","kg/ha/yr"),MeanT:=MeanT/1000
    ][Units %in% c("kg/ha","kg/ha/yr"),Units:="Mg/ha"
    ][Units %in% c("kg/m2"),MeanC:=MeanC*10
    ][Units %in% c("kg/m2"),MeanT:=MeanT*10
    ][Units %in% c("kg/m2"),Units:="Mg/ha"
    ][Units %in% c("kg/acre"),MeanC:=MeanC*10*0.404686
    ][Units %in% c("kg/m2"),MeanT:=MeanT*10*0.404686
    ][Units %in% c("kg/m2"),Units:="Mg/ha"
    ][Units %in% c("g/m2"),MeanC:=MeanC/100
    ][Units %in% c("g/m2"),MeanT:=MeanT/100
    ][Units %in% c("g/m2"),Units:="Mg/ha"
    ][Units %in% c("Mg/fed"),MeanC:=MeanC*0.41682658
    ][Units %in% c("Mg/fed"),MeanT:=MeanT*0.41682658
    ][Units %in% c("Mg/fed"),Units:="Mg/ha"
    ][Units %in% c("kg/fed"),MeanC:=MeanC*(1/0.41682658)/1000
    ][Units %in% c("kg/fed"),MeanT:=MeanT*(1/0.41682658)/1000
    ][Units %in% c("kg/fed"),Units:="Mg/ha"]
    
    return(Data)
  }
  
  X<-Units2MgHa(Data=X)
  
  # Exclude anything other than "Mg/ha", make sure sites have rainfall data and are not irrigated
  X<-X[Units=="Mg/ha"]
  
  return(X)
}

# ERA #####
OutcomeCodes<-data.table::fread("analogues/Data/Outcomes.csv")
PracticeCodes<-data.table::fread("analogues//Data/Practices.csv")
EUCodes<-data.table::fread("analogues/Data/EU.csv")

# Source functions to prepare and analyse data (in future these should be publically available from the ERAg package)
source("analogues/R/PrepareERA.R")
source("analogues/R/ERAAnalyze.R")
source("analogues/R/OutCalc.R")

# Set analysis aggregation level to site, practice, subindicator and product.simple
agg_by <- c("Site.ID","Country","Latitude","Longitude","Buffer","AEZ16simple","PrName","Out.SubInd","Product.Simple","Product.Type")

# Load data (in future data should be publically available from the ERAg package)
load("analogues/Data/ERA_Derived.rda")

# Subset data to crop yield
ERA_Derived<-ERA_Derived[Out.SubInd=="Crop Yield"]

# Removed Irrigated Dta
ERA_Derived<-ERA_Derived[!grepl("Irrigation",PrName.Base)][!grepl("Irrigation",PrName)]

# Harmonize Units
ERA_Derived<-PrepareData(X=ERA_Derived)


# Prepare ERA (see function documentation for more information)
ERAPrepared<-PrepareERA(data.table::copy(ERA_Derived),
                        DoCombinations=F,
                        CombineAll = F, 
                        PLevel = "Practice",
                        OutcomeCodes = OutcomeCodes,
                        PracticeCodes = PracticeCodes,
                        EUCodes = EUCodes)

# Set analysis aggregation level to site, practice, subindicator and product.simple
Aggregate.By <- c("Site.ID","Country","Latitude","Longitude","PrName","Out.SubInd","Product.Simple","Product.Type")

ERAPrepared<-ERAPrepared[!is.na(Yield.Diff)]

Data<-data.table::copy(ERAPrepared)


# Remove outliers?
if(T){
    Outliers<-unlist(Data[,R:=1:nrow(Data)
    ][,list(Outliers=list(R[OutCalc(Yield.Diff)])), by=Aggregate.By
    ][,Outliers])
    
    Data<-Data[!Outliers]
}

Weight.Group<-unique(c("Code",Aggregate.By))

Data<-Data[,N.Obs.Study:=.N,by=Weight.Group # Recalculate Weightings by study within obervations grouping
  ][,Weight.Study:=(Rep^2/(2*Rep))/N.Obs.Study # Recalculate Weightings
    ][list(Observations=.N,
          Studies=length(unique(Code)),
          Sites=length(unique(ID)),
          Yield.Diff=round(weighted.mean(log(MeanT-MeanC),Weight.Study,na.rm = T)),4),by=Aggregate.By]

