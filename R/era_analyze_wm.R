#' Analyze ERA Weighted Mean Differences
#'
#' @param data A data frame or data table containing the dataset to analyze.
#' @param rmOut Logical; if TRUE, outliers are removed based on the ERAg::OutCalc function.
#' @param aggregate_by Character; the name(s) of the column(s) by which to aggregate the data.
#' @param rounding Integer; the number of decimal places to which numeric output should be rounded.
#'
#' @details The function performs several steps in analyzing weighted mean effects:
#' - It calculates the difference between the treatment and control means.
#' - It optionally removes outliers using the ERAg::OutCalc function.
#' - It excludes cases where the treatment and control means are identical.
#' - It computes various statistics such as weighted mean, weighted standard error,
#'   confidence intervals, median, variance, and quantiles for aggregated data.
#' - For modeling, it either uses mixed-effects models via lmerTest::lmer or linear models
#'   via lm, depending on the data characteristics.
#' - It rounds all numeric output to the specified number of decimal places.
#' 
#' @return A data table with the aggregated and analyzed results, including statistics and model estimates.
#'
#' @examples
#' # Assume 'dt' is a data.table with appropriate structure and columns
#' results <- era_analyze_wm(data = dt, rmOut = TRUE, aggregate_by = "GroupColumn", rounding = 2)
#'
#' @importFrom data.table data.table
#' @importFrom ERAg OutCalc
#' @importFrom stats shapiro.test weighted.mean
#' @importFrom spatstat.geom weighted.median weighted.quantile
#' @importFrom Hmisc wtd.var
#' @importFrom diagis weighted_se
#' @importFrom lmerTest lmer
#' @export
era_analyze_wm<-function(data,rmOut=T,aggregate_by,rounding=5){
  
  options(scipen=999)
  
  data<-data[,value:=MeanT-MeanC][!is.na(value)][Units=="kg/ha"]

  
  # Remove Outliers
  if(rmOut){
    Outliers<-unlist(data[,R:=1:nrow(data)
    ][,list(Outliers=list(R[ERAg::OutCalc(value)])), by=aggregate_by
    ][,Outliers])
    
    data<-data[!Outliers]
  }
  
  # Remove any data where all MeanC and MeanT are identical (creates Error in asMethod(object) : not a positive definite matrix)
  data<-data[,Identical:=all(MeanT==MeanC),by=aggregate_by][Identical==F][,Identical:=NULL]
  
  
  # Estimate treatment effect size ####
  FunShap<-function(X){
    tryCatch(stats::shapiro.test(X)$p.val,error=function(cond) {as.numeric(NA)})
  }
  
  # 2.4b) Slower, more accurate: Code that estimate parameters using LMs/LMMs and generates signifance values ####
  # I suggest that we pre-make tables for different grouping and then read these in rather than recalculating the data on the fly.
  
  # t value = test statistic from lm or lmm model, where NA there were insufficient data to run a test
  # Pr(>|t|) = significance of test statistic
  
  extract_model<-function(X,Prefix){
    if(!(is.na(Prefix)|Prefix=="")){
      Names<- paste0(Prefix,".",c("Estimate","Std. Error","t value","Pr(>|t|)"))
    }else{
      Names<- c("Estimate","Std. Error","t value","Pr(>|t|)")
    }
    
    if(!class(X)=="logical"){
      if(class(X)[1]=="lmerModLmerTest"){
        X<-summary(X)$coefficients[-3]
        names(X)<-Names
      }else{
        X<-summary(X)$coefficients
        names(X)<-Names
      }
    }else{
      X<-rep(NA,4)
    }
    return(X)
  }
  
  weight_group<-unique(c("Code",aggregate_by))
  
  results<-data[,N.Obs.Study:=.N,by=weight_group # Recalculate Weightings
  ][,Weight.Study:=(Rep^2/(2*Rep))/N.Obs.Study # Recalculate Weightings
  ][,list(Observations=.N,
          Studies=length(unique(Code)),
          Sites=length(unique(Site.Key)),
          Shapiro.Sig=FunShap(value),
          wmean=stats::weighted.mean(value,Weight.Study,na.rm=T),
          wmean_t=stats::weighted.mean(MeanT,Weight.Study,na.rm=T),
          wmean_c=stats::weighted.mean(MeanC,Weight.Study,na.rm=T),
          median=spatstat.geom::weighted.median(x=value,w=Weight.Study,na.rm = T),
          var=suppressWarnings(abs(Hmisc::wtd.var(value,Weight.Study,na.rm=T))),
          se=diagis::weighted_se(value, Weight.Study, na.rm=T),
          ci.low.lm=suppressWarnings(confint(lm(value~1,weights=Weight.Study))[1]),
          ci.high.lm=suppressWarnings(confint(lm(value~1,weights=Weight.Study))[2]),
          quantiles0.25=if(.N==1){as.character(NA)}else{if(length(unique(value))==1){as.character(NA)}else{paste(round(spatstat.geom::weighted.quantile(value,Weight.Study,probs=seq(0,1,0.25),na.rm=T),rounding),collapse="|")}},
          units=if(length(unique(Units))==1){unique(Units)}else{"Multiple"},
          # To run the lmer the requirement of three or more sites of which two must have at least three observations must be met
          # If not sufficient data for random-effects model run a t-test if >5 observations
          lmer=list(if(length(unique(Site.Key))>2 & sum(table(Site.Key)>2)>=2 & !sum(Out.SubInd %in% c("Feed Conversion Ratio (FCR)","Protein Conversion Ratio (PCR)"))>0){
            suppressWarnings(lmerTest::lmer(value~1 + (1|Site.Key),weights=Weight.Study))
          }else{
            if(length(Site.Key)>5 & !sum(Out.SubInd %in% c("Feed Conversion Ratio (FCR)","Protein Conversion Ratio (PCR)"))>0){
              suppressWarnings(lm(value~1,weights=Weight.Study))
            }else{NA}
          })
  ),
  by=aggregate_by]
  
  models<-do.call("rbind",lapply(1:nrow(results),FUN=function(i){
    round(extract_model(results[i,lmer][[1]],""),rounding)
  }))
  
  ci<-lapply(1:nrow(results),FUN=function(i){
    X<-results[i,lmer][[1]]
    suppressWarnings(
      if(class(X) %in% c("lm","lmerModLmerTest")){
        X<-round(confint(X,method="Wald"),rounding)
        if(nrow(X)>1){
          X[3,]
        }else{
          X
        }
      }else{
        c(NA,NA)
      }
    )
  })
  
  results_merged<-cbind(results,models)
  
  setnames(results_merged,c("Estimate","Std. Error","Pr(>|t|)","t value"),c("estimate","se.model","sig","t.value"))
  
  results_merged[,model:=unlist(lapply(lmer,class))
                 ][,ci.low.lmer:=unlist(lapply(ci,"[",1))
                   ][,ci.high.lmer:=unlist(lapply(ci,"[",2))
                     ][,lmer:=NULL]
    
  results_merged[model=="logical",model:=NA]
  
  # Round output
  results_merged[, (names(results_merged)) := lapply(.SD, function(x) if (is.numeric(x)) round(x, digits = rounding) else x)]
  
  return(results_merged)
}
