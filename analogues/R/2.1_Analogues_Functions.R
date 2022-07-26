#' Wrapper to run analogues function over ERA data for climate data
#' 
#' @export
#' @importFrom raster extract
 run_points_climate <- function(Index,Data, SaveDir, wc_prec,wc_tmean,wc_prec_fut,wc_tmean_fut,Verbose=F) {

     #verbose what i'm running
         if(Verbose){
             cat('\r                                 ')
             cat("Processing ", Index, "of", nrow(Data))
             flush.console()
             }
     
     File<-paste0(SaveDir,"/",Data$ID[Index],".tif")

        #loop through points 
     if(!file.exists(File)){
         par1 <- analogues::createParameters(x=Data$Lon[Index], 
                                            y=Data$Lat[Index], 
                                            vars=c("prec","tmean"),
                                            weights=c(0.75,0.25),
                                            ndivisions=c(12,12),
                                            growing.season=c(1,12),
                                            rotation="prec",
                                            threshold=1,
                                            env.data.ref=list(wc_prec,wc_tmean), 
                                            env.data.targ=list(wc_prec_fut,wc_tmean_fut),
                                            outfile=SaveDir,
                                            fname=Data$ID[Index],
                                            writefile=F)
        X<-analogues::calc_similarity(par1)
        terra::writeRaster(X,File,overwrite=T)
         gc()
         File
         }
}
#' Wrapper to run analogues function over ERA data for soils data
#' 
#' @export
#' @importFrom raster extract
 run_points_soil <- function(Index,Data, SaveDir, soilstk,Verbose=F,DoAll){
     data <- Data[Index] 

     #verbose what i'm running
         if(Verbose){
             cat('\r                                 ')
             cat("Processing ", Index, "of", nrow(Data))
             flush.console()
             }
     
     if(DoAll){
         SaveDir1 <- paste0(SaveDir,"/all")
         if(!dir.exists(SaveDir1)){
             dir.create(SaveDir1,recursive=T)
         }

         File<-paste0(SaveDir1,"/",Data$ID[Index],".tif")

         NVar<-length(soilstk)

         Soils<-lapply(1:NVar,FUN=function(ii){
             raster::stack(soilstk[[ii]])
             })

            #loop through points 
         if(!file.exists(File)){
             par1 <- analogues::createParameters(x=Data$Lon[Index], 
                                                y=Data$Lat[Index], 
                                                vars=names(soilstk),
                                                weights=rep(1/NVar,NVar),
                                                ndivisions=c(raster::nlayers(Soils[[1]]),raster::nlayers(Soils[[1]])),
                                                growing.season=c(1,raster::nlayers(Soils[[1]])),
                                                rotation="none",
                                                threshold=1,
                                                env.data.ref=Soils, 
                                                env.data.targ=Soils,
                                                outfile=SaveDir,
                                                fname=Data$ID[Index],
                                                writefile=F)
            X<-analogues::calc_similarity(par1)
            terra::writeRaster(X,File,overwrite=T)
         }
     }else{
         for(VAR in names(soilstk)){
            SaveDir1 <- paste0(SaveDir,"/",VAR)
             if(!dir.exists(SaveDir1)){
                 dir.create(SaveDir1,recursive=T)
             }
                      
             File<-paste0(SaveDir1,"/",Data$ID[Index],".tif")
             NVar<-1
             Soils<-raster::stack(soilstk[[VAR]])
             #loop through points 
             if(!file.exists(File)){
                par1 <- analogues::createParameters(x=Data$Lon[Index], 
                                                    y=Data$Lat[Index], 
                                                    vars=VAR,
                                                    weights=1,
                                                    ndivisions=c(raster::nlayers(Soils),raster::nlayers(Soils)),
                                                    growing.season=c(1,raster::nlayers(Soils)),
                                                    rotation="none",
                                                    threshold=1,
                                                    env.data.ref=Soils, 
                                                    env.data.targ=Soils,
                                                    outfile=SaveDir1,
                                                    fname=Data$ID[Index],
                                                    writefile=F)
                X<-analogues::calc_similarity(par1)
                terra::writeRaster(X,File,overwrite=T)
                gc()
                File
            }
        }
     }
 }        
#' Wrapper to run analogues function over ERA data for soils data
#' 
#' @export
#' @importFrom terra rast writeRaster
#' @import data.table
combine_analogues<-function(Index,Data,Combinations,SaveDir,overwrite,cimdir,SoilDir,gamma){
    Data<-as.data.frame(Data)
    
    Practice<-Combinations$PrName[Index]
    Product<-Combinations$Product.Simple[Index]
    Outcome<-Combinations$Out.SubInd[Index]
    Scenario<-Combinations$Scenarios[Index]
    Year<-Combinations$Years[Index]
    Threshold<-as.character(Combinations$Thresholds[Index])
    
    if(Scenario != "baseline"){
            FileName<-gsub(" ","_",paste0(c(paste0("t",Threshold),Year,Scenario,Practice,Product,Outcome),collapse="-"))
        }else{
            FileName<-gsub(" ","_",paste0(c(paste0("t",Threshold),Scenario,Practice,Product,Outcome),collapse="-"))
        }
    
    
    if(!dir.exists(SaveDir)){
        dir.create(SaveDir,recursive=T)
    }

    if((!file.exists(paste0(SaveDir,"/",FileName,"_maxsim.tif")))|overwrite==T){
        
        if(Scenario!="baseline"){
            ClimDir<-paste0(cimdir,"/",Year,"_",Scenario)
        }else{
            ClimDir<-paste0(cimdir,"/",Scenario)
        }
        
        if(Threshold == "all"){
            IDs<-unique(Data[which(Data$PrName==Practice & Data$Product.Simple==Product & Data$Out.SubInd==Outcome),"ID"])
        }else{
            if(!grepl("m",Threshold)){
                Threshold<-as.numeric(Threshold)
                IDs<-unique(Data[which(Data$PrName==Practice & Data$Product.Simple==Product & Data$Out.SubInd==Outcome & Data$RR.pc.jen>=Threshold),"ID"])
            }else{
                Threshold<-as.numeric(gsub("m","-",Threshold))
                IDs<-unique(Data[which(Data$PrName==Practice & Data$Product.Simple==Product & Data$Out.SubInd==Outcome & Data$RR.pc.jen<=Threshold),"ID"])
            }
        }
        
        IDs<-IDs[!is.na(IDs)]
        
        if(length(IDs)>0){
            simclim<-terra::rast(paste0(ClimDir,"/",IDs,".tif")) 
            simsol<-terra::rast(paste0(SoilDir,"/",IDs,".tif"))
            
            if(length(IDs)>1){
                # Find minimum suitability of soil and climate
                allstk<-terra::rast(lapply(IDs,FUN=function(ID){
                    simclim<-terra::rast(paste0(ClimDir,"/",ID,".tif"))
                    simsol<-terra::rast(paste0(SoilDir,"/",ID,".tif"))
                    min(c(simclim,simsol), na.rm=TRUE)
                }))
                
                maxsim <- max(allstk, na.rm=TRUE)
                fsim <- max(simclim, na.rm=TRUE)^(1-gamma) * max(simsol, na.rm=TRUE)^gamma
            }else{      
                maxsim<-min(c(simclim,simsol), na.rm=TRUE)
                fsim <- simclim^(1-gamma) * simsol^gamma
            }
            suppressWarnings(terra::writeRaster(maxsim,paste0(SaveDir,"/",FileName,"-",length(IDs),"-maxsim.tif"),overwrite=T))
            suppressWarnings(terra::writeRaster(fsim,paste0(SaveDir,"/",FileName,"-",length(IDs),"-fsim.tif"),overwrite=T))
            gc()
            FileName
        }
    }
}
#' Wrapper to run analogues function over ERA data for soils data
#' 
#' @export
#' @importFrom terra rast writeRaster
#' @import data.table
combine_analogues_limits<-function(Index,Data,Combinations,SaveDir,overwrite,cimdir,SoilDir,minsim,doRR=T,doN=F){
    Data<-as.data.frame(Data)
    
    Practice<-Combinations$PrName[Index]
    Product<-Combinations$Product.Simple[Index]
    Outcome<-Combinations$Out.SubInd[Index]
    Scenario<-Combinations$Scenarios[Index]
    Year<-Combinations$Years[Index]
    
     if(!dir.exists(SaveDir)){
        dir.create(SaveDir,recursive=T)
    }
    
    if(Scenario != "baseline"){
        FileName<-gsub(" ","_",paste0(c(Year,Scenario,Practice,Product,Outcome),collapse="-"))
    }else{
        FileName<-gsub(" ","_",paste0(c(Scenario,Practice,Product,Outcome),collapse="-"))
    }

    if(doN){
     FileName<-paste0("n-",FileName)
    }
        
    if((!any(grepl(FileName,list.files(SaveDir))))|overwrite){

            if(Scenario!="baseline"){
                ClimDir<-paste0(cimdir,"/",Year,"_",Scenario)
            }else{
                ClimDir<-paste0(cimdir,"/",Scenario)
            }
                
        IDs<-unique(Data[which(Data$PrName==Practice & Data$Product.Simple==Product & Data$Out.SubInd==Outcome &!is.na(Data$ID)),c("ID","RR","PC")])
          
        if(nrow(IDs)>0){
                  
        RR<-IDs$RR
        PC<-IDs$PC
        IDs<-IDs$ID
            
   
            simclim<-terra::rast(paste0(ClimDir,"/",IDs,".tif")) 
            simsol<-terra::rast(paste0(SoilDir,"/",IDs,".tif"))
            
            # Find minimum suitability of soil and climate
            allstk<-terra::rast(lapply(IDs,FUN=function(ID){
                simclim<-terra::rast(paste0(ClimDir,"/",ID,".tif"))
                simsol<-terra::rast(paste0(SoilDir,"/",ID,".tif"))
                min(c(simclim,simsol), na.rm=TRUE)
            }))  
        
       # If doN is TRUE then the function outputs a data availabity map of studies per Practice x Product x Outcome per pixel
       if(doN){
           
          simn<-function(x,y,minsim){
              x<-x[!x<minsim]
              return(length(x))
          }
           
           if(length(IDs)>1){
               sim_y<-terra::app(allstk,fun=simn,minsim=minsim)
           }else{
               sim_y<-allstk
               sim_y[sim_y<minsim]<-NA
               sim_y[!is.na(minsim)]<-1
           }
           
        }else{
        
         # x = similarity, y = value (e.g. RR or PC)
        simrank<-function(x,y,minsim){
            y<-y[!x<minsim]
            x<-x[!x<minsim]
            if(length(x)>0){
                ranks<-(1:length(x))[order(x,decreasing=T)]
                
                return(weighted.mean(y[ranks],x[ranks]))
            }else{
                return(NA)
            }
        }            
            
           if(length(IDs)>1){
               sim_y<-terra::app(allstk,fun=simrank,y=if(doRR){RR}else{PC},minsim=minsim)
          }else{
              sim_y<-allstk
              sim_y[sim_y<minsim]<-NA
          }
       }
            
            suppressWarnings(terra::writeRaster(sim_y,paste0(SaveDir,"/",FileName,"-",length(IDs),".tif"),overwrite=T))
            gc()
            FileName
        }
    }
}

    
