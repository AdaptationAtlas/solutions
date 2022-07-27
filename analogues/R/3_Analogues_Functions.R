#' Wrapper to run analogues function over ERA data for climate data
#' 
#' @export
#' @importFrom raster extract
 run_points_climate <- function(Index,Data, SaveDir, wc_prec,wc_tmean,wc_prec_fut,wc_tmean_fut,Verbose=F) {
     data <- Data[Index] 

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

         File<-paste0(SaveDir,"/",Data$ID[Index],".tif")

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
            }
        }
     }
 }        