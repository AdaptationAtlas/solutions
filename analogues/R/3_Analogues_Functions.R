#' Wrapper to run analogues function over ERA data for climate data
#' 
#' @export
#' @importFrom raster extract
 run_points_climate <- function(Index,Data, SaveDir, cores,wc_prec,wc_tmean,wc_prec_fut,wc_tmean_fut,Verbose=F) {
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
            