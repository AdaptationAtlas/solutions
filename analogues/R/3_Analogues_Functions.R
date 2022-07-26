#' Wrapper to run analogues function over ERA data for climate data
#' 
#' @export
#' @importFrom raster extract
 run_points_climate <- function(Index,Data, cimdir, Year, Scenario, vr,cores,wc_prec,wc_tmean,wc_prec_fut,wc_tmean_fut) {
                   
        data <- Data[Index] 
                   
        #output directory  =====
          if(Scenario=="baseline"){
                pr_odir <- paste0(cimdir,"v",vr,"/Climate/",gsub("[.]","_",Scenario))
            }else{
                pr_odir <- paste0(cimdir,"v",vr,"/Climate/",Year,"/",gsub("[.]","_",Scenario))
              }
     
        if (!file.exists(pr_odir)) {dir.create(pr_odir,recursive=T)}
        
        #verbose what i'm running
        cat('\r                                 ')
        cat("Processing ", Index, "of", nrow(Data))
        flush.console()

        #loop through points 
     if(!file.exists(paste0(pr_odir,"/",Data$ID[Index],".tif"))){
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
                                            outfile=pr_odir,
                                            fname=Data$ID[Index],
                                            writefile=F)
        X<-calc_similarity(par1)
        terra::writeRaster(X,paste0(pr_odir,"/",Data$ID[Index],".tif"),overwrite=T)
         }
}
            