#JRV - ERA analyses for adaptation atlas

#working directory
wd <- "~/work"
cimdir <- paste(wd,"/ERA_comparison",sep="")
soldir <- paste(wd,"/soilgrids_global/5km",sep="")
hzdir <- paste(wd,"/hazard_layers",sep="")

#analysis version
vr <- 3

#Practice (n) as follows
#   1. Reduced Tillage (47)
#   2. Mulch (42)
#   3. Agroforestry Pruning (38)
#   4. Green Manure (37)
#   5. Water Harvesting (35)
prname <- "Green Manure"

#load library
library(analogues)
library(rgdal)
library(raster)
library(maptools); data(wrld_simpl)
library(data.table)
library(Hmisc)
library(tidyverse)
library(ggplot2)
library(metR)

#read ERA data file
all_data <- read.csv(paste(cimdir,"/ERA_Test_Data.csv",sep=""))
all_data <- all_data[which(all_data$Product.Simple == "Maize"),]
all_data <- all_data[which(all_data$PrName %in% c("Mulch","Reduced Tillage","Green Manure","Water Harvesting","Agroforestry Pruning")),]
all_data <- droplevels(all_data)

#get Africa shapefile
sh_ctry <- readOGR(paste(wd,"/Africa_shp/African_continet.shp",sep=""))
sh_ctry <- spTransform(sh_ctry, crs("+proj=longlat +ellps=WGS84 +no_defs"))
sh_xt <- extent(sh_ctry)
sh_xt@xmin <- sh_xt@xmin-1; sh_xt@ymin <- sh_xt@ymin-1; sh_xt@xmax <- sh_xt@xmax+1; sh_xt@ymax <- sh_xt@ymax+1

#get worldclim climate data
for (tvar in c("prec","tmin","tmean","tmax")) {
  #tvar <- "prec"
  cat("downloading",tvar,"\n")
  if (!file.exists(paste(cimdir,"/wc_",tvar,".rda",sep=""))) {
    wc_data <- getData('worldclim', var=tvar, res=2.5, path=cimdir)
    wc_data <- crop(wc_data, sh_xt)
    wc_data <- readAll(wc_data)
    assign(paste("wc_",tvar,sep=""), wc_data)
    rm(wc_data)
    save(list=c(paste("wc_",tvar,sep="")), file=paste(cimdir,"/wc_",tvar,".rda",sep=""), compress="xz",compression_level=9)
    unlink(paste(cimdir,"/wc2-5",sep=""),recursive=T,force=T)
  }
}

#create a mask
load(paste(cimdir,"/wc_prec.rda",sep=""))
msk <- wc_prec[[1]]; rm(wc_prec)
msk[which(!is.na(msk[]))] <- 1

#prepare soil data, load and resample soil data
#sl1 (0 cm) - sl5 (60 cm)
#BLDFIE, CECSOL, CLYPPT, SNDPPT, SLTPPT ORCDRC, PHIHOX, AWCh3
#Note: CL used SOC, PH, CLAY, SAND
soilvars <- c("BLDFIE","CECSOL","CLYPPT","SNDPPT","SLTPPT","ORCDRC","PHIHOX","AWCh3")
sdepths <- paste("sl",1:5,sep="")
cat("loading soil data\n")
if (!file.exists(paste(cimdir,"/soils.rda",sep=""))) {
    soilstk <- list()
    for (svar in soilvars) {
        cat("processing soil variable=",svar,"\n")
        soilstk[[svar]] <- raster::stack(paste(soldir,"/",svar,"_M_",sdepths,"_5km_ll.tif",sep="")) %>%
                        raster::readAll(.) %>%
                        raster::crop(., msk) %>%
                        raster::resample(., msk)
    }
    save(list=c("soilstk"), file=paste(cimdir,"/soils.rda",sep=""),compress="xz",compression_level=9)
} else {
    load(paste(cimdir,"/soils.rda",sep=""))
}

#monthly dry days hazard data
hzrs_m <- stack(paste(hzdir,"/dry_days_hist/monthly/dry_days_1981_2019_",1:12,"_mean.tif",sep="")) %>%
            raster::readAll(.) %>%
            raster::crop(., msk) %>%
            raster::resample(., msk)
hzrs_v <- stack(paste(hzdir,"/dry_days_hist/monthly/dry_days_1981_2019_",1:12,"_cv.tif",sep="")) %>%
            raster::readAll(.) %>%
            raster::crop(., msk) %>%
            raster::resample(., msk)

#ppt driest quarter data
pdq_rs <- raster(paste(hzdir,"/ppt_driest_quarter/pdq_hist.tif",sep="")) %>%
            raster::readAll(.) %>%
            raster::crop(., msk)

#aridity index data
ai_rs <- raster(paste(hzdir,"/aridity/aridity_thornthwaite_hist.tif",sep="")) %>%
            raster::readAll(.) %>%
            raster::crop(., msk)

#chirps cv dta
chcv_rs <- raster(paste(hzdir,"/chirps_cv/cvr_africa.tif",sep="")) %>%
            raster::readAll(.) %>%
            raster::crop(., msk) %>%
            raster::resample(., msk)

#select practice and organize practice data
#aggregate data up to site level
rnd <- 4
agg_by <- c("Site.ID","PrName")

#Identify outliers
outlier_calc <- function(x){
    return((x < quantile(x)[2] - 3 *  IQR(x)  | x > quantile(x)[4] + 3 * IQR(x)))
}

#determine outliers
all_data <- as.data.table(all_data)
outliers <- unlist(all_data[,R:=1:nrow(all_data)][,list(outliers=list(R[outlier_calc(yi)])), by=agg_by][,outliers])

#Calculate Weighted mean effect size by practice and site combination
wt_grp <- unique(c("Code",agg_by))
data_sites <- all_data[!outliers
                      ][,N.Obs.Study:=.N,by=wt_grp # Recalculate Weightings by study within obervations grouping 
                       ][,Weight.Study:=(Rep^2/(2*Rep))/N.Obs.Study # Recalculate Weightings
                        ][,list(Observations=.N,
                                Studies=length(unique(Code)),
                                Sites=length(unique(Site.ID)),
                                Lat=mean(Latitude),
                                Lon=mean(Longitude),
                                RR=round(weighted.mean(yi,Weight.Study,na.rm=T),rnd),
                                RR.sd=suppressWarnings(round(abs(wtd.var(yi,Weight.Study,na.rm=T))^0.5,rnd))),
                          by=agg_by  # this 
                         ][,pc:=round(100*exp(RR)-100,rnd-1)
                          ][,pc.sd:=round(100*exp(RR.sd)-100,rnd-1)]
data_sites$Label <- paste(data_sites$Site.ID, data_sites$PrName)

#back to data.frame
data_sites <- as.data.frame(data_sites)

####
pdata <- data_sites[which(data_sites$PrName == prname),]
pdata <- pdata[which(!is.na(pdata$Lat)),]
pdata <- pdata[which(!is.na(pdata$Lon)),]
pdata <- droplevels(pdata)
pdata <- pdata[,c("Lon","Lat","RR")]

#cleanup any data points outside African continent
pdata$isvalid <- raster::extract(msk, pdata[,c("Lon","Lat")])
pdata <- pdata[which(!is.na(pdata$isvalid)),]
pdata$isvalid <- NULL

#positive and negative yield impact
pdata_p <- pdata[which(pdata$RR >= 0.15),]
pdata_p$ID <- paste("s",1:nrow(pdata_p),sep="")
pdata_n <- pdata[which(pdata$RR < 0.15),]
pdata_n$ID <- paste("s",1:nrow(pdata_n),sep="")

#output directory
pr_odir <- paste(cimdir,"/",gsub(" ", "_", prname, fixed=T),"_v",vr,sep="")
if (!file.exists(pr_odir)) {dir.create(pr_odir,recursive=T)}

#plot
png(paste(pr_odir,"/plot_data_points.png",sep=""),width=8,height=7,units='in',res=300,pointsize=12)
par(mar=c(5,5,1,1))
plot(pdata$Lon, pdata$Lat, pch=20, col="white")
plot(msk,add=T,legend=F)
plot(sh_ctry, add=T)
points(pdata_p$Lon, pdata_p$Lat, pch=20, col="blue", cex=1.5)
points(pdata_n$Lon, pdata_n$Lat, pch=20, col="red", cex=1.5)
dev.off()

#load climate data for calculation
load(paste(cimdir,"/wc_prec.rda",sep="")); wc_prec <- stack(wc_prec)
load(paste(cimdir,"/wc_tmean.rda",sep="")); wc_tmean <- stack(wc_tmean)

#calculate similarity (both temp and precip for positive sites)
#adjust so that for each site
#1. similarity for mean climate
#2. similarity for each soil variable
#3. similarity for hazard layer (mean and var)
#4. combine all in a sensible layer and save individual layers and output as .RData
run_points <- function(in_data, etype='pos') {
    allstk <- c()
    for (i in 1:nrow(in_data)) {
      #i <- 1
      cat("processing site=",i,"\n")
      if (!file.exists(paste(pr_odir,"/out_",etype,"_",in_data$ID[i],".RData",sep=""))) {
          #1. mean climate
          cat("...mean climate\n")
          par1 <- createParameters(x=in_data$Lon[i], y=in_data$Lat[i], vars=c("prec","tmean"),weights=c(0.75,0.25),
                                   ndivisions=c(12,12),growing.season=c(1,12),
                                   rotation="prec",threshold=1,env.data.ref=list(wc_prec,wc_tmean), 
                                   env.data.targ=list(wc_prec,wc_tmean),outfile=pr_odir,
                                   fname=NA,writefile=F)
          sim1 <- calc_similarity(par1)
          if (!inMemory(sim1)) {sim1 <- readAll(sim1)}

          #2. each soil variable, then combine in one by taking the median
          cat("...soils ORCDRC, PHIHOX, CLYPPT, SNDPPT\n")
          simsol <- c()
          for (svar in c("ORCDRC","PHIHOX","CLYPPT","SNDPPT")) {
              soil_data <- stack(soilstk[[svar]])
              parx <- createParameters(x=in_data$Lon[i], y=in_data$Lat[i], vars=c(svar),weights=1,
                                       ndivisions=c(5),growing.season=c(1,5),rotation="none",
                                       threshold=1,env.data.ref=list(soil_data), 
                                       env.data.targ=list(soil_data),outfile=pr_odir,
                                       fname=NA,writefile=F)
              simsol <- c(simsol,calc_similarity(parx))
          }
          sim2 <- stack(simsol) %>% min(., na.rm=TRUE)
          if (!inMemory(sim2)) {sim2 <- readAll(sim2)} 

          #3. similarity hazard layer (mean and variability)
          cat("...hazard layers\n")
          #a. mean
          cat("   ...mean\n")
          par3a <- createParameters(x=in_data$Lon[i], y=in_data$Lat[i], vars=c("tmean"),weights=1,
                                   ndivisions=c(12),growing.season=c(1,12),
                                   rotation="tmean",threshold=1,env.data.ref=list(stack(hzrs_m)), 
                                   env.data.targ=list(stack(hzrs_m)),outfile=pr_odir,
                                   fname=NA,writefile=F)
          sim3a <- calc_similarity(par3a)
          if (!inMemory(sim3a)) {sim3a <- readAll(sim3a)}

          #b. variability
          cat("   ...variability\n")
          par3b <- createParameters(x=in_data$Lon[i], y=in_data$Lat[i], vars=c("tmean"),weights=1,
                                   ndivisions=c(12),growing.season=c(1,12),
                                   rotation="tmean",threshold=1,env.data.ref=list(stack(hzrs_v)), 
                                   env.data.targ=list(stack(hzrs_v)),outfile=pr_odir,
                                   fname=NA,writefile=F)
          sim3b <- calc_similarity(par3b)
          if (!inMemory(sim3b)) {sim3b <- readAll(sim3b)}
          
          #c. ppt driest quarter
          cat("   ...prec driest quarter\n")
          par3c <- createParameters(x=in_data$Lon[i], y=in_data$Lat[i], vars=c("bio_1"),
                                    weights=c(1),ndivisions=c(1),growing.season=NA,
                                    rotation="none",threshold=1,env.data.ref=list(pdq_rs), 
                                    env.data.targ=list(pdq_rs),outfile=pr_odir,
                                    fname=NA,writefile=F)
          sim3c <- calc_similarity(par3c)
          if (!inMemory(sim3c)) {sim3c <- readAll(sim3c)}
          
          #d. aridity index
          cat("   ...aridity index\n")
          par3d <- createParameters(x=in_data$Lon[i], y=in_data$Lat[i], vars=c("bio_1"),
                                    weights=c(1),ndivisions=c(1),growing.season=NA,
                                    rotation="none",threshold=1,env.data.ref=list(ai_rs), 
                                    env.data.targ=list(ai_rs),outfile=pr_odir,
                                    fname=NA,writefile=F)
          sim3d <- calc_similarity(par3d)
          if (!inMemory(sim3d)) {sim3d <- readAll(sim3d)}
          
          #e. chirps cv
          cat("   ...chirps c.v.\n")
          par3e <- createParameters(x=in_data$Lon[i], y=in_data$Lat[i], vars=c("bio_1"),
                                    weights=c(1),ndivisions=c(1),growing.season=NA,
                                    rotation="none",threshold=1,env.data.ref=list(chcv_rs), 
                                    env.data.targ=list(chcv_rs),outfile=pr_odir,
                                    fname=NA,writefile=F)
          sim3e <- calc_similarity(par3e)
          if (!inMemory(sim3e)) {sim3d <- readAll(sim3e)}
          
          #calculate low limit
          cat("   ...aggregating\n")
          sim3 <- stack(c(sim3a, sim3b, sim3c, sim3d, sim3e)) %>% min(., na.rm=TRUE)
          if (!inMemory(sim3)) {sim3 <- readAll(sim3)}

          #4. combine all in a sensible layer and save individual layers and output as .RData
          #soil weight 50%, and climate indicators are divided equally over the remaining %
          #sim_i <- sim1 * 0.25 + sim2 * 0.5 + sim3 * 0.25
          cat("...minimum similarity across all factors\n")
          sim_i <- stack(c(sim1, sim2, sim3)) %>%
                      min(., na.rm=TRUE)
          if (!inMemory(sim_i)) {sim_i <- readAll(sim_i)}

          #5. put results in stack
          simstk <- stack(sim1, sim2, sim3, sim_i)
          names(simstk) <- c("meanclim","soil","hazard","overall")
          if (!inMemory(simstk)) {simstk <- readAll(simstk)}

          #6. save results
          cat("...saving\n")
          save(list=c("simstk"), 
               file=paste(pr_odir,"/out_",etype,"_",in_data$ID[i],".RData",sep=""),
               compress="xz",compression_level=9)

          #cleanup
          cat("...clean-up\n")
          rm(list=c("sim1","sim2","sim3a","sim3b","sim3c","sim3d","sim3e","sim3","soil_data"))
          gc(full=TRUE, verbose=FALSE)
      } else {
          cat("...already processed, hence loading\n")
          load(paste(pr_odir,"/out_",etype,"_",in_data$ID[i],".RData",sep=""))
      }
      allstk <- c(allstk, simstk[["overall"]])
      rm(simstk)
    }
    
    #maximum, minimum, and specific quantile values
    maxsim <- max(stack(allstk), na.rm=TRUE) %>%
                writeRaster(., paste(pr_odir,"/max_similarity_",etype,".tif",sep=""))
    minsim <- min(stack(allstk), na.rm=TRUE) %>%
                writeRaster(., paste(pr_odir,"/min_similarity_",etype,".tif",sep=""))
    lowsim <- calc(stack(allstk), fun=function(x) {quantile(x, probs=0.2, na.rm=TRUE)}) %>%
                writeRaster(., paste(pr_odir,"/lowlim_similarity_",etype,".tif",sep=""))
    uppsim <- calc(stack(allstk), fun=function(x) {quantile(x, probs=0.8, na.rm=TRUE)}) %>%
                writeRaster(., paste(pr_odir,"/upplim_similarity_",etype,".tif",sep=""))
    
    #cut similarity to growing areas
    #load harvested area
    aharv <- raster(paste(wd,"/spam2010V1r1/spam2010V1r1_global_H_MAIZ_A.tif",sep="")) %>%
                raster::crop(., maxsim) %>%
                raster::resample(., maxsim)
    aharv[which(aharv[] < 0.0001)] <- NA
    aharv[which(!is.na(aharv[]))] <- 1

    #mask
    if (!file.exists(paste(pr_odir,"/max_similarity_",etype,"_maizeareas.tif",sep=""))) {
        maxsim_m <- raster::mask(maxsim, aharv) %>%
                    writeRaster(., paste(pr_odir,"/max_similarity_",etype,"_maizeareas.tif",sep=""))
    } else {
        maxsim_m <- raster(paste(pr_odir,"/max_similarity_",etype,"_maizeareas.tif",sep=""))
    }
    
    #return object
    res <- raster::stack(maxsim, maxsim_m)
    return(res)
}

#run all positive points
simres_p <- run_points(pdata_p, etype="pos")

#pretty plot 
#first mask to Africa shapefile
rsp <- raster::mask(simres_p[[2]], sh_ctry)

#to points (for use of geom_tile)
rs_vls <- rasterToPoints(rsp) %>% 
  as_tibble() %>% 
  setNames(c('x', 'y', 'value'))

#plot
g1 <- ggplot(rs_vls)  +
    geom_tile(aes(x = x, y =  y, fill = value)) +
    scale_fill_divergent(low='tomato', 
                         mid = 'lemonchiffon', 
                         high = 'darkcyan', 
                         midpoint=0.5, 
                         na.value="white",
                         limits=c(0,1)) +
    coord_fixed(ratio=1,xlim = c(-20,60), ylim = c(-40,40)) +
    #coord_equal() +
    geom_polygon(data=sh_ctry, 
                aes(x=long, y=lat, group=group), 
                color="grey20", size=0.25, fill=NA) +
    theme_bw() +
    labs(x = 'Longitude', y = 'Latitude', fill = "Similarity", caption = 'Smallholder Adaptation Atlas') +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          strip.background = element_rect(color="white", fill="white"),
          strip.text = element_text(size=12, face = "bold"))
ggsave(plot = g1, filename = paste(pr_odir,"/plot_similarity_overall.png",sep=""), units = 'in', width = 9, height = 9, dpi = 300)


###
#run all negative points
simres_n <- run_points(pdata_n, etype="neg")

#pretty plot 
#first mask to Africa shapefile
rsp <- raster::mask(simres_n[[2]], sh_ctry)

#to points (for use of geom_tile)
rs_vls <- rasterToPoints(rsp) %>% 
    as_tibble() %>% 
    setNames(c('x', 'y', 'value'))

#plot
g1 <- ggplot(rs_vls)  +
    geom_tile(aes(x = x, y =  y, fill = value)) +
    scale_fill_divergent(low='tomato', 
                         mid = 'lemonchiffon', 
                         high = 'darkcyan', 
                         midpoint=0.5, 
                         na.value="white",
                         limits=c(0,1)) +
    coord_fixed(ratio=1,xlim = c(-20,60), ylim = c(-40,40)) +
    #coord_equal() +
    geom_polygon(data=sh_ctry, 
                aes(x=long, y=lat, group=group), 
                color="grey20", size=0.25, fill=NA) +
    theme_bw() +
    labs(x = 'Longitude', y = 'Latitude', fill = "Similarity", caption = 'Smallholder Adaptation Atlas') +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          strip.background = element_rect(color="white", fill="white"),
          strip.text = element_text(size=12, face = "bold"))
ggsave(plot = g1, filename = paste(pr_odir,"/plot_neg_similarity_overall.png",sep=""), units = 'in', width = 9, height = 9, dpi = 300)

#simple plots
plot(simres_p[[2]],zlim=c(0,1),breaks=seq(0,1,by=0.25),col=rev(terrain.colors(4)))
plot(sh_ctry, add=T)

