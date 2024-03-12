# a) Install and load packages ####
load_and_install_packages <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package)
      library(package, character.only = TRUE)
    }
  }
}

# List of packages to be loaded
packages <- c("terra", 
              "data.table", 
              "exactextractr",
              "s3fs",
              "sf", 
              "sfarrow", 
              "pbapply",
              "arrow",
              "httr")

# Call the function to install and load packages
load_and_install_packages(packages)


# b) Create functions & wrappers ####

# Create extraction by geography function
admin_extract<-function(data,Geographies,FUN="mean",max_cells_in_memory=3*10^7){
  output<-list()
  if("admin0" %in% names(Geographies)){
    data0<-exactextractr::exact_extract(data,sf::st_as_sf(Geographies$admin0),fun=FUN,append_cols=c("admin_name","admin0_name","iso3"),max_cells_in_memory=max_cells_in_memory)
    data0<-terra::merge(Geographies$admin0,data0)
    output$admin0<-data0
  }
  
  if("admin1" %in% names(Geographies)){
    data1<-exactextractr::exact_extract(data,sf::st_as_sf(Geographies$admin1),fun=FUN,append_cols=c("admin_name","admin0_name","admin1_name","iso3"),max_cells_in_memory=max_cells_in_memory)
    data1<-terra::merge(Geographies$admin1,data1)
    output$admin1<-data1
  }
  
  if("admin2" %in% names(Geographies)){
    data2<-exactextractr::exact_extract(data,sf::st_as_sf(Geographies$admin2),fun=FUN,append_cols=c("admin_name","admin0_name","admin1_name","admin2_name","iso3"),max_cells_in_memory=max_cells_in_memory)
    data2<-terra::merge(Geographies$admin2,data2)
    output$admin2<-data2
  }
  
  return(output)
}

# c) Create directories ####
analogues_dir<-"data/results_mean_class_crop5"

analogues_ex_dir<-paste0(analogues_dir,"_ex")
if(!dir.create(analogues_ex_dir)){
  dir.create(analogues_ex_dir)
}

# d) Load datasets ####
  # 1) Geographies #####
  # Load and combine geoboundaries
  overwrite<-F
  
  geoboundaries_s3<-"s3://digital-atlas/boundaries"
  geo_files_s3<-s3fs::s3_dir_ls(geoboundaries_s3)
  geo_files_s3<-grep("harmonized.gpkg",geo_files_s3,value=T)
  
  geo_files_local<-file.path("data/boundaries",basename(geo_files_s3))
  names(geo_files_local)<-c("admin0","admin1","admin2")
  
  Geographies<-lapply(1:length(geo_files_local),FUN=function(i){
    file<-geo_files_local[i]
    if(!file.exists(file)|overwrite==T){
      s3fs::s3_file_download(path=geo_files_s3[i],new_path=file,overwrite = T)
    }
    data<-terra::vect(file)
    names(data)<-gsub("_nam$","_name$",names(data))
    data
    
  })
  names(Geographies)<-names(geo_files_local)
  
  # 2) Base raster #####
  # Load base raster to resample to 
  base_raster<-"data/base_raster.tif"
  if(!file.exists(base_raster)){
    url <- "https://raw.githubusercontent.com/AdaptationAtlas/hazards_prototype/main/metadata/base_raster.tif"
    httr::GET(url, httr::write_disk(base_raster, overwrite = TRUE))
  }
  base_rast<-terra::rast(base_raster)
  
  # 3) AEZ #####
  # AEZ data is downloaded from https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/M7XIUB/PW7APO&version=3.1
  aez_dir<-"data/aez"
  aez<-terra::rast(list.files(aez_dir,"CRUTS32",full.names = T))
  aez<-terra::resample(aez,base_rast,method="near")
  aez_cats<-levels(aez)

  # 4) Analogues ####
  analogues_files<-list.files(analogues_dir,".tif$",full.names = T)
  
  analogues<-pbapply::pblapply(1:length(analogues_files),FUN=function(i){
    x<-terra::rast(analogues_files[i])
    x_cat<-terra::cats(x)[[1]]
    x_cat[6,1]<--9999
    levels(x)<-x_cat[,c("value","Lab")]
    x
  })
  
  analogues<-terra::rast(analogues)
  
  names(analogues)<-gsub("[.]tif","",basename(analogues_files))

# e) Extract aez by admin ####
  aez_ex<-exactextractr::exact_extract(aez,sf::st_as_sf(Geographies$admin0),include_area=T)
  
  aez_ex<-pbapply::pblapply(1:length(aez_ex),FUN=function(i){
    x<-data.table(aez_ex[[i]])
    x$admin_name<-Geographies$admin0$admin_name[i]
    x$admin_name0<-Geographies$admin0$admin0_name[i]
    x$total_area<-sum(x$area)
    x
  })
  
  aez_ex<-rbindlist(aez_ex)
  
  aez_ex<-aez_ex[,list(prop=sum(area)/total_area[1]),by=list(value,admin_name,admin_name0)]
  aez_ex<-merge(aez_ex,aez_cats,all.x=T)
  
# f) Extract analogues by admin ####
  analogues_ex<-exactextractr::exact_extract(analogues,sf::st_as_sf(Geographies$admin0),include_area=T)
  
  analogues_ex<-rbindlist(pbapply::pblapply(1:length(analogues_ex),FUN=function(i){
    x<-data.table(analogues_ex[[i]])
    x$admin_name<-Geographies$admin0$admin_name[i]
    x$admin_name0<-Geographies$admin0$admin0_name[i]
    x<-x[,!"coverage_fraction"]
    x
  }))
  
  analogues_ex<-melt(analogues_ex,id.vars=c("admin_name","admin_name0","area"))
  analogues_ex[!is.na(value),total_area:=sum(area),by=list(variable,admin_name,admin_name0)]
  analogues_ex<-analogues_ex[,list(area=sum(area)),by=list(admin_name,admin_name0,total_area,variable,value)]
  analogues_ex[,prop:=area/total_area]
  
# g) Extract analogues by admin and aez ####
  # combine aez and admin area
    # rasterize admin
    admin_arleea<-terra::rasterize(Geographies$admin0,base_rast,field="iso3")
    admin_area2<-admin_area
    # Add two zeros 
    admin_area2[]<-as.numeric(paste0(as.character(admin_area[]),"00"))
    # Add admin number to aez number
    admin_aez<-admin_area2+aez
    # Convert to vector
    admin_aez<-terra::as.polygons(admin_aez)
    
    # Extraction analogues by admin & aez
    analogues_aez_ex<-exactextractr::exact_extract(analogues,sf::st_as_sf(admin_aez),include_area=T)
    
    analogues_aez_ex<-rbindlist(pbapply::pblapply(1:length(analogues_aez_ex),FUN=function(i){
      x<-data.table(analogues_aez_ex[[i]])
      x$code<-admin_aez$iso3[i]
      x<-x[,!"coverage_fraction"]
      x
    }))
    
    analogues_aez_ex<-melt(analogues_aez_ex,id.vars=c("code","area"))
    
    analogues_aez_ex[,code_aez:=as.numeric(unlist(tstrsplit(format(code[1]/100, nsmall=2),"[.]",keep=2))),by=code]
    analogues_aez_ex[,code_admin:=as.numeric(unlist(tstrsplit(format(code[1]/100, nsmall=2),"[.]",keep=1))),by=code]
    
    analogues_aez_ex<-merge(analogues_aez_ex,levels(admin_area)[[1]],all.x=T,by.x="code_admin",by.y="ID")
    analogues_aez_ex<-merge(analogues_aez_ex,levels(aez)[[1]],all.x=T,by.x="code_aez",by.y="value")
    analogues_aez_ex[,code_admin:=NULL][,code_aez:=NULL][,code:=NULL]
    setnames(analogues_aez_ex,"category","aez")
    
    analogues_aez_ex[!is.na(value),total_area_admin:=sum(area),by=list(variable,iso3)]
    analogues_aez_ex[!is.na(value),total_area_admin_aez:=sum(area),by=list(variable,iso3,aez)]
    
    analogues_aez_ex<-analogues_aez_ex[,list(area=sum(area)),by=list(iso3,aez,total_area_admin,total_area_admin_aez,variable,value)]

    # Add back analogues category label
    analogues_aez_ex<-merge(analogues_aez_ex,levels(analogues[[1]])[[1]],all.x=T)
    
    # Remove unneeded value col
    analogues_aez_ex[,value:=NULL]
    
    # Split variable name into component columns
      # Replace baseline name
      analogues_aez_ex[,variable:=gsub("baseline","historic-historic",variable)]
      
      # Replace "-" with "+" for combined practices
      analogues_aez_ex[,variable:=gsub("Mulch-Reduced","Mulch+Reduced",variable)]
      
      split<-analogues_aez_ex[,list(var_split=list(tstrsplit(variable[1],"-"))),by=variable]
      split_tab<-rbindlist(split$var_split)
      names(split_tab)<-c("timeframe","scenario","solution","crop","outcome","studies")
      split_tab$variable<-split$variable
      
    analogues_aez_ex<-merge(analogues_aez_ex,split_tab,all.x=T)
    analogues_aez_ex[,variable:=NULL]
  
    # add admin columns
    analogues_aez_ex<-merge(analogues_aez_ex,data.frame(Geographies$admin0),all.x=T)[,iso3:=NULL]
    
    file<-file.path(analogues_ex_dir,"analogues_ex_aez_admin.parquet")
  
  