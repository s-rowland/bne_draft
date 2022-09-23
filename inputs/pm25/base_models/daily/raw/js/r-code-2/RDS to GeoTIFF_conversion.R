#####################################################################################
# Project: Convert PM2.5 rds to geotiff                                             
# Authors: Yaguang Wei1 & Carolynne Hultquist2                                        
#  1 Harvard T.H. Chan School of Public Health, 
#    Harvard University
#  2 Center for International Earth Science Information Network, 
#    Columbia University
#
#####################################################################################

############## 0. Setup ###############
rm(list=ls())
gc()

library(rgdal)
library(sp)
library(raster)
library(doParallel)

## paths to directories of raw rds daily and annual data - change as needed to the location of data
dir_rds_daily <- '~/Exposure modeling/PM2.5/Daily/'
dir_rds_annual <- '~/Exposure modeling/PM2.5/Annual/'

## list of the daily and annual rds files in those directories
grid_daily_files <- list.files(path=dir_rds_daily,pattern = "^PredictionStep2_PM25_USGrid_(.*)rds$")
grid_annual_files <- list.files(path=dir_rds_annual,pattern = "^PredictionStep2_Annual_PM25_USGrid_(.*)rds$")

## paths to empty folders to store daily and annual geotiff output data - create and direct to folders as needed
dir_tif_daily <- '~/convert_PM/PM25_raster/Daily/'
dir_tif_annual <- '~/convert_PM/PM25_raster/Annual/'


## load the geographic coordinates that associated with both the daily and annual data
SiteData <- readRDS(paste0(dir_rds_daily,"USGridSite.rds"))

## load an outline of US and transform to the same geographic projection 
USBuffer <- readOGR("/media/qnap3/Yaguang/convert_PM/shapefile/US.shp")
USBuffer <- spTransform(USBuffer,CRS("+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))

# create an empty raster object at 1km to the extent of the US buffer at the same geographic projection 
empty_raster <- raster(ext=extent(USBuffer), resolution=1000, crs="+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

################ 1. Convert daily files ################
cl = makeCluster(20,outfile='')
registerDoParallel(cl)
  
foreach(i = 1:length(grid_daily_files), .packages=c('rgdal','sp','raster')) %dopar%
{
  Data <- readRDS(paste0(dir_rds_daily,grid_daily_files[i]))
  SiteData$Value <- as.numeric(Data)
  SiteData_sp <-SpatialPointsDataFrame(data=SiteData,coords = cbind(SiteData$Lon, SiteData$Lat), proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  SiteData_sp <-spTransform(SiteData_sp, CRS("+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
  
  SiteData_raster<-rasterize(SiteData_sp,empty_raster, SiteData_sp$Value, fun = mean)
  SiteData_raster<-mask(SiteData_raster, USBuffer)
  SiteData_interp<-focal(SiteData_raster, w=matrix(1/8,nrow=3,ncol=3), NAonly = TRUE, na.rm=TRUE)
  
  writeRaster(SiteData_interp, filename=file.path(paste0(dir_tif_daily,substr(gsub("[^0-9.]", "", grid_daily_files[i]), start=4, stop=11),'.tif')), format="GTiff",overwrite=TRUE)
  
  SiteData$Value <- NULL
  rm(Data,SiteData_sp,SiteData_raster,SiteData_interp)
  gc()
  cat(i,substr(gsub("[^0-9.]", "", grid_daily_files[i]), start=4, stop=11),' ')
}
  
stopCluster(cl)
  


################ 2. Convert annual files ################
cl = makeCluster(20,outfile='')
registerDoParallel(cl)

foreach(i = 1:length(grid_annual_files), .packages=c('rgdal','sp','raster')) %dopar%
  {
    Data <- readRDS(paste0(dir_rds_annual,grid_annual_files[i]))
    SiteData$Value <- as.numeric(Data)
    SiteData_sp <-SpatialPointsDataFrame(data=SiteData,coords = cbind(SiteData$Lon, SiteData$Lat), proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    SiteData_sp <- spTransform(SiteData_sp, CRS("+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
    
    SiteData_raster<-rasterize(SiteData_sp,empty_raster, SiteData_sp$Value, fun = mean)
    SiteData_raster<-mask(SiteData_raster, USBuffer) 
    SiteData_interp<-focal(SiteData_raster, w=matrix(1/8,nrow=3,ncol=3), NAonly = TRUE, na.rm=TRUE)
    
    writeRaster(SiteData_interp, filename=file.path(paste0(dir_tif_annual,substr(gsub("[^0-9.]", "", grid_annual_files[i]), start=4, stop=11),'.tif')), format="GTiff",overwrite=TRUE)
    
    SiteData$Value <- NULL
    rm(Data,SiteData_sp,SiteData_raster,SiteData_interp)
    gc()
    cat(i,substr(gsub("[^0-9.]", "", grid_annual_files[i]), start=4, stop=11),' ')
  }

stopCluster(cl)


