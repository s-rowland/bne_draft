# File: STR_d_02_make_training_predictions_JS.R
# Authors:
# Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 03/17/23
#
# Contents:
#  N. notes
#  0. Package Imports
#  1. general set up
#  2. wrangle base models
#  3. make training and prediction Ddta by day
#  3A. process date variables and aqs
#  3B. add me
#  3C. save datasets
#  4. save training dataset

#### ------------------ ####
####       N. notes     ####
#### ------------------ ####


#yyyy <- 2010


#### -------------- ####
#### 0. preparation ####
#### -------------- ####

# 0.a. load packages, etc
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}



for (yyyy in 2010:2015) {
# 0.b set up parallelization
### code to paralized the processing among multiple cores
cores <- ceiling(parallel::detectCores() -2) # use half of the available cores
myCluster <- snow::makeCluster(cores)
doSNOW::registerDoSNOW(myCluster)


#### ------------------- ####
####  1. general set up  ####
#### ------------------- ####

# 1.g. get conus bounding box
# 1.g.i bring in conus shapefile
conus <- sf::st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                                'conus.shp')) %>% 
  sf::st_transform(., crs=st_crs('epsg:4326'))
# 1.g.ii get the bounding box 
bbox.conus <- list(xMin = sf::st_bbox(conus)$xmin[[1]], 
                   xMax = sf::st_bbox(conus)$xmax[[1]], 
                   yMin = sf::st_bbox(conus)$ymin[[1]], 
                   yMax = sf::st_bbox(conus)$ymax[[1]])

# 1.h. bring in daily merra caps 
activeCap <- paste0('cap_', yyyy)
me_caps <- read_csv(here::here('ancillary_data', 'formatted', 'processing_support', 
                     'me_daily_caps.csv')) 
me_caps <- me_caps %>% 
  rename(cap := !!activeCap) %>% 
  arrange(as.numeric(lat)) %>%
  arrange(as.numeric(lon) )
rm(activeCap)

#me_caps <- me_caps %>%
#  sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326"))%>% 
#  sf::st_transform(crs=sf::st_crs(projCRS))

#me_caps2 <- me_caps %>% filter(is.na(cap)) %>% mutate(region = 5) %>%dplyr::select(region)
#plot(me_caps2)


#### ------------------------ ####
####  2. wrangle base models  ####
#### ------------------------ ####


# 2.b. declare the path where merra data is locations 
# all days are in one nc file
path.merra <- here::here('inputs','pm25',  'base_models', 'daily', 'raw', 'me', 
                        paste0('daily', yyyy, 'adjPM25sum_v2.nc'))


# 0.c. strings for js download 
prefix <- "https://beta.sedac.ciesin.columbia.edu/downloads/data/aqdh/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016-"
suffix <- "-rds.zip"
wd <- paste0(here::here('inputs', 'pm25', 'base_models', 'daily', 'raw', 'js'), '/')

#### --------------------------------------------- ####
####  3. Make Training and Prediction Data by Day  ####
#### --------------------------------------------- ####

# 3.a. set up loop


maxDoY <- 365 
if (yyyy %in% c(2004, 2008, 2012, 2016)) {maxDoY <- 366}
  
# 0.c progress bar
pb <- txtProgressBar(min = 0, max = 10, width = 25, style = 3)
progress <- function(p) setTxtProgressBar(pb, p)
opts <- list(progress=progress)

Sys.time()
me.capped <- foreach(
  dayOfYear = 1:maxDoY, 
  .combine = 'rbind',
  .options.snow = opts, 
  .inorder = FALSE,
  .export = ls(globalenv()),
  .packages = c('dplyr', 'magrittr')
  
) %dopar% {
  
  #### ---------------------------- ####
  ####  3A. process date variables  ####
  #### ---------------------------- ####
  
  # 3A.a determine the date 
  activeDate <- lubridate::parse_date_time(paste0('01/01/', yyyy), 'dmy') + 
    lubridate::as.duration((dayOfYear-1)*60*60*24)
  
  # 3A.b. extract date components
  yyyy <- lubridate::year(activeDate)
  mm <- pad0(lubridate::month(activeDate))
  dd <- pad0(lubridate::day(activeDate))
  
  #### ------------ ####
  ####  3C. add me  ####
  #### ------------ ####
  
  # 3C.a. bring in the merra predictions for that day
  me <- raster::raster(path.merra, band = as.numeric(dayOfYear))# %>%
  #projectRaster(crs ='+init=epsg:4326')
  
  # 3C.b crop to be within CONUS
  me.conus <- raster::crop(me, 
                           raster::extent(bbox.conus$xMin, 
                                          bbox.conus$xMax,
                                          bbox.conus$yMin, 
                                          bbox.conus$yMax))
  # 3C.c put in tidy dataframe
  me.conus.coords <- raster::coordinates(me.conus)
  me <- rbind(tibble::tibble(lon = me.conus.coords[,"x"], 
                             lat = me.conus.coords[,"y"], 
                             pred_me = raster::extract(me.conus, me.conus.coords))) %>% 
    na.omit()
  
  
  #me <- me %>% 
   # dplyr::arrange(as.numeric(desc(lat))) %>%
  #  dplyr::arrange(as.numeric(lon) )
  
  me <- me %>%
    dplyr::bind_cols(dplyr::select(me_caps, cap)) %>% 
    dplyr::mutate(pred_me = if_else(pred_me > cap, cap, pred_me)) %>% 
    dplyr::select(-cap)
  # return capped me
  me
}


#### ---------------- ####
####  4. final stuff  ####
#### ---------------- ####

me.capped.mean  <- me.capped %>% 
  group_by(lat, lon) %>% 
  summarize(pred_me = mean(pred_me))


#me2 <- me.capped.mean %>% 
 # sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326"))
me.capped.mean %>% 
  write_csv(here::here('inputs', 'pm25', 'base_models', 'annual', 'formatted', 
                       'me', paste0('annual_me_', yyyy, '.csv')))

# 4.b. close out cluster 
close(pb)
#stopCluster(my.cluster)


}
