# File: STR_d_03b_addMERRA_training_daily.R
# Author: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 01/14/22
#
# Contents:
#  N. Notes
#  0. Import Packages and Set Global Objects
#  1. Read in Training Dataset & MERRA Predictions 
#  2. Combine MERRA and Training Dataset
#  3: Save Dataset

#### ------------------- ####
####       N. NOTES      ####
#### ------------------- ####

#### ------------------------------------------- ####
####  0. IMPORT PACKAGES AND SET GLOBAL OBJECTS  ####
#### ------------------------------------------- ####

# 0.a. import packages and global objects, if you haven't already done so
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

#### ------------------------------------------------- ####
####  1. READ IN TRAINING DATASET & MERRA PREDICTIONS  ####
#### ------------------------------------------------- ####

# 1.a. get training data
trainingData <- read_csv(here::here('inputs', 'pm25','training_datasets', 
                                    'daily_combined', 
                                    'training_avgscmcijscc_all.csv'))
# 1.b. filter to 2010
# so far, 2010 is the only year for which MERRA is available
trainingData <- trainingData %>%
  filter(year == 2010)

# 1.c. get conus bounding box
# 1.c.i bring in conus shapefile
conus <- sf::st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                                'conus.shp')) %>% 
  sf::st_transform(., crs=st_crs('epsg:4326'))
# 1.c.ii get the bounding box 
bbox.conus <- list(xMin = sf::st_bbox(conus)$xmin[[1]], 
                   xMax = sf::st_bbox(conus)$xmax[[1]], 
                   yMin = sf::st_bbox(conus)$ymin[[1]], 
                   yMax = sf::st_bbox(conus)$ymax[[1]])

# 1.d. get merra 
# 1.d.i declare the path where merra data is locations 
# all days are in one nc file
path.merra <- here::here('inputs', 'pm25','base_models', 'raw', 'merra_daily_raw', 
                   'daily2010adjPM25sum.nc')
# 1.d.ii define function to grab merra 
# we subtract the day index by 1 so that it starts at zero
grabMerra <- function(dayIndex) {
  
  # bring in the merra predictions for that day
  dta <- raster(path.merra, band = dayIndex)
  #conus <- raster::crop(dta, 
   #                     raster::extent(-124.8, -66.9, 24.4, 49.5))
  # crop to be within CONUS
  conus <- raster::crop(dta, 
                       raster::extent(bbox.conus$xMin, 
                                      bbox.conus$xMax,
                                      bbox.conus$yMin, 
                                      bbox.conus$yMax))
  # put in tidy dataframe
  conus.coords <- raster::coordinates(conus)
  conus.pm <- raster::extract(conus, conus.coords)
  conus.coords <- raster::coordinates(conus)
  dta1 <- rbind(tibble::tibble(lon = conus.coords[,"x"], 
                               lat = conus.coords[,"y"], 
                               pred = conus.pm, 
                               day_index = dayIndex -1)) %>% 
    na.omit()
}

# 1.d.iii get the merra for the various days
merra <- map_dfr(1:365, grabMerra)

# 1.e. fix date variable 
merra <- merra %>% 
  dplyr::mutate(day_date = lubridate::parse_date_time('01/01/2010', 'dmy') + 
                  lubridate::duration(days = day_index)) %>% 
  dplyr::mutate(year = lubridate::year(day_date), 
                month = lubridate::month(day_date), 
                day = lubridate::day(day_date)) %>% 
  dplyr::select(-day_index, -day_date) %>% 
  mutate(month = str_pad(month, 2, 'left', '0'), 
         day = str_pad(day, 2, 'left', '0'))

#### --------------------------------------- ####
####  2. COMBINE MERRA AND TRAINING DATASET  ####
#### --------------------------------------- ####
  
# 2.a. combine using spatiotemporal join 
# temporary fix - we ahve to call the aqs column obs_pm2_5 for now 

trainingData <- trainingData %>% rename(obs_pm2_5 = obs_pm25)

trainingData <- spatioTemporalJoin(refData = trainingData,
                                   modelData = merra,
                                   modelName = "MERRA",
                                   override = T,
                                   censusTractFile = NULL)

# 2.b. fix any bad variable names 
trainingData <- trainingData %>% 
  rename(obs_pm25 = obs_pm2_5, 
         pred_me = pred_MERRA) 

#### ----------------- ####
####  3: SAVE DATASET  ####
#### ----------------- ####

# 3.a. save the dataset with merra
trainingData %>%
  dplyr::select(lat, lon, day_index, year, month, day, obs_pm25, pred_av, pred_cm, pred_js, pred_me, ref_id) %>% 
  readr::write_csv(here::here('inputs', 'pm25', 'training_datasets', 'daily_yearly', 
                              paste0('training_avcmjsme_', 2010, '_all.csv')))

# 3a.b. save the training data without merra 
trainingData %>%
  dplyr::select(lat, lon, day_index, year, month, day, obs_pm25, pred_av, pred_cm, pred_js,  ref_id) %>% 
  readr::write_csv(here::here('inputs', 'pm25', 'training_datasets', 'daily_yearly', 
                              paste0('training_avcmjs_', 2010, '_all.csv')))
