# File: STR_d_03c_addMERRA_predictions_daily.R
# Author: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 01/14/22
#
# Contents:
#  N. Notes
#  0. Import Packages and Set Global Objects
#  1. Assign Daily MERRA

#### ------------------------------------------- ####
####  0. IMPORT PACKAGES AND SET GLOBAL OBJECTS  ####
#### ------------------------------------------- ####

# 0.a. import packages and global objects, if you haven't already done so
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

#### ----------------------- ####
####  1. ASSIGN DAILY MERRA  ####
#### ----------------------- ####

# 1.a declare the path where merra data is locations 
# all days are in one nc file
path <- here::here('inputs','pm25',  'base_models', 'raw', 'merra_daily_raw', 
                   'daily2010adjPM25sum.nc')

# 1.b. get conus bounding box
# 1.b.i bring in conus shapefile
conus <- sf::st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                                'conus.shp')) %>% 
  sf::st_transform(., crs=st_crs('epsg:4326'))
# 1.b.ii get the bounding box 
bbox.conus <- list(xMin = sf::st_bbox(conus)$xmin[[1]], 
                   xMax = sf::st_bbox(conus)$xmax[[1]], 
                   yMin = sf::st_bbox(conus)$ymin[[1]], 
                   yMax = sf::st_bbox(conus)$ymax[[1]])

# 1.c. function to assign MERRA
assignMerra <- function(dayIndex) {
  
  # 1.c.i. bring in the merra predictions for that day
  merra <- raster(path, band = dayIndex)
  # 1.c.ii. crop to be within CONUS
  conus <- raster::crop(merra, 
                        raster::extent(bbox.conus$xMin, 
                                       bbox.conus$xMax,
                                       bbox.conus$yMin, 
                                       bbox.conus$yMax))
  # 1.c.iii. put in tidy dataframe
  conus.coords <- raster::coordinates(conus)
  conus.pm <- raster::extract(conus, conus.coords)
  merra <- rbind(tibble::tibble(lon = conus.coords[,"x"], 
                               lat = conus.coords[,"y"], 
                               pred = conus.pm, 
                               day_index = dayIndex -1)) %>% 
    na.omit()
  
  # 1.c.vi. read in prediction dataset
  pred.dataset <- readr::read_csv(
    here::here('inputs', 'pm25', 'prediction_datasets', 'individual_daily',
               paste0('prediction_avcmjs_', 2010, '_', dayIndex, '_all.csv')))
  
  # 1.c.vii. join 
  # first convert to spatial
  merra.sf <- merra %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(refCRS)) 
  
  pred.dataset.sf <- pred.dataset %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(refCRS)) 
  # identify the nearest merra centroid for each prediction point
  modelIndices <- unlist(
    suppressMessages(
      nngeo::st_nn(pred.dataset.sf, merra.sf, k = 1)
    )
  )
  # add the meera preditions as a new column, based on those indicies
  pred.dataset$pred_me <- merra$pred[modelIndices]
    
  # rename some variables 
  pred.dataset <- pred.dataset %>% 
    dplyr::rename(pred_av = av_pred,
           pred_js = js_pred, 
           pred_cm = cmaq_outs_pred) %>% 
    dplyr::mutate(day_index = dayIndex, 
                  year = merra$year[1], 
                  month = merra$month[1], 
                  day = merra$day[1]) 
  
  # save result 
  pred.dataset %>%
    dplyr::select(lat, lon, day_index, year, month, day, pred_av, pred_cm, pred_js, pred_me) %>% 
    readr::write_csv(here::here('BNE_inputs', 'prediction_datasets', 'individual_daily', 
                                paste0('prediction_avcmjsme_', 2010, '_', dayIndex, '_all.csv')))
  
  pred.dataset %>%
    dplyr::select(lat, lon, day_index, year, month, day, pred_av, pred_cm, pred_js) %>% 
    readr::write_csv(here::here('BNE_inputs', 'prediction_datasets', 'daily_individual', 
                                paste0('prediction_avcmjs_', 2010, '_', dayIndex, '_all.csv')))
}


# actually do it.
merra <- map_dfr(1:365, assignMerra)

