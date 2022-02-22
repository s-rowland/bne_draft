# File: STR_d_02c_add_ak_training_preds.R
# Authors:
# Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 9/03/21
#
# Contents:
#  N. notes
#  0. Package Imports
#  1. general set up
#  2. Make Training and Prediction Data by Day
#  2A. process date variables
#  2B. bring in rk
#  2C: assign to aqs
#  2D: assign to preds
#  2E: save results

#### ------------------ ####
####       N. notes     ####
#### ------------------ ####

# jsut add the ak data. 
# If ak passess assessment, then we will add this code to the d_02a

#### ------------------ ####
#### 0. PACKAGE IMPORTS ####
#### ------------------ ####
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

#### ------------------- ####
####  1. general set up  ####
#### ------------------- ####

# 1.a. declare the AOI 
AOI <- 'conus'

# 1.b. specify the years and months we are interested in:
# just 2010 for now because we don't have the other MERRA available. 
years <- c(2010)
months <- stringr::str_pad(1:12, 2, "left", "0")
timeSteps <- expand.grid(yyyy = years, mm = months) %>%
  dplyr::arrange(yyyy, mm)

# 1.c. bring in aqs data
aqs <- read_csv(here::here('inputs', 'pm25', 'ground_truth', 'formatted', 
                           'lgc_daily_data_2000-2016_conus.csv'))

# 1.d. read in keys
# 1.d.i. AQS keys
key.AQS.rk <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                  'key_nn_aqsDaily_rkDaily.fst'))
# 1.d.ii refGridConus keys
# we need to arrange to make sure every key is in the same order
key.refGridConus.rk <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                           'key_nn_refGridConus_rkDaily.fst')) %>% 
  arrange(ref_lat) %>% 
  arrange(ref_lon)

# 1.e bring in training data with the other base models 
training <- read_csv(here::here('inputs', 'pm25', 'training_datasets', 
                                'daily_yearly', 'training_avcmjsmecb_2010_all.csv'))

training.list <- c()

#### --------------------------------------------- ####
####  2. Make Training and Prediction Data by Day  ####
#### --------------------------------------------- ####

for (julianDay in 1:365) {
  #julianDay <- 1 
  
  #### ---------------------------- ####
  ####  2A. process date variables  ####
  #### ---------------------------- ####
  
  # 2A.a. determine the date 
  activeDate <- parse_date_time('01/01/2010', 'dmy') + as.duration((julianDay-1)*60*60*24)
  # 2A.b. extract date components
  yyyy <- year(activeDate)
  mm <- pad0(month(activeDate))
  dd <- pad0(day(activeDate))
  
  #### ----------------- ####
  ####  2B. bring in rk  ####
  #### ----------------- ####
  
  # 2B.a. read in the raster for one month
  # note: we do not need to crop since this is a US-based model
  rk.ras <- ncdf4::nc_open(here::here('inputs', 'pm25', 'base_models', 'daily', 'rk', 
                                      paste0('cmaqout_', 
                                             yyyy, '-', mm, '-', dd, 
                                             '_pm25_daily_avg_lon_lat_UCAR_CONUS.nc')))
                                       #'2015-01-01_pm25_daily_avg_lon_lat_UCAR_CONUS.nc')))
  
  # 2B.b. create dataframe
  rk <- data.frame(
    pred_rk = as.vector(ncdf4::ncvar_get(rk.ras, 'pm25_daily_avg')), 
    lat = as.vector(ncdf4::ncvar_get(rk.ras, 'latitude')), 
    lon = as.vector(ncdf4::ncvar_get(rk.ras, 'longitude')))
  
  # check
  #a <- rk.df %>%  st_as_sf(coords = c('lon', 'lat')) %>% slice_sample(prop = 0.25)
  #ggplot(a) + geom_sf(aes(color = pred_rk, fill = pred_rk))
  
  # 2B.c close the netCDF object
  nc_close(rk.ras)
  
  #### ------------------- ####
  ####  2C: assign to aqs  ####
  #### ------------------- ####
  
  # 2C.a. isolate aqs to day of interest 
  training.day <- training %>% 
   dplyr::filter(year == yyyy & month == pad0(mm) & day == pad0(dd))
  
  # 2C.b. keep only the rk preds at potentially relevant locations
  rk.atAqs <- rk %>% 
   filter(row_number() %in% key.AQS.rk$baseModel_id)
  
  # 2C.c. assign nearest rk pred to each aqs 
  # make simple features
  training.day.sf <- training.day %>% 
   sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(plotCRS)) %>% 
  st_transform(crs= projCRS)
  rk.atAqs.sf <- rk.atAqs %>% 
   sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(projCRS)) 
  # 2C.c.ii. determine nearest js prediction
  rk.aqs.indices <- unlist(nngeo::st_nn(training.day.sf, rk.atAqs.sf, k=1))
  # 2C.c.iii. assign js predictions
  training.day <- rk.atAqs %>% 
   slice(rk.aqs.indices) %>% 
  dplyr::select(pred_rk) %>%
  bind_cols(training.day)
  
  # code to confirm that it works
  #js.atAqs.sf <- js.atAqs.sf %>% 
  #  st_transform(crs= plotCRS)
  #aqs.day <- js.atAqs %>% 
  #  mutate(lat2 = st_coordinates(js.atAqs.sf)[,2], 
  #         lon2 = st_coordinates(js.atAqs.sf)[,1]) %>%
  # slice(js.aqs.indices) %>% 
  #  dplyr::select(-lat, -lon) %>%
  #  bind_cols(aqs.day)
  
  # 2C.d. add it to the list
  training.list[julianDay] <- list(training.day)
  
  #### --------------------- ####
  ####  2D: assign to preds  ####
  #### --------------------- ####
  
  # 2D.a. readin preds for that day 
  preds.day <- read_fst(here::here('inputs', 'pm25', 'prediction_datasets', 
                                      'daily_individual_str', 
                                      paste0('preds_avjscmme_2010_', str_pad(julianDay, 3, 'left', '0'), '.fst'))) %>% 
    arrange(lat) %>% 
    arrange(lon)
  
  # 2D.b. assign the rk predictions
  preds.day <- rk %>% 
    slice(key.refGridConus.rk$baseModel_id) %>% 
    dplyr::select(-lat, -lon) %>%
    bind_cols(preds.day)
  
  # check
  #refGridConus <- rk %>%
  # slice(key.refGridConus.rk$baseModel_id) %>% 
  # rename(lat_js = lat, lon_js = lon) %>% 
  # bind_cols(preds.day)
  
  #### ------------------ ####
  ####  2E: save results  ####
  #### ------------------ ####
  
  refGridConus.day %>% 
    rename(lat = ref_lat, lon = ref_lon) %>%
    write_fst(here::here('inputs', 'pm25', 'prediction_datasets', 
                         'daily_individual_str', 
                         paste0('preds_avjscmmerk_2010_', str_pad(julianDay, 3, 'left', '0'), '.fst')))
  # 
  print(julianDay)
}
