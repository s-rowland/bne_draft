# File: STR_d_03a_rename_daily_datasets.R
# Author: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 01/14/22
#
# Contents:
#  N. Notes
#  0. Import Packages and Set Global Objects
#  1. Get CONUS Bounding Box
#  2. Wrangle Training Datasets
#  3: Wrangle Prediction Datasets

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

#### --------------------------- ####
####  1. GET CONUS BOUNDING BOX  ####
#### --------------------------- ####

# 1.a. bring in conus shapefile
conus <- sf::st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                                'conus.shp')) %>% 
  sf::st_transform(., crs=st_crs('epsg:4326'))

# 1.b. get the bounding box 
bbox.conus <- list(xMin = sf::st_bbox(conus)$xmin[[1]], 
                   xMax = sf::st_bbox(conus)$xmax[[1]], 
                   yMin = sf::st_bbox(conus)$ymin[[1]], 
                   yMax = sf::st_bbox(conus)$ymax[[1]])

#### ------------------------------ ####
####  2. WRANGLE TRAINING DATASETS  ####
#### ------------------------------ ####

# 2.a. get training data
trainingData <- read_csv(here::here('inputs', 'pm25','training_datasets', 
                                    'daily_combined', 
                                    'dailyTrainingData_2010-2015.csv'))

# 2.b. filter to conus
trainingData <- trainingData %>% 
  dplyr::filter(lon > bbox.conus$xMin & 
                  lon < bbox.conus$xMax & 
                  lat > bbox.conus$yMin & 
                  lat < bbox.conus$yMax)

# 2.c. rename columns 
trainingData <- trainingData %>% 
  rename(obs_pm25 = obs_pm2_5, 
         pred_av = av_pred, 
         pred_gs = gs_pred, 
         pred_ci = cmaq_ins_pred,
         pred_cm = cmaq_outs_pred, 
         pred_js = js_pred, 
         pred_cc = caces_pred)

# 2.d. add day_index variable 
trainingData <- trainingData %>% 
  mutate(day_index = as.numeric(as.duration(as.interval(
    parse_date_time('01/01/2010', 'dmy'), 
    parse_date_time(paste0(day, '/', month, '/', year), 'dmy')
  )), 'days')) 

# 2.e. save datasets
trainingData %>% 
  dplyr::select(lat, lon, day_index, year, month, day, obs_pm25, pred_av, pred_gs, 
                pred_cm, pred_ci, pred_js, pred_cc, ref_id) %>% 
  write_csv(here::here('inputs', 'pm25','training_datasets', 
                           'daily_combined', 
                           'training_avgscmcijscc_all.csv'))
trainingData %>% 
  dplyr::select(lat, lon, day_index, year, month, day, obs_pm25, pred_av, pred_gs, 
                pred_cm, pred_js, pred_cc, ref_id) %>% 
  write_csv(here::here('inputs', 'pm25','training_datasets', 
                       'daily_combined', 
                       'training_avgscmjscc_all.csv'))

trainingData %>% 
  dplyr::select(lat, lon, day_index, year, month, day, obs_pm25, pred_av, pred_cm,
                pred_js, ref_id) %>% 
  write_csv(here::here('inputs', 'pm25','training_datasets', 
                       'daily_combined', 
                       'training_avcmjs_all.csv'))

#### -------------------------------- ####
####  3. WRANGLE PREDICTION DATASETS  ####
#### -------------------------------- ####

# 3.a. function to wrangle one day
renamePredictionDataset <- function(dayIndex) {
  
  # 3.a.i. figure out the date components from the DayIndex
  dayIndexDate <- lubridate::parse_date_time('01/01/2010', 'dmy') + 
    lubridate::duration(days = dayIndex)
  
  # 3.a.ii. read in prediction
  pred.dataset <- readr::read_csv(here::here('inputs', 'pm25',  'prediction_datasets', 'daily_individual',
                                             'dailyPredictionsData_2010-2015',
                                             paste0(year(dayIndexDate), '-',
                                                    pad0(month(dayIndexDate)), 
                                                    '-', pad0(day(dayIndexDate)), '.csv')))
  # 3.a.iii. isolate to conus 
  pred.dataset <- pred.dataset %>%
     dplyr::filter(lon > bbox.conus$xMin & 
                  lon < bbox.conus$xMax & 
                  lat > bbox.conus$yMin & 
                  lat < bbox.conus$yMax)
  
  # 3.a.iv. rename columns 
  pred.dataset <- pred.dataset %>% 
    rename(pred_av = av_pred, 
           pred_gs = gs_pred, 
           pred_ci = cmaq_ins_pred,
           pred_cm = cmaq_outs_pred, 
           pred_js = js_pred, 
           pred_cc = caces_pred) 
  
  # 3.a.v. add column for day index 
  pred.dataset <- pred.dataset %>% 
    mutate(day_index = dayIndex)
  
  # 3.a.vi shrink number of observations 
  # we actually only need at 60k observations for conus unless we are making the 
  # final product
  pred.dataset <- pred.dataset %>% 
    slice_sample(prop = 0.6)
  
  # 3.a.vii save result 
  # here we restrict to only the base models we will actually use. 
  pred.dataset %>%
    dplyr::select(lat, lon, day_index, pred_av, pred_cm, pred_js) %>% 
    readr::write_csv(here::here('inputs', 'pm25', 'prediction_datasets', 'daily_individual', 
                                paste0('prediction_avcmjs_', 2010, '_',
                                       dayIndexString <- stringr::str_pad(dayIndex, 3, 'left', '0'), 
                                       '_all.csv')))
  
}

# 3.b. determine the max day index for our 2010: 2015 study period
dayIndex.max <- interval(lubridate::parse_date_time('01/01/2010', 'dmy'), 
         lubridate::parse_date_time('31/12/2016', 'dmy')) /duration(1, units = 'days')

# 3.c. now wrangle the data
dta <- map(0:dayIndex.max, renamePredictionDataset)

