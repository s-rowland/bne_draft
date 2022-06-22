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
#  3. make training and prediction data by day
#  3A. process date variables and aqs
#  3B. add av 
#  3C. add cb
#  3D. add cc
#  3E. add cm 
#  3F. add gs
#  3G. add js 
#  3H. add me
#  3I. add rk
#  3J. save datasets
#  4. save training dataset

#### ------------------ ####
####       N. notes     ####
#### ------------------ ####


#### -------------- ####
#### 0. preparation ####
#### -------------- ####

# 0.a. load packages, etc
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

# 1.b. bring in aqs data
training <- fst::read_fst(here::here('inputs', 'pm25', 'ground_truth', 'formatted',
                                'aqs_annual_curated.fst'))
# 1.c. read in keys
# 1.c.i. AQS keys
key.aqs.av <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                  'key_nn_aqsAnnual_avAnnual.fst'))
# add cb
key.aqs.cc <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                  'key_nn_aqsAnnual_ccAnnual.fst'))
key.aqs.cm10 <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                  'key_nn_aqsAnnual_cm10Annual.fst'))
key.aqs.cm15 <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                    'key_nn_aqsAnnual_cm15Annual.fst'))
key.aqs.gs <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                  'key_nn_aqsAnnual_gsAnnual.fst'))
key.aqs.js <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                  'key_nn_aqsAnnual_jsAnnual.fst'))
key.aqs.me <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                  'key_nn_aqsAnnual_meAnnual.fst'))
key.aqs.rk <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                  'key_nn_aqsAnnual_rkAnnual.fst'))
# 1.c.ii. refGridConus keys
# we need to arrange to make sure every key is in the same order
key.refGridConus.av <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                           'key_nn_refGridConus_avAnnual.fst')) %>% 
  arrange(ref_lat) %>% 
  arrange(ref_lon)
key.refGridConus.cc <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                           'key_nn_refGridConus_ccAnnual.fst')) %>% 
  arrange(ref_lat) %>% 
  arrange(ref_lon)
key.refGridConus.cm10 <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                           'key_nn_refGridConus_cm10Annual.fst')) %>% 
  arrange(ref_lat) %>% 
  arrange(ref_lon)
key.refGridConus.cm15 <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                           'key_nn_refGridConus_cm15Annual.fst')) %>% 
  arrange(ref_lat) %>% 
  arrange(ref_lon)

key.refGridConus.gs <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                           'key_nn_refGridConus_gsAnnual.fst')) %>% 
  arrange(ref_lat) %>% 
  arrange(ref_lon)
key.refGridConus.js <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                           'key_nn_refGridConus_jsAnnual.fst')) %>% 
  arrange(ref_lat) %>% 
  arrange(ref_lon)
key.refGridConus.me <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                           'key_nn_refGridConus_meAnnual.fst')) %>% 
  arrange(ref_lat) %>% 
  arrange(ref_lon)
key.refGridConus.rk <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                           'key_nn_refGridConus_rkAnnual.fst')) %>% 
  arrange(ref_lat) %>% 
  arrange(ref_lon)

# 1.d. generate refgrid 
preds <- key.refGridConus.js %>% 
  dplyr::select(ref_lat, ref_lon) %>% 
  arrange(ref_lat) %>% 
  arrange(ref_lon)

# 1.f. get conus bounding box
# 1.f.i bring in conus shapefile
conus <- sf::st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                                'conus.shp')) %>% 
  sf::st_transform(., crs=st_crs('epsg:4326'))
# 1.f.ii get the bounding box 
bbox.conus <- list(xMin = sf::st_bbox(conus)$xmin[[1]], 
                   xMax = sf::st_bbox(conus)$xmax[[1]], 
                   yMin = sf::st_bbox(conus)$ymin[[1]], 
                   yMax = sf::st_bbox(conus)$ymax[[1]])

#### ------------------------ ####
####  2. wrangle base models  ####
#### ------------------------ ####

# 2.a. bring in gs
# the one gs file contains data for all years
gs.allYears <- loadData(path =  here::here('inputs', 'pm25', 'base_models', 'annual', 'raw', 
                                  'gs', 'GBD2016_PREDPOP_FINAL.RData'), 
               dataset = 'GS_annual') %>% 
  rename(pred_gs = pred)

#### --------------------------------------------- ####
####  3. Make Training and Prediction Data by Day  ####
#### --------------------------------------------- ####


# 0.b set up parallelization
# 0.b.i get the number of cores
# we subtract one to reserve a core for non-lbic tasks
n.cores <-  parallel::detectCores() - 2
# 0.b.ii create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, type = 'FORK'
)
#check cluster definition (optional)
print(my.cluster)

# 0.b.iii register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
#check if it is registered (optional)
foreach::getDoParRegistered()

# 3.a. set up loop
#Sys.time()
training.full <- foreach(
  yyyy = 2010:2015, 
  .combine = 'rbind'
) %do% {
  
  #### ---------------------------- ####
  ####  3A. process date variables  ####
  #### ---------------------------- ####
  
  # 3A.a. isolate training to year of interest 
  training.year <- training %>% 
    dplyr::filter(as.numeric(date_local) == as.numeric(yyyy ))
  
  # 3A.b make year version of preds dataset 
  preds.year <- preds
  
  #### ------------ ####
  ####  3B. add av  ####
  #### ------------ ####
  
  # 3B.a. readin av
  av <- fst::read_fst(here::here('inputs', 'pm25', 'base_models', 'annual', 'formatted', 'av', 
                          paste0('av_annual_', yyyy, '.fst')))
  
  # 3B.b. assign to training data 
  # 3B.b.i. attach key to the training data 
  training.year <- training.year %>% 
    inner_join(key.aqs.av, by = 'ref_id')
  # 3B.b.ii attach the correct predictions 
  training.year <- av %>% 
    slice(training.year$baseModel_id) %>% 
    dplyr::select(pred_av) %>% 
    bind_cols(training.year) %>% 
    dplyr::select(-ref_lat, -ref_lon, -starts_with('baseModel_'))
  
  # 3B.c assign to prediction data 
  # remember the key is already in order wrt the prediction dataset 
  # because of the arranging we did earlier. 
  preds.year <- av %>% 
    slice(key.refGridConus.av$baseModel_id) %>% 
    dplyr::select(pred_av) %>%
    bind_cols(preds.year)
  
  #### ------------ ####
  ####  3C. add cb  ####
  #### ------------ ####
  
  #### ------------ ####
  ####  3C. add cc  ####
  #### ------------ ####

  # 3B.a. readin av
  cc <- read_csv(here::here('inputs', 'pm25', 'base_models', 'annual', 'raw', 'cc', 
                                 paste0('CACES_annual_', yyyy, '_blockgrp_raw.csv'))) %>% 
    rename(pred_cc = pred_wght)
  
  # 3B.b. assign to training data 
  # 3B.b.i. attach key to the training data 
  training.year <- training.year %>% 
    inner_join(key.aqs.cc, by = 'ref_id')
  # 3B.b.ii attach the correct predictions 
  training.year <- cc %>% 
    slice(training.year$baseModel_id) %>% 
    dplyr::select(pred_cc) %>% 
    bind_cols(training.year) %>% 
    dplyr::select(-ref_lat, -ref_lon, -starts_with('baseModel_'))
  
  # 3B.c assign to prediction data 
  # remember the key is already in order wrt the prediction dataset 
  # because of the arranging we did earlier. 
  preds.year <- cc %>% 
    slice(key.refGridConus.cc$baseModel_id) %>% 
    dplyr::select(pred_cc) %>%
    bind_cols(preds.year)
  
  #### ------------ ####
  ####  3D. add cm  ####
  #### ------------ ####
  
  # activate the correct cmaq keys 
  if (yyyy < 2010) {
    key.aqs.cm <- key.aqs.cm05
    key.refGridConus.cm <- key.refGridConus.cm05
  }
  if (yyyy >= 2010 & yyyy < 2015) {
    key.aqs.cm <- key.aqs.cm10
    key.refGridConus.cm <- key.refGridConus.cm10
  }
  if (yyyy >= 2015) {
    key.aqs.cm <- key.aqs.cm15
    key.refGridConus.cm <- key.refGridConus.cm15
  }
  
  # 3D.a. get cmaq predictions for year of interest
  cm <- fst::read_fst(here::here('inputs', 'pm25', 'base_models', 'annual', 
                                 'formatted', 'cmout', 
                                 paste0('cmout_', yyyy, '_formatted.fst'))) %>% 
    mutate(pred_cm = pred)
  
  # 3D.b. assign to training data 
  # 3D.b.i. attach key to the training data 
  training.year <- training.year %>% 
    inner_join(key.aqs.cm, by = 'ref_id')
  # 3D.b.ii attach the correct predictions 
  training.year <- cm %>% 
    slice(training.year$baseModel_id) %>% 
    dplyr::select(pred_cm) %>% 
    bind_cols(training.year) %>% 
    dplyr::select(-ref_lat, -ref_lon, -starts_with('baseModel_'))
  
  # 3D.c assign to prediction data 
  # remember the key is already in order wrt the prediction dataset 
  # because of the arranging we did earlier. 
  preds.year <- cm %>% 
    slice(key.refGridConus.cm$baseModel_id) %>% 
    dplyr::select(pred_cm) %>%
    bind_cols(preds.year)
  
  #### ------------ ####
  ####  3E. add gs  ####
  #### ------------ ####

  # filter by year 
  gs <- gs.allYears %>% 
    filter(year == yyyy)
  # 3D.b. assign to training data 
  # 3D.b.i. attach key to the training data 
  training.year <- training.year %>% 
    inner_join(key.aqs.gs, by = 'ref_id')
  # 3D.b.ii attach the correct predictions 
  training.year <- gs %>% 
    dplyr::slice(training.year$baseModel_id) %>% 
    dplyr::select(pred_gs) %>% 
    dplyr::bind_cols(training.year) %>% 
    dplyr::select(-ref_lat, -ref_lon, -starts_with('baseModel_'))
  
  # 3D.c assign to prediction data 
  # remember the key is already in order wrt the prediction dataset 
  # because of the arranging we did earlier. 
  preds.year <- gs %>% 
    slice(key.refGridConus.gs$baseModel_id) %>% 
    dplyr::select(pred_gs) %>%
    bind_cols(preds.year)
  
  #### ------------ ####
  ####  3E. add js  ####
  #### ------------ ####
  
  # 3E.a. bring in js 
  js <- tibble(pred_js = as.vector(readRDS(here::here('inputs', 'pm25', 'base_models', 'annual', 'raw',
                           'js', paste0(yyyy, '.rds'))) ))
  
  # 3E.c. assign to training data 
  # 3E.c.i. attach key to the training data 
  training.year <- training.year %>% 
    inner_join(key.aqs.js, by = 'ref_id')
  # 3E.c.ii attach the correct predictions 
  training.year <- js %>% 
    dplyr::slice(training.year$baseModel_id) %>% 
    dplyr::select(pred_js) %>% 
    dplyr::bind_cols(training.year) %>% 
    dplyr::select(-ref_lat, -ref_lon, -starts_with('baseModel_'))
  
  # 3E.d assign to prediction data 
  # remember the key is already in order wrt the prediction dataset 
  # because of the arranging we did earlier. 
  preds.year <- js %>% 
    slice(key.refGridConus.js$baseModel_id) %>% 
    dplyr::select(pred_js) %>%
    bind_cols(preds.year)
  
  
  #### ------------ ####
  ####  3G. add rk  ####
  #### ------------ ####
  
  # 3G.a. bring in the rk raster
  rk <- fst::read_fst(here::here('inputs', 'pm25', 'base_models', 'annual', 'formatted', 
                                 'rk',
                                 paste0('rk_annual_', yyyy, '.fst')))

    # 3G.d. keep only rk at the potenital AQS sites 
    rk.atAQS <- rk %>% 
      filter(row_number() %in% key.aqs.rk$baseModel_id)
    
    # 3G.e. attach the key 
    training.year <- training.year %>% 
      inner_join(key.aqs.rk, by = c('ref_id'))

    # 3G.f. attach rk values 
    training.year <- rk %>% 
      slice(training.year$baseModel_id) %>% 
      dplyr::select(-lat, -lon) %>% 
      bind_cols(training.year)
    
    # 3G.g. attach rk values to preds 
    preds.year <- rk %>% 
      slice(key.refGridConus.rk$baseModel_id) %>% 
      dplyr::select(-lat, -lon) %>% 
      bind_cols(preds.year)
        
  #### ------------------- ####
  ####  3H. save datasets  ####
  #### ------------------- ####
  
  # 3H.a. save it! 
  preds.year %>% 
    rename(lat = ref_lat, lon = ref_lon) %>%
      mutate(date_local = yyyy) %>%
      dplyr::select(lat, lon, date_local,
                    pred_av, pred_cc, pred_cm, pred_gs, pred_js, pred_rk) %>%
    readr::write_csv(here::here('inputs', 'pm25', 'prediction_datasets', 
                         'annual_individual', 
                         paste0('preds_annual_', yyyy, '_nome.csv')))
  
  # 3H.b. return the training dataset 
  training.year
  
}

#  4. save training dataset
training.full %>% 
  dplyr::select(-lat, -lon) %>%
  rename(lat = ref_lat, lon = ref_lon) %>%
  dplyr::select(lat, lon, date_local, obs,
                pred_av, pred_cc, pred_cm, pred_gs, pred_js, pred_rk,
                state, ref_id) %>%
  readr::write_csv(here::here('inputs', 'pm25', 'training_datasets', 'annual_combined', 
                       paste0('training_', 'annual', '_nome.csv')))

# close out cluster 
stopCluster(my.cluster)