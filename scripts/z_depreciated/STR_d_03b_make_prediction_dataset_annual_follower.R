# File: STR_b_02b_make_prediction_dataset_annual_follower.R
# Author: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 01/14/2022
#
# Contents:
#  N. Notes
#  0. package imports & global variables
#  1. set features we will loop over
#  2. loop over the years
#    2A. create year-specific objects
#    2B. inner loop over base models
#    2C. save dataset


#### -------- ####
#### N. notes ####
#### -------- ####

# This script takes about X minutes to run
# assuming you do not include FIPS codes...


# Issue: currently MERRA is set to read for just 2010 because the other years 
# are not yet available. 

# Issue: we need a way to deal with CB, which has its own join approach 

#### ----------------------------------- ####
####  0. set features we will loop over  ####
#### ----------------------------------- ####

# 0.a. import packages and global objects, if you haven't already done so
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

#------------------------------------------#
####  1. SET FEATURES WE WILL LOOP OVER ####
#------------------------------------------#

# 1a. read the refGrid
#refGrid <- fst::read_fst(here::here('data_ancillary', 'generated',  
 #                                   'refGrid_0125Deg_Centroids.fst'))

# 1b. make the columns that are required for the join function
#refGrid <- refGrid %>% 
 # dplyr::mutate(ref_id = row_number()) %>%
  #dplyr::mutate(obs_pm2_5 = 1) 

# 1c. list of years
YYYYlist <- 2010:2015

# 1d. list of codes defining the datasets
datasetCodes <- c('JS_annual', 'AV_annual', 'GS_annual', 'CMAQOUTS_annual',  
                  'CACES_annual' , "MERRA_annual") # 'CB_annual',
modelNames <- c('js', "av", "gs", "cmaq_outs",  "caces", 'merra') # 'cb',
modelNames <- c('js', "av", "gs", "cmaq_outs",  "caces", 'merra') # 'cb',

overrides <- c(T, T, T, T, T, T) # used to be c(T, F, F, F)
censusTrackFiles <- list(NULL, NULL, NULL, NULL, NULL, NULL)

conus <- sf::st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                                'conus.shp')) %>% 
  sf::st_transform(., crs=st_crs('epsg:4326'))

#### -------------------- ####
####  2. loop over years  ####
#### -------------------- ####

# 2a. begin loop
for (i in 1:length(YYYYlist)){
  
  #### ---------------------------------- ####
  ####  2A. create year-specific objects  ####
  #### ---------------------------------- ####
  
  refGrid <- fst::read_fst(here::here('inputs', 'pm25', 'reference_grids',  
                                      paste0('refGrid_', AOI, '.fst')))
  
                                      
  # 2A.a. add the year to the refGrid
  refGrid.yyyy <- refGrid %>% 
    dplyr::mutate(year = as.character(YYYYlist[i]))
  
  # 2A.b. list of paths where the annual data is stored
  paths <- c(
    # JS 
    here::here('inputs', 'pm25', 'base_models', 'raw', 'JS_annual_raw', 
               paste0(YYYYlist[i], '.rds')),
    
    # AV
    here::here('inputs', 'pm25', 'base_models', 'raw', 'av_annual_raw', 
               paste0('V4NA03_PM25_NA_', YYYYlist[i], '01_', YYYYlist[i], '12-RH35.nc')),
    # GS 
    here::here('inputs', 'pm25', 'base_models', 'raw', 
               'gs_annual_raw', 'GBD2016_PREDPOP_FINAL.RData'), 
    #CMout
    here::here('inputs', 'pm25', 'base_models', 'formatted', 'cmout_annual_formatted',
               paste0('cmout_annual_',YYYYlist[i], '_formatted.fst')),
    # CACES
    here::here('inputs', 'pm25', 'base_models', 'raw', 
               'cc_annual_raw', 
               paste0('CACES_annual_', YYYYlist[i], '_blockGrp_raw.csv')),
    # MERRA
    here::here('inputs', 'pm25', 'base_models', 'raw', 
               'merra_annual_raw', 
               paste0(2010, 'adjPM25sum_jun2021.nc'))
  )
  
  #### --------------------------------- ####
  ####  2B. inner loop over base models  ####
  #### --------------------------------- ####
  
  # 2B.a. begin loop
  for (j in 1:length(datasetCodes)){
    
    # 2B.b. read dataset of input model's predictions
    modelData <- loadData(path = paths[j], dataset = datasetCodes[j]) %>%
      dplyr::filter(year == YYYYlist[i])
    
    # 2B.c. remove the fips column if included
    if(stringr::str_detect(ls(modelData)[1], 'fips')){
      modelData <- modelData %>% 
        dplyr::select(-fips)
    }
    
    refGrid.yyyy$obs_pm2_5 = 2
    refGrid.yyyy <- refGrid.yyyy %>% mutate(ref_id = row_number())
    # 2B.d. conduct spatial join 
    refGrid.yyyy <- spatioTemporalJoin(refData = refGrid.yyyy,
                                       modelData = modelData,
                                       modelName = modelNames[j],
                                       override = overrides[j],
                                       censusTractFile = censusTrackFiles[[j]])
  }

  # 2B.e. remove duplicates 
  # for some reason the function creates 147 duplicate rows 
  refGrid.yyyy <- refGrid.yyyy %>% 
    distinct() %>% 
    dplyr::select(-obs_pm2_5)
  
  if(YYYYlist[i] != 2010) {
    refGrid.yyyy <- refGrid.yyyy %>% 
      mutate(pred_me = NA)
  }
  
  #### ------------------ ####
  ####  2C. save dataset  ####
  #### ------------------ ####
  
  # 2C.a. save results
  refGrid.yyyy %>% 
    mutate(time = YYYYlist[i]) %>%
    rename(pred_av = av_pred, pred_gs = gs_pred, pred_cm = cmaq_outs_pred, pred_js = js_pred, pred_merra = merra_pred) %>% 
    dplyr::select(lat, lon, time, av_pred, gs_pred, cmaq_outs_pred, js_pred, caces_pred, merra_pred, ref_id) %>%
    readr::write_csv(here::here('inputs', 'pm25', 'prediction_datasets', 'annual_individual', 
                                paste0('predictions_main', YYYYlist[i], '_', AOI, '.csv')))
  
  # 2C.b. save number of observations 
  # we use the number of observations when we use trained BNE to generate predictions
  data.frame(num_point = nrow(refGrid.yyyy)) %>% 
    readr::write_csv(here::here('inputs', 'pm25', 'prediction_datasets', 'annual_individual', 
                                paste0('predCount_main_', YYYYlist[i], '_', AOI, '.csv')))

}

