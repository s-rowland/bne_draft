# File: STR_b_02_make_prediction_dataset_annual.R
# Author: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 9/15/2021
#
# Contents:
# N. Notes
# 0. Package Imports & Global Variables
# 1. Main Loop
#    b. Set up CMAQ, GS, CACES loop
#    c. Run CMAQ, GS, CACES loop
#    d. AV data loop
#    e. Save outputs

#### -------- ####
#### N. NOTES ####
#### -------- ####

# This script takes about X minutes to run
# assuming you do not include FIPS codes...

#### ------------------------------------- ####
#### 0. PACKAGE IMPORTS & GLOBAL VARIABLES ####
#### ------------------------------------- ####

# 0a. load packages and functions required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_set_up_env.R"))
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
datasetCodes <- c('AV', 'GS', 'CMAQOUTS_annual',  'CACES') # 'JS_annual',
modelNames <- c("av", "gs", "cmaq_outs",  "caces") # 'js',
overrides <- c(T, T, T, T, T) # used to be c(T, F, F, F)
censusTrackFiles <- list(NULL, NULL, NULL, NULL, NULL)

#----------------------------------------#
####  2. LOOP OVER YEARS ####
#----------------------------------------#

# 2a. begin loop
for (i in 1:length(YYYYlist)){
  
  #------------------------------------------#
  ####  2A. CREATE YEAR-SPECIFIC ELEMENTS ####
  #------------------------------------------#
  
  refGrid <- readr::read_csv(here::here('BNE_inputs', 'prediction_datasets',  
                                      'JSrefGrid', 
                                      paste0('js_cities_', YYYYlist[i], '.csv'))) %>% 
    rename(obs_pm2_5 = js_pred)
                                      
  # 2A.a. add the year to the refGrid
  refGrid.yyyy <- refGrid %>% 
    dplyr::mutate(year = as.character(YYYYlist[i]))
  
  # 2A.b. list of paths where the annual data is stored
  paths <- c(
    # AV
    here::here('BNE_inputs', 'input_models', 'raw', 'av_annual_raw', 
               paste0('V4NA03_PM25_NA_', YYYYlist[i], '01_', YYYYlist[i], '12-RH35.nc')),
    # GS 
    here::here('BNE_inputs', 'input_models', 'raw', 
               'gs_annual_raw', 'GBD2016_PREDPOP_FINAL.rdta'), 
    #CMout
    here::here('BNE_inputs', 'input_models', 'formatted', 'cmout_annual_formatted',
               paste0('cmout_annual_',YYYYlist[i], '_formatted.fst')),
    # JS 
    #here::here('BNE_inputs', 'input_models', 'formatted', 'js_annual_formatted', 
     #          paste0('js_annual_', YYYYlist[i], '_formatted.fst')),
    # CACES
    here::here('BNE_inputs', 'input_models', 'raw', 
               'cc_annual_raw', 
               paste0('CACES_annual_', YYYYlist[i], '_blockGrp_raw.csv'))
  )
  
  #-----------------------------------------#
  ####  2B. INNER LOOP OVER INPUT MODELS ####
  #-----------------------------------------#
  
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
    rename(js_pred = obs_pm2_5)
  
  #-------------------------#
  ####  2C. SAVE DATASET ####
  #-------------------------#
  
  # 2C.a. save results
  refGrid.yyyy %>% 
    mutate(time = YYYYlist[i]) %>%
    dplyr::select(lat, lon, time, av_pred, gs_pred, cmaq_outs_pred, js_pred, caces_pred, ref_id) %>%
    readr::write_csv(here::here('BNE_inputs', 'prediction_datasets', 'individual_annual', 
                                paste0('predictions_avgscmjscc_', YYYYlist[i], '_cities.csv')))
  
  # 2C.b. save number of observations 
  # we use the number of observations when we use trained BNE to generate predictions
  data.frame(num_point = nrow(refGrid.yyyy)) %>% 
    readr::write_csv(here::here('BNE_inputs', 'prediction_datasets', 'individual_annual', 
                                paste0('predCount_avgscmjscc_', YYYYlist[i], '_cities.csv')))

}

