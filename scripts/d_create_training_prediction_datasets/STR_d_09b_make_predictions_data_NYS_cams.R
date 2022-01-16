# File: STR_b_02_make_prediction_dataset_annual.R
# Author: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 9/15/2021
#
# Contents:
# N. Notes
# 0. Package Imports & Global Variables
# 1. Main Loop
#    b. Set up base mode loop
#    c. Run up base model loop
#    d. Save outputs

#### -------- ####
#### N. NOTES ####
#### -------- ####

# This script takes about X minutes to run
# assuming you do not include FIPS codes...

# the code is set up so that the paths object in the inner loop in modular - 
# you can add whatever paths you'd like, and then only rely on the ones actually 
# in the modelNames list

#### ------------------------------------- ####
#### 0. PACKAGE IMPORTS & GLOBAL VARIABLES ####
#### ------------------------------------- ####

# 0a. load packages and functions required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_config_env.R"))
}

AOI <- 'NYS'

#------------------------------------------#
####  1. SET FEATURES WE WILL LOOP OVER ####
#------------------------------------------#

# 1a. name the years we will loop over
# This is code is more convoluted so that it is parallel with the daily code
years <- c(2010) #:2016)
timeSteps <- expand.grid(list("year" = years)) %>%
  dplyr::arrange(year)

# 1b. define all of the base models you want to include - including js. 
baseModelNames.full <- c('js', "av", "gs", "cm",  "cc", 'me')

# 1c. name the list of codes defining the datasets
baseModelNames <- baseModelNames.full[2:length(baseModelNames.full)] 
baseModelCodes <- c('AV', 'GS', 'CMAQOUTS_annual',  'CACES' , "MERRA_annual") 
overrides <- c(T, T, T, T, T, T) # used to be c(T, F, F, F)
censusTrackFiles <- list(NULL, NULL, NULL, NULL, NULL, NULL)

# 1d. create list of column names of predictions
predColNames <- paste0('pred_', baseModelNames.full)

#----------------------------------------#
####  2. LOOP OVER YEARS ####
#----------------------------------------#

# we do a loop because this is a bit too memory intensive for map. 
# 2a. begin loop
for (i in 1:nrow(timeSteps)){
  
  #------------------------------------------#
  ####  2A. CREATE YEAR-SPECIFIC ELEMENTS ####
  #------------------------------------------#
  
  # 2A.a. bring in the predictions dataset 
  # at this point it only has the js predictions that we added earlier in d_02
  # we have to rename pred_js to obs_pm2_5 because of some hard-coding in the 
  # spatiotemporaljoin function which we will clena up 
  preds.df <- fst::read_fst(here::here('BNE_inputs','prediction_datasets', "pred_js_annual", 
                                paste0('js_', AOI, '_', timeSteps$year[i], ".fst"))) %>% 
    rename(obs_pm2_5 = pred_js)
  
  # 2A.b. add the year 
  preds.df <- preds.df %>% 
    dplyr::mutate(year = as.character(timeSteps$year[i]))
  
  # 2A.c. list of paths where the annual data is stored
  # these are named within the loop because the paths are different each year
  
  paths <- list(
    # AV
   av = here::here('BNE_inputs', 'input_models', 'raw', 'av_annual_raw', 
               paste0('V4NA03_PM25_NA_', timeSteps$year[i], '01_', timeSteps$year[i], '12-RH35.nc')),
    # GS 
    gs = here::here('BNE_inputs', 'input_models', 'raw', 
               'gs_annual_raw', 'GBD2016_PREDPOP_FINAL.RData'), 
    #CMout
    cm = here::here('BNE_inputs', 'input_models', 'formatted', 'cmout_annual_formatted',
               paste0('cmout_annual_',timeSteps$year[i], '_formatted.fst')),
    # CACES
    cc = here::here('BNE_inputs', 'input_models', 'raw', 
               'cc_annual_raw', 
               paste0('CACES_annual_', timeSteps$year[i], '_blockGrp_raw.csv')),
    # MERRA
    me = here::here('BNE_inputs', 'input_models', 'raw', 
               'merra_annual_raw', 
               paste0(timeSteps$year[i], 'adjPM25sum_jun2021.nc'))
  )
  
  #-----------------------------------------#
  ####  2B. INNER LOOP OVER base MODELS ####
  #-----------------------------------------#
  
  # 2B.a. begin loop
  for (j in 1:length(baseModelNames)){
    
    # 2B.b. read dataset of input model's predictions
    baseModel <- loadData(path = paths[baseModelNames[j]][[1]], dataset = baseModelCodes[j]) %>%
      dplyr::filter(year == timeSteps$year[i])
    
   # baseModel <- loadData(path = paths[j], dataset = datasetCodes[j]) %>%
    #  dplyr::filter(year == timeSteps$year[i])
    
    # 2B.c. remove the fips column if included
    if(stringr::str_detect(ls(baseModel)[1], 'fips')){
      baseModel <- baseModel %>% 
        dplyr::select(-fips)
    }
    
    # 2B.d. conduct spatial join 
    preds.df <- spatioTemporalJoin(refData = preds.df,
                                       modelData = baseModel,
                                       modelName = baseModelNames[j],
                                       override = overrides[j],
                                       censusTractFile = censusTrackFiles[[j]], 
                                       verbose = FALSE)
  }

  # 2B.e. remove duplicates 
  # for some reason the function creates 147 duplicate rows 
  preds.df <- preds.df %>% 
    distinct() %>% 
    rename(pred_js = obs_pm2_5)
  
  #-------------------------#
  ####  2C. SAVE DATASET ####
  #-------------------------#
  

 
  # 2C.b. save results
  preds.df %>% 
    dplyr::mutate(time = timeSteps$year[i]) %>%
    dplyr::select(lat, lon, time, predColNames, ref_id) %>%
    readr::write_csv(here::here('BNE_inputs', 'prediction_datasets', 'individual_annual', 
                                paste0('predictions_', 
                                paste0(baseModelNames.full, collapse =''), '_', 
                                timeSteps$year[i], '_', AOI, '.csv')))
  
  # 2C.c. save number of observations 
  # we use the number of observations when we use trained BNE to generate predictions in the MatLab spatial version
  data.frame(num_point = nrow(preds.df)) %>% 
    readr::write_csv(here::here('BNE_inputs', 'prediction_datasets', 'individual_annual', 
                                paste0('predCount_', 
                                       paste0(baseModelNames.full, collapse =''), '_', 
                                       timeSteps$year[i], '_', AOI, '.csv')))

}

