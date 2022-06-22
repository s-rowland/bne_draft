# File: STR_b_02_make_prediction_dataset_annual.R
# Author: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 9/15/2021
#
# Contents:
#  N. Notes
#  0. Import Packages and Set Global Objects
#  1. Main Loop
#    1A. Create Year-Specific Objects
#    1B. Run CMAQ, GS, CACES Loop
#    1C. AV Loop
#    1D. Save Outputs

#### -------- ####
#### N. NOTES ####
#### -------- ####

# This script takes about X minutes to run
# assuming you do not include FIPS codes...

#### ------------------------------------------- ####
####  0. IMPORT PACKAGES AND SET GLOBAL OBJECTS  ####
#### ------------------------------------------- ####

# 0.a. import packages and global objects, if you haven't already done so
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

#### -------------- ####
####  1. MAIN LOOP  ####
#### -------------- ####

# 1.a. set list of years to loop over
YYYYlist <- 2010:2015

# 1.b. list of codes defining the datasets
baseModelCodes <- c('AV_annual', 'GS_annual', 'CMAQOUTS_annual',  'CACES_annual') #, 'MERRA_annual') 
baseModelNames <- c("av", "gs", "cm",  "cc")
  #c("av", "gs", "cmaq_outs",  "caces") #, 'merra') 
overrides <- c(T, T, T, T, T) #, T) # used to be c(T, F, F, F)
censusTrackFiles <- list(NULL, NULL, NULL, NULL, NULL) #, NULL)

# 1.c. begin loop
for (i in 1:length(YYYYlist)) {
  
  #### --------------------------------- ####
  ####  1A. CREATE YEAR-SPECIFIC OBJECTS ####
  #### --------------------------------- ####
  
  # 1A.a. bring in the training dataset that already has js. 
  train.js <-
    fst::read_fst(here::here('inputs', 'pm25','training_datasets', 'annual_train_js', 
                              paste0("js_", YYYYlist[i], ".fst")))
  
  # 1A.a. add the year to the refGrid
 train.js <- train.js %>% 
    dplyr::mutate(year = as.character(YYYYlist[i]))
  
  # 1A.b. rename the aqs column 
  train.js <- train.js %>% 
    rename(obs_pm2_5 = obs_pm25)
  
  # 2A.b. list of paths where the annual data is stored
  paths <- c(
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
               paste0('CACES_annual_', YYYYlist[i], '_blockGrp_raw.csv')) #, 
    # MERRA
   # here::here('inputs', 'pm25', 'base_models', 'raw', 
    #           'merra_annual_raw', 
     #          paste0(YYYYlist[i], 'adjPM25sum_jun2021.nc'))
  )
  
  #-----------------------------------------#
  ####  2B. INNER LOOP OVER INPUT MODELS ####
  #-----------------------------------------#
  
  # 2B.a. begin loop
  for (j in 1:length(baseModelCodes)){
    
    # 2B.b. read dataset of input model's predictions
    baseModelData <- loadData(path = paths[j], dataset = baseModelCodes[j]) %>%
      dplyr::filter(year == YYYYlist[i])
    
    # 2B.c. remove the fips column if included
    if(stringr::str_detect(ls( baseModelData)[1], 'fips')){
      baseModelData <-  baseModelData %>% 
        dplyr::select(-fips)
    }
    
    # 2B.d. conduct spatial join 
    train.js <- spatioTemporalJoin(refData = train.js,
                                       modelData = baseModelData,
                                       modelName = baseModelNames[j],
                                       override = overrides[j],
                                       censusTractFile = censusTrackFiles[[j]])
  }

  #-------------------------#
  ####  2C. SAVE DATASET ####
  #-------------------------#
  

  # 2C.i. rename variables
  train.js <- train.js %>% 
    rename(obs_pm25 = obs_pm2_5)
  
  # 2C.ii. save results
  train.js %>% 
    dplyr::select(lat, lon, year, obs_pm25, pred_av, pred_gs, pred_cm, pred_js, pred_cc, #pred_me, 
                  ref_id ) %>% 
    readr::write_csv(here::here('inputs', 'pm25', 'training_datasets', 'annual_individual', 
                                paste0('training_avgscmjscc_', YYYYlist[i], '_all.csv')))
}

