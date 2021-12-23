# File: STR_d_02b_make_training_predictions_js_follower.R
# Authors:
# Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 12/23/21
#
# Contents:
#  N. Notes
#  0. Package Imports
#  1. general set up
#  2. process js by year
#  2A. wrangle js
#  2B. get js predictions at the AQS observations
#  2C. get js predictions for refGrid
#  2D. save results

#### ------------------ ####
####       N. NOTES     ####
#### ------------------ ####

# This script requires you to declare the area of interest (AOI) 
# it is not a function because it would be extremely memory intensive 
# (reading in big js datasets).
# We can source this script from d_02a, the leader script, where we specify the AOI. 

#### ------------------ ####
#### 0. PACKAGE IMPORTS ####
#### ------------------ ####

# 0a. load packages and functions required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_config_env.R"))
}

#### ------------------ ####
####  1. general set up ####
#### ------------------ ####

# 1a. specify the years 
years <- c(2010:2016)
timeSteps <- expand.grid(list("year" = years)) %>%
  dplyr::arrange(year)

# 1b. read in aqs data
aqs <- readr::read_csv(here::here('BNE_inputs', 'ground_truth', 'formatted', 
                                  'lgc_annual_data_2000-2016_conus.csv'))

# get the unique id's 
aqs <- aqs %>% 
  dplyr::select(lat, lon) %>%
  dplyr::distinct() %>% 
  dplyr::arrange(lat, lon) %>%
  dplyr::mutate(aqs_id = row_number()) %>% 
  inner_join(aqs, by = c('lat', 'lon'))

# 1c. read in the refGrid
refGrid <- fst::read_fst(here::here('data_ancillary', 'generated',  
                                    paste0('refGrid_', 'conus', '.fst')))

# 1c read in the js keys 
key.aqs.js <- read_fst(here::here('BNE_inputs','keys', 
                                 paste0('aqs', '_', 'JS', '_key_nn_',
                                        'annual', '.fst')))
key.aqs.js <- key.aqs.js %>% 
  dplyr::select(-ref_id, -baseModel_lat, -baseModel_lon)

key.refGrid.js <- read_fst(here::here('BNE_inputs','keys', 
                                     paste0('refGrid', AOI, '_', 'JS', '_key_nn_',
                                            'annual', '.fst')))

# 1d. set up output directories:
#outDir.train <- here::here('BNE_inputs','training_datasets', "train_js")
#outDir.pred <- here::here('BNE_inputs','prediction_datasets', "pred_js")

# 1e. progress bar:
n <- as.numeric(as.Date("2017-01-01") - as.Date("2010-01-01"))

progressBar <- txtProgressBar(min=0, max=6, width=50, style=3)
counter <- 0
pb <- function(p) { setTxtProgressBar(progressBar, p); return(p+1) }
counter <- pb(counter)

#### ----------------------- ####
####  2. process js by year  ####
#### ----------------------- ####

for (i in 1:nrow(timeSteps)) {

  #### ---------------- ####
  ####  2A. wrangle js  ####
  #### ---------------- ####

  # 2A.a. read JS predictions
    js <- readr::read_rds(
      here::here('BNE_inputs', 'input_models', 'raw', 'JS_annual_raw', 
                 paste0('PredictionStep2_Annual_PM25_USGrid_',
                        timeSteps$year[i], '0101_', timeSteps$year[i], '1231.rds')))
  
  # 2A.b. format as dataframe
  js <- tibble::as_tibble(as.vector(t(js)))
    
  # 2A.c. give the column the corred name 
  names(js) <- 'pred_js'
    
  #### ------------------------------------------------ ####
  ####  2B. get js predictions at the AQS observations  ####
  #### ------------------------------------------------ ####
    
  # 2B.a. restrict to timeStep
  aqs.timeStep <- aqs %>% 
    dplyr::filter(year == timeSteps$year[i],)
    
    # 2B.b. join aqs and js predictions 
  # we have this condition just in case aqs data is missing everywhere for one day 
    if (nrow(aqs.timeStep) > 0) {
      
      # 2B.c. first, for our training points, add the corresponding crosswalk. 
      # and now we have the column of base_model_id to use for slicing, in the right order
      aqs.timeStep <- aqs.timeStep %>% 
        inner_join(key.aqs.js, by = c('lat' = 'ref_lat', 'lon' = 'ref_lon')) 
      
      # 2B.d. now keep only the js predictions at the relevant locations 
      # and put them in order
      js.atAQS <- js %>%
        dplyr::slice(aqs.timeStep$baseModel_id)
      
      # 2B.e. combine
      train.js <- cbind(aqs.timeStep, js.atAQS) %>% 
        dplyr::select(-baseModel_id)

      # 2B.e. if there are no aqs observations for that timestep, just return an 
      # empty dataframe
    } else {
      
      train.js <- tibble::tibble()
      
    }
    
    #### ------------------------------------ ####
    ####  2C. get js predictions for refGrid  ####
    #### ------------------------------------ ####
     
    # 2C.a. first, for our prediction points, add the corresponding crosswalk. 
  # and now we have the column of base_model_id to use for slicing. 
  refGrid.timeStep <- refGrid %>% 
    inner_join(key.refGrid.js, by = c('lat' = 'ref_lat', 'lon' = 'ref_lon')) 
  
  # 2C.b. now keep only the js predictions at the relevant locations 
  # and put them in order
  js.atRefGrid <- js %>%
    dplyr::slice(refGrid.timeStep$baseModel_id)
  
  # 2C.d. combine
  pred.js <- cbind(refGrid.timeStep, js.atRefGrid) %>% 
    dplyr::select(-contains('baseModel'))
  
  # progress bar:
  counter <- pb(counter)
  
  #### ------------------ ####
  ####  2D. save results  ####
  #### ------------------ ####
    
  # 2D.a. save the training data with js
  train.js %>% 
      readr::write_csv(here::here('BNE_inputs','training_datasets', 'train_js_annual', 
                                  paste0("js_", timeSteps$year[i], ".csv")))
  # 2D.b save the prediction data with js
  pred.js %>% 
    readr::write_csv(here::here('BNE_inputs','prediction_datasets', "pred_js_annual", 
                       paste0('js_', AOI, '_', timeSteps$year[i], ".csv")))

}
