# File: STR_d_02b_make_training_predictions_js_follower.R
# Authors:
# Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 12/23/21
#
# Contents:
#  N. Notes
#  0. Import Packages and Set Global Objects
#  1. General Set Up for Loop
#  2. Process JS by Year
#    2A. Wrangle JS
#    2B. Get JS Predictions at the AQS Observations
#    2C. Get JS Predictions for refGrid
#    2D. Save Results

#### ------------------- ####
####       N. NOTES      ####
#### ------------------- ####

# N.1. first declare area of interest! 
# This script requires you to declare the area of interest (AOI) 
# it is not a function because it would be extremely memory intensive 
# (reading in big js datasets).
# We can source this script from d_02a, the leader script, where we specify the AOI. 

#### ------------------------------------------- ####
####  0. IMPORT PACKAGES AND SET GLOBAL OBJECTS  ####
#### ------------------------------------------- ####

# 0.a. import packages and global objects, if you haven't already done so
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

#### ---------------------------- ####
####  1. GENERAL SET UP FOR LOOP  ####
#### ---------------------------- ####

# 1.a. specify the years 
years <- c(2010:2016)
timeSteps <- expand.grid(list("year" = years)) %>%
  dplyr::arrange(year)

# 1.b. read in aqs data
aqs <- readr::read_csv(here::here('inputs', 'pm25', 'ground_truth', 'formatted', 
                                  'lgc_annual_data_2000-2016_conus.csv'))

# 1.c. get the unique locations 
aqs <- aqs %>% 
  dplyr::select(lat, lon) %>%
  dplyr::distinct() %>% 
  dplyr::arrange(lat, lon) %>%
  dplyr::mutate(aqs_id = row_number()) %>% 
  dplyr::inner_join(aqs, by = c('lat', 'lon'))

# 1.d. read in the refGrid
refGrid <- fst::read_fst(here::here('inputs', 'pm25', 'reference_grids',  
                                    paste0('refGrid_', AOI, '.fst')))

# 1e read in the js keys 
# 1.e.ii for aqs (for making training dataset)
key.aqs.js <- fst::read_fst(here::here('inputs', 'pm25','keys', 
                                 paste0('aqs', '_', 'JS', '_key_nn_',
                                        'annual', '.fst')))
key.aqs.js <- key.aqs.js %>% 
  dplyr::select(-ref_id, -baseModel_lat, -baseModel_lon)
# 1.e.ii for the refGrid of our AOI - for making prediciton datasets
key.refGrid.js <- fst::read_fst(here::here('inputs', 'pm25','keys', 
                                     paste0('refGrid', AOI, '_', 'JS', '_key_nn_',
                                            'annual', '.fst')))

# 1f. set progress bar:
n <- as.numeric(as.Date("2017-01-01") - as.Date("2010-01-01"))

progressBar <- txtProgressBar(min=0, max=6, width=50, style=3)
counter <- 0
pb <- function(p) { setTxtProgressBar(progressBar, p); return(p+1) }
counter <- pb(counter)

#### ----------------------- ####
####  2. PROCESS JS BY YEAR  ####
#### ----------------------- ####

for (i in 1:nrow(timeSteps)) {

  #### ---------------- ####
  ####  2A. WRANGLE JS  ####
  #### ---------------- ####

  # 2A.a. read JS predictions
  # although for daily JS we avoid reading in the whole dataset, 
  # it is okay for annual data becuase we only have to do it a few times. 
    js <- readr::read_rds(
      here::here('inputs', 'pm25', 'base_models', 'raw', 'JS_annual_raw', 
                 paste0('PredictionStep2_Annual_PM25_USGrid_',
                        timeSteps$year[i], '0101_', timeSteps$year[i], '1231.rds')))
  
  # 2A.b. format as dataframe
  js <- tibble::as_tibble(as.vector(t(js)))
    
  # 2A.c. give the column the corred name 
  names(js) <- 'pred_js'
    
  #### ------------------------------------------------ ####
  ####  2B. GET JS PREDICTIONS AT THE AQS OBSERVATIONS  ####
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
        dplyr::inner_join(key.aqs.js, by = c('lat' = 'ref_lat', 'lon' = 'ref_lon')) 
      
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
    ####  2C. GET JS PREDICTIONS FOR REFGRID  ####
    #### ------------------------------------ ####
     
    # 2C.a. first, for our prediction points, add the corresponding crosswalk. 
   # and now we have the column of base_model_id to use for slicing. 
  refGrid.timeStep <- refGrid %>% 
    dplyr::inner_join(key.refGrid.js, by = c('lat' = 'ref_lat', 'lon' = 'ref_lon')) 
  
  # 2C.b. now keep only the js predictions at the relevant locations 
  # and put them in order
  js.atRefGrid <- js %>%
    dplyr::slice(refGrid.timeStep$baseModel_id)
  
  # 2C.d. combine
  pred.js <- cbind(refGrid.timeStep, js.atRefGrid) %>% 
    dplyr::select(-contains('baseModel'))
  
  # 2C.e. progress bar:
  counter <- pb(counter)
  
  #### ------------------ ####
  ####  2D. SAVE RESULTS  ####
  #### ------------------ ####
    
  # 2D.a. save the training data with js
  train.js %>% 
    rename(obs_pm25 = obs) %>%
      fst::write_fst(here::here('inputs', 'pm25','training_datasets', 'annual_train_js', 
                                  paste0("js_", timeSteps$year[i], ".fst")))
  # 2D.b save the prediction data with js
  pred.js %>% 
    fst::write_fst(here::here('inputs', 'pm25','prediction_datasets', "annual_pred_js", 
                       paste0('js_', AOI, '_', timeSteps$year[i], ".fst")))

}
