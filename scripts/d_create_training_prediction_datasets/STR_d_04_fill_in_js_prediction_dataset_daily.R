# File: STR_d_02_make_training_predictions_JS.R
# Authors:
# Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 03/17/23
#
# Contents:
#  N. notes
#  0. Package Imports

#### ------------------ ####
####       N. notes     ####
#### ------------------ ####


#yyyy <- 2010


#### -------------- ####
#### 0. preparation ####
#### -------------- ####

# 0.a. load packages, etc
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

fileNames <- list.files(here::here('inputs', 'pm25', 'prediction_datasets', 'daily_individual'))[[1]]
fileNames <- "preds_2013_237.csv"

training.full <-  foreach(
  fileNames = list.files(here::here('inputs', 'pm25', 'prediction_datasets', 'daily_individual')),
  .combine = 'rbind'
  
) %do% {
   # bring in annual training data
  dta <- read_csv(here::here('inputs', 'pm25', 'prediction_datasets', 'daily_individual', 
                      fileNames))
  
  # isolate the rows with missing js
  dta0 <- dta %>% 
    filter(is.na(pred_js))
  
  dta <- dta %>% 
    filter(!is.na(pred_js))
  
  
  if (nrow(dta0) > 0) {
    for (j in nrow(dta0)) {
      # isolate point 
      dta0.obs <- dta0[j,]
      # bring in prediction dataframe
      preds <- dta 
      # find nearest full neighbor
      # remove missing from preds 
      preds <- preds %>% 
        filter(!is.na(pred_js))
      
      # make both spatial 
      preds <- preds %>% st_as_sf(coords = c('lon', 'lat'))
      
      dta0.obs.sf <- dta0.obs %>% st_as_sf(coords = c('lon', 'lat'))
      
      a <- unlist(nngeo::st_nn(dta0.obs.sf, preds))
      
      dta0.obs$pred_js[1] <- preds$pred_js[a]
      
      dta <- dta %>% 
        bind_rows(dta0.obs)
      
    }
    dta.full <- dta
    else {dta.full <- dta}
  }
  
  dta.full %>% write_csv(here::here('inputs', 'pm25', 'prediction_datasets', 'daily_individual', 
                             fileNames))
}
  
 