# File: STR_d_01c_fill_in_js_training_daily.R
# Authors:
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 03/17/23
#
# Contents:
#  N. notes
#  0. preparation

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

#fileNames <- list.files(here::here('inputs', 'pm25', 'training_datasets', 'daily_yearly'))[[7]]


training.full <-  foreach(
  fileNames = list.files(here::here('inputs', 'pm25', 'training_datasets', 'daily_yearly')),
  .combine = 'rbind'
  
) %do% {
   # bring in annual training data
  dta <- read_csv(here::here('inputs', 'pm25', 'training_datasets', 'daily_yearly', 
                      fileNames))
  
  # determine the year
  yyyy = str_sub(fileNames, 10, 13)
  # get the whole-number day of the year
  dta <- dta %>% 
    mutate(yyyy = yyyy)
  
  if (yyyy %in% c(2008, 2012, 2016)) {
    dta$max_doy <- 366
    } else {dta$max_doy <- 365}
  
  dta <- dta %>% 
    mutate(DoY2 = round(day_of_year*max_doy, 0) ) 
  # isolate the rows with missing js
  dta0 <- dta %>% 
    filter(is.na(pred_js))
  
  dta <- dta %>% 
    filter(!is.na(pred_js))
  
  # correct is required 
  if (nrow(dta0) > 0) {
    for (j in nrow(dta0)) {
      # isolate point 
      dta0.obs <- dta0[j,]
      # bring in prediction dataframe
      preds <- read_csv(here::here('inputs', 'pm25', 'prediction_datasets', 
                                   'daily_individual', 
                                   paste0('preds_', yyyy,'_', 
                                          str_pad(dta0.obs$DoY2[1], 3, 'left', '0' ), '.csv')))
      # find nearest full neighbor
      # remove missing from preds 
      preds <- preds %>% 
        filter(!is.na(pred_js))
    
      # make both spatial 
      preds <- preds %>% st_as_sf(coords = c('lon', 'lat'))
      
      dta0.obs.sf <- dta0.obs %>% st_as_sf(coords = c('lon', 'lat'))
      
      a <- unlist(nngeo::st_nn(dta0.obs.sf, preds))
      
      dta0.obs$pred_js[1] <- preds$pred_js[a]
      
      dta <- dta %>% bind_rows(dta0.obs)
        
    }
  }

  dta
}

training.full %>% 
  rename(percent_of_year = day_of_year) %>%
  readr::write_csv(here::here('inputs', 'pm25', 'training_datasets', 'daily_combined', 
                              paste0('training_', 'daily_nofolds', '.csv')))

