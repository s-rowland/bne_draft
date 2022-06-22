# File: STR_d_02_make_training_predictions_JS.R
# Authors:
# Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 03/17/22
#
# Contents:
#  N. notes
#  0. preparation
#  1. make daily training data for each year
#  2. combined yearly daily training data 

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


#### ------------------------------------------- ####
####  1. make daily training data for each year  ####
#### ------------------------------------------- ####

for (activeYear in 2008:2016) {
  yyyy <- activeYear 
  source(here::here('scripts', 'd_create_training_prediction_datasets', 
                    'STR_d_02a_make_training_predictions_daily.R'))
}

#### ---------------------------------------- ####
####  2. combined yearly daily training data  ####
#### ---------------------------------------- ####


# 2.a identify the training files we made
list.files(here::here('inputs', 'pm25', 'training_datasets', 'daily_yearly'))

# 2.b read in and combine the training files
training.full <-  foreach(
  fileNames = list.files(here::here('inputs', 'pm25', 'training_datasets', 'daily_yearly')),
  .combine = 'rbind'
  
) %do% {
  dta <- read_csv(here::here('inputs', 'pm25', 'training_datasets', 'daily_yearly', 
                             fileNames))
  if (str_sub("training_2005.csv", 10, 13) %in% c(2008, 2012, 2016)) {
    dta$max_doy <- 366
  } else {dta$max_doy <- 365}
  dta
  
}

# 2.c save
training.full %>% 
  rename(percent_of_year = day_of_year) %>%
  readr::write_csv(here::here('inputs', 'pm25', 'training_datasets', 'daily_combined', 
                              paste0('training_', 'daily_nofolds', '.csv')))

