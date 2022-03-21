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
#  3. make training and prediction Ddta by day
#  3A. process date variables and aqs
#  3B. add av 
#  3C. add cb
#  3D. add cm 
#  3E. add js 
#  3F. add me
#  3G. add rk
#  3H. save datasets
#  4. save training dataset

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

list.files(here::here('inputs', 'pm25', 'training_datasets', 'daily_yearly'))

training.full <-  foreach(
  fileNames = list.files(here::here('inputs', 'pm25', 'training_datasets', 'daily_yearly')),
  .combine = 'rbind'
  
) %do% {
read_csv(here::here('inputs', 'pm25', 'training_datasets', 'daily_yearly', 
                    fileNames))
  
}
training.full %>% 
  readr::write_csv(here::here('inputs', 'pm25', 'training_datasets', 'daily_combined', 
                              paste0('training_', 'daily_nofolds', '.csv')))
