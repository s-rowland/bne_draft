# File: STR_d_02_make_training_prediction_datasets_annual.R
# Authors:
# Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 03/17/22
#
# Contents:
#  N. notes
#  0. preparation
#  1. create keys
#  1. make daily training data for each year
#  4. identify spatial folds

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


# 0.b. declare the Area of Interest
AOI <- 'conus01deg'

#### ---------------- ####
####  1. create keys  ####
#### ---------------- ####

# note that currently this step will make all of the potentially relevant keys, 
# not just for the Area of Interested defined in step 0.b

source(here::here('scripts', 'd_create_training_prediction_datasets', 
                  'STR_d_02a_make_keys_annual_base_models.R'))

#### ------------------------------------------ ####
####  1. make training and prediction datasets  ####
#### ------------------------------------------ ####

# note that this step will only prediction datasets for the defined AOI, 
# and will always create training datasets for CONUS. 
source(here::here('scripts', 'd_create_training_prediction_datasets', 
                  'STR_d_02b_make_training_predictions_annual.R'))

#### --------------------------- ####
####  4. identify spatial folds  ####
#### --------------------------- ####

timeWindow <- 'annual'
train.full <- readr::read_csv(here::here('inputs', 'pm25', 'training_datasets', 
                                         'annual_combined', 
                                         paste0('training_', 'annual.csv')))

saveFileName <- 'training_cvfolds_annual.csv'
source(here::here('scripts', 'd_create_training_prediction_datasets', 
                  'STR_d_06ii_identify_spatial_folds_follower.R'))