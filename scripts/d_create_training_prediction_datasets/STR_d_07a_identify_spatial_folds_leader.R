# File: STR_h_01a_identify_spatial_folds_leader.R
# Authors:
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 08/03/2022
#
# Contents:
#  0. preparation 
#  1. identify spatial folds

#### ---------------- ####
####  0. preparation  ####
#### ---------------- ####

# 0.a. import packages and global objects, if you haven't already done so
if(!exists("ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', 
                    "a_00_import_packages_set_global_objects.R"))
}

#### --------------------------- ####
####  1. identify spatial folds  ####
#### --------------------------- ####

# 1.a. for daily prediction data 
timeWindow <- 'daily'
trainingFileName <- 'training_daily_nofolds.csv'



train.full <- readr::read_csv(here::here('inputs', 'pm25', 'training_datasets', 'daily_combined', 
                            paste0('training_', 'daily_nofolds', '.csv')))

#train.full <- train.full %>% 
#  mutate(day_of_year = day_of_year * max_doy)

source(here::here('scripts', 'd_create_training_prediction_datasets', 
                  'STR_d_06ii_identify_spatial_folds_follower.R'))

# 1.b. for annual prediction data
timeWindow <- 'annual'
train.full <- readr::read_csv(here::here('inputs', 'pm25', 'training_datasets',
                                         'annual_combined', 
                                         paste0('training_', 'annual', '.csv')))
saveFileName <- 'training_cvfolds.csv'
source(here::here('scripts', 'd_create_training_prediction_datasets', 
                  'STR_d_06ii_identify_spatial_folds_follower.R'))

timeWindow <- 'annual'
train.full <- readr::read_csv(here::here('inputs', 'pm25', 'training_datasets',
                                         'annual_combined', 
                                         paste0('training_', 'annual', '_meBlend.csv')))
saveFileName <- 'training_cvfolds_meBlend.csv'
source(here::here('scripts', 'd_create_training_prediction_datasets', 
                  'STR_d_06ii_identify_spatial_folds_follower.R'))

timeWindow <- 'annual_nome'
train.full <- readr::read_csv(here::here('inputs', 'pm25', 'training_datasets',
                                         'annual_combined', 
                                         paste0('training_', 'annual', '_meBlend.csv')))
saveFileName <- 'training_cvfolds_nome.csv'
source(here::here('scripts', 'd_create_training_prediction_datasets', 
                  'STR_d_06ii_identify_spatial_folds_follower.R'))
