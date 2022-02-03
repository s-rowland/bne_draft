# File: STR_d_06a_make_training_predictions_js_annual_leader.R
# Authors: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 12/23/21
#
# Contents:
#  N. Notes
#  0. Import Packages and Set Global Objects
#  1. Create Training and Prediction Datasets with Just JS

#### ------------------- ####
####       N. NOTES      ####
#### ------------------- ####

# N.1. goal
# This script calls the d_02b follower script, after specifying the area of interest (AOI) 
# d_02b is not a function because it would be extremely memory intensive 
# (reading in big js datasets).

#### ------------------------------------------- ####
####  0. IMPORT PACKAGES AND SET GLOBAL OBJECTS  ####
#### ------------------------------------------- ####

# 0.a. import packages and global objects, if you haven't already done so
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

#### --------------------------------------------------------- ####
####  1. CREATE TRAINING AND PREDICITON DATASETS WITH JUST JS  ####
#### --------------------------------------------------------- ####

AOI <- 'conus'
source(here::here('scripts', 'd_create_training_prediction_datasets', 
                  'STR_d_06b_make_training_predictions_JS_annual_follower.R'))

AOI <- 'NYS'
source(here::here('scripts', 'd_create_training_prediction_datasets', 
                  'STR_d_06b_make_training_predictions_JS_annual_follower.R'))

AOI <- 'cities'
source(here::here('scripts', 'd_create_training_prediction_datasets', 
                  'STR_d_06b_make_training_predictions_JS_annual_follower.R'))
