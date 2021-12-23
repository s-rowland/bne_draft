# File: STR_d_02a_make_training_predictions_js_annual_leader.R
# Authors: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 12/23/21
#
# Contents:
#  N. notes
#  0. Package Imports
#  1. create the training and prediction datasets

#### --------- ####
####  N. notes ####
#### --------- ####

# This script calls the d_02b script, after specifying the area of interest (AOI) 
# d_02b is not a function because it would be extremely memory intensive 
# (reading in big js datasets).

#### ------------------ ####
#### 0. PACKAGE IMPORTS ####
#### ------------------ ####

# 0a. load packages and functions required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_config_env.R"))
}

#### ----------------------------------------------- ####
####  1. create the training and prediction datasets ####
#### ----------------------------------------------- ####

AOI <- 'conus'
source(here::here('scripts', 'd_create_training_prediction_datasets', 
                  'STR_d_02b_make_training_predictions_JS_annual_follower.R'))

AOI <- 'NYS'
source(here::here('scripts', 'd_create_training_prediction_datasets', 
                  'STR_d_02b_make_training_predictions_JS_annual_follower.R'))

AOI <- 'cities'
source(here::here('scripts', 'd_create_training_prediction_datasets', 
                  'STR_d_02b_make_training_predictions_JS_annual_follower.R'))




