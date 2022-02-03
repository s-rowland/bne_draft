# File: STR_b_02b_make_prediction_dataset_annual_follower.R
# Author: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 01/14/2022
#
# Contents:
#  N. Notes
#  0. package imports & global variables
#  1. set features we will loop over
#  2. loop over the years
#    2A. create year-specific objects
#    2B. inner loop over base models
#    2C. save dataset


#### -------- ####
#### N. notes ####
#### -------- ####

# This script takes about X minutes to run
# assuming you do not include FIPS codes...


# Issue: currently MERRA is set to read for just 2010 because the other years 
# are not yet available. 

#### ----------------------------------- ####
####  0. set features we will loop over  ####
#### ----------------------------------- ####

# 0.a. import packages and global objects, if you haven't already done so
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

#------------------------------------------#
####  1. SET FEATURES WE WILL LOOP OVER ####
#------------------------------------------#

AOI <- 'Conus'
source(here::here('scripts', 'd_create_training_prediction_datasets', 
                  'STR_d_02b_make_prediction_dataset_annual_follower.R'))
AOI <- 'NYS'
source(here::here('scripts', 'd_create_training_prediction_datasets', 
                  'STR_d_02b_make_prediction_dataset_annual_follower.R'))
AOI <- 'Cities'
source(here::here('scripts', 'd_create_training_prediction_datasets', 
                  'STR_d_02b_make_prediction_dataset_annual_follower.R'))