# File: STR_b_02_make_prediction_dataset_annual.R
# Author: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 9/15/2021
#
# Contents:
# N. Notes
# 0. Package Imports & Global Variables
# 1. Main Loop
#    b. Set up CMAQ, GS, CACES loop
#    c. Run CMAQ, GS, CACES loop
#    d. AV data loop
#    e. Save outputs

#### -------- ####
#### N. NOTES ####
#### -------- ####

# This script takes about X minutes to run
# assuming you do not include FIPS codes...

#### ------------------------------------- ####
#### 0. PACKAGE IMPORTS & GLOBAL VARIABLES ####
#### ------------------------------------- ####

# 0a. load packages and functions required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_config_env.R"))
}

#------------------------------------------#
####  1. TRAINING DATA ####
#------------------------------------------#

# 1.a. function 
read_training <- function(yyyy) {
    readr::read_csv(here::here('inputs', 'pm25', 'training_datasets', 'annual_individual', 
                                paste0('training_avgscmjscc_', yyyy, '_all.csv')))
}

# 1.b. read all of the datasets in, combine them into one dataframe via bind_rows
# and then store result as a csv
map_dfr(2010:2015, read_training) %>% 
  readr::write_csv(here::here('inputs', 'pm25', 'training_datasets', 'annual_combined', 
                              'training_avgscmjscc_all.csv'))


#### ------------------- ####
####  2. PREDICTION DATA ####
#### ------------------- ####

# 2.a. function 
read_annual_prediction <- function(yyyy) {
  readr::read_csv(here::here('inputs', 'pm25', 'prediction_datasets', 'annual_individual', 
                             paste0('predictions_avgscmjscc_', yyyy, '_all.csv')))
}

# 2.b. read all of the datasets in, combine them into one dataframe via bind_rows
dta <- map_dfr(2010:2015, read_annual_prediction) 

# 2.c. save as csv
dta %>% 
  readr::write_csv(here::here('inputs', 'pm25', 'prediction_datasets', 'annual_combined', 
                              'predictions_avgscmjscc_all.csv'))

# 2.d. store the number of predictions, required for some matlab versions (might be depreciated)
data.frame(count = nrow(dta)) %>% 
  readr::write_csv(here::here('inputs', 'pm25', 'prediction_datasets', 'annual_combined', 
                              'predCount_avgscmjscc_all.csv'))
