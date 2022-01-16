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

read_training <- function(YYYY) {
    readr::read_csv(here::here('BNE_inputs', 'training_datasets', 'individual_annual', 
                                paste0('training_avgscmjscc_', YYYY, '_all.csv')))
}

map_dfr(2010:2015, read_training) %>% 
  readr::write_csv(here::here('BNE_inputs', 'training_datasets', 'combined_annual', 
                              'training_avgscmjscc_all.csv'))


#------------------------------------------#
####  2. PREDICTION DATA ####
#------------------------------------------#

read_prediction <- function(YYYY) {
  readr::read_csv(here::here('BNE_inputs', 'prediction_datasets', 'individual_annual', 
                             paste0('predictions_avgscmjscc_', YYYY, '_all.csv')))
}

dta <- map_dfr(2010:2015, read_prediction) 

dta %>% 
   rename(pred_av = av_pred, pred_gs = gs_pred, pred_cm = cmaq_outs_pred, 
          pred_js = js_pred,pred_cc = caces_pred) %>%
  readr::write_csv(here::here('BNE_inputs', 'prediction_datasets', 'combined_annual', 
                              'predictions_avgscmjscc_all.csv'))

data.frame(count = nrow(dta)) %>% 
  readr::write_csv(here::here('BNE_inputs', 'prediction_datasets', 'combined_annual', 
                              'predCount_avgscmjscc_all.csv'))
