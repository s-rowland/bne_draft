# File: STR_b_02_make_prediction_dataset_annual.R
# Author: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 9/15/2021
#
# Contents:
# N. Notes
# 0. Package Imports
# 1. Read in EPA data
# 2. Read in JS site codes
# 3. Spatial Join
# 4. Predictions Dataset

#---------------------------#
####       N. NOTES      ####
#### ------------------- ####

# The point of this script is to create two files:
#
# (1) epa_js_nn_key.csv, which maps EPA monitor locations to
#     their nearest neighbour JS <lat, lon> coordinate pairs, along with 
#     recording the index of the JS pair. In this way, a "key" is made, allowing
#     someone to extract only the points of interest from a JS .rda file using the
#     "js_index" column from epa-js_nn_key.csv (= "nnTable" in this file before writing the csv).
#
# (2) js_preds_ref_grid.csv, which simply contains a random 1% of JS's data to be used 
#     as a reference grid for a predictions dataset. A plot of the spatial distribution 
#     can be found towards the end of the file.

#### ------------------- ####
####  0. PACKAGE IMPORTS ####
#### ------------------- ####

if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_set_up_env.R"))
}

#--------------------------------#
#### 1. FUNCTION TO AGGREGATE ####
#--------------------------------#

# begin function
aggregate_cmqout <- function(YYYY){
  
  # 1a. read the cmaq dataset
  cmq <- readr::read_csv(here::here('BNE_inputs', 'input_models', 'raw', 'cmout_daily_raw', 
                                    paste0(YYYY, '_pm25_daily_average.txt'))) %>% 
    janitor::clean_names()
  
  # 1b. rename the variables according to the year
  if (YYYY != 2015){
    cmq <- cmq %>% 
      dplyr::rename(lat = latitude, lon = longitude, pred = pm25_daily_average_ug_m3) 
  } else if (YYYY == 2015) {
    cmq <- cmq %>% 
      dplyr::rename(lat = latitude, lon = longitude, pred = prediction)
  }
  
  # 1c. get the annual average at each location
  cmq.fips <- cmq %>%
    dplyr::group_by(lat, lon) %>% 
    dplyr::summarize(pred = mean(as.numeric(pred)))
  
  # 1d. save as an fst
  cmq.fips %>% 
    fst::write_fst(
      here::here('BNE_inputs', 'input_models', 'formatted', 'cmout_annual_formatted', 
                 paste0('cmout_annual_', YYYY, '_formatted.fst')))
}
  
#-------------------------------------#
#### 2. AGGREGATE ACROSS THE YEARS ####
#-------------------------------------#

# 2a. aggregate for all of the years
purrr::map(c(2010:2015), aggregate_cmqout)
  
  