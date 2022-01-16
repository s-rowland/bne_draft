# File: STR_b_02_average_cmaq_by_year.R
# Author: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 01/14/2022
#
# Contents:
#  N. Notes
#  0. Import Packages and Global Objects
#  1. Define Function to Get Annual Average
#  2. Average Across the Years

#### ------------------- ####
####       N. NOTES      ####
#### ------------------- ####

# N.1. goal 
# the goal of this function to get CMAQ's estimate of annual average PM2.5 
# concentration. CMAQ does not directly report these values, so we calculate 
# them from the daily values. 

#### ------------------------------------------- ####
####  0. IMPORT PACKAGES AND SET GLOBAL OBJECTS  ####
#### ------------------------------------------- ####

# 0.a. import packages and global objects, if you haven't already done so
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

#### ----------------------------------------- ####
#### 1. DEFINE FUNCTION TO GET ANNUAL AVERAGE  ####
#### ----------------------------------------- ####

# begin function
aggregate_cmqout <- function(YYYY){
  
  # 1.a. read the cmaq dataset
  cmq <- readr::read_csv(here::here('inputs', 'pm25', 'base_models', 'raw', 'cmout_daily_raw', 
                                    paste0(YYYY, '_pm25_daily_average.txt'))) %>% 
    janitor::clean_names()
  
  # 1.b. rename the variables according to the year
  if (YYYY < 2015){
    cmq <- cmq %>% 
      dplyr::rename(lat = latitude, lon = longitude, pred = pm25_daily_average_ug_m3) 
  } else if (YYYY >= 2015) {
    cmq <- cmq %>% 
      dplyr::rename(lat = latitude, lon = longitude, pred = prediction)
  }
  
  # 1.c. get the annual average at each location
  cmq.fips <- cmq %>%
    dplyr::group_by(lat, lon) %>% 
    dplyr::summarize(pred = mean(as.numeric(pred)))
  
  # 1.d. save as an fst
  cmq.fips %>% 
    fst::write_fst(
      here::here('inputs', 'pm25', 'base_models', 'formatted', 'cmout_annual_formatted', 
                 paste0('cmout_annual_', YYYY, '_formatted.fst')))
}
  
#### ---------------------------- ####
#### 2. AVERAGE ACROSS THE YEARS  ####
#### ---------------------------- ####

# 2.a. aggregate for all of the years
purrr::map(c(2010:2015), aggregate_cmqout)
  