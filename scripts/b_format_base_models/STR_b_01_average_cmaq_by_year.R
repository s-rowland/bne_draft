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
  
#### ---------------------------- ####
#### 1. AVERAGE ACROSS THE YEARS  ####
#### ---------------------------- ####

# 1.a. aggregate cmaq ins for all of the years
purrr::map2('ins', c(2010:2015), getAnnualAverageCMAQ)

# 1.b. aggregate cmaq out for all of the years
purrr::map2('out', c(2010:2015), getAnnualAverageCMAQ)

  