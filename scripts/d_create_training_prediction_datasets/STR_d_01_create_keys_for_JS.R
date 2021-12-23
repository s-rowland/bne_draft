# File: STR_02a_filter_JS_for_aqs.R
# Author: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 12/22/2021
#
# Contents:
# N. notes
# 0. import packages and global variables
# 1. Read in EPA data
# 2. Read in JS site codes
# 3. Spatial Join
# 4. Predictions Dataset

#----------------#
#### N. notes ####
#----------------#

# The point of this script is to create two files:
#
# (1) epa-js_nn_key.csv, which maps EPA monitor locations to
#     their nearest neighbour JS <lat, lon> coordinate pairs, along with 
#     recording the index of the JS pair. In this way, a "key" is made, allowing
#     someone to extract only the points of interest from a JS .rda file using the
#     "js_index" column from epa-js_nn_key.csv (= "nnTable" in this file before writing the csv).

#### ------------------- ####
####  0. PACKAGE IMPORTS ####
#### ------------------- ####

# 0a. load packages and functions required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_config_env.R"))
}

## ------------------------ ##
#### 1. annual aqs and JS ####
## ------------------------ ##

# 1a. read js locations
jsPath <- here::here('BNE_inputs', 'input_models', 'raw', 'JS_annual_raw', 
                     'USGridSite.rds')
js.loc <- loadData(jsPath, "JSSITES") %>% 
  rename(lat = js_lat, lon = js_lon)

# 1b. read annual AQS
aqs <- read_csv(aqsPath <- here::here('BNE_inputs', 'ground_truth', 'formatted', 
                                    'lgc_annual_data_2000-2016_conus.csv'))

# 1c. create key for annual AQS and JS
create_baseModelKey(ref.df = aqs, refName = 'aqs', baseModel.df = js.loc, 
                    baseModelName = 'js', timeScale = 'annual') 

# 1d. read daily AQS
aqs <- read_csv(aqsPath <- here::here('BNE_inputs', 'ground_truth', 'formatted', 
                                      'lgc_daily_data_2000-2016_conus.csv'))

# 1e. create key for daily AQS and JS
create_baseModelKey(ref.df = aqs, refName = 'aqs', baseModel.df = js.loc, 
                    baseModelName = 'js', timeScale = 'daily') 

## ------------------------ ##
#### 2. annual aqs and JS ####
## ------------------------ ##

# 2a. create key for conus refGrid
# 2a.i. read the refgrid
refGrid <- fst::read_fst(here::here('data_ancillary', 'generated',  
                            paste0('refGrid_', 'conus', '.fst')))
# 2a.ii. create the key 
create_baseModelKey(refGrid, 'refGridConus', js.loc, 'js', 'daily') 
create_baseModelKey(refGrid, 'refGridConus', js.loc, 'js', 'annual') 

# 2b. create key for NYS refGrid
# 2b.i. read the refgrid
refGrid <- fst::read_fst(here::here('data_ancillary', 'generated',  
                                    paste0('refGrid_', 'NYS', '.fst')))
# 2b.ii. create the key 
create_baseModelKey(refGrid, 'refGridNYS', js.loc, 'js', 'daily') 
create_baseModelKey(refGrid, 'refGridNYS', js.loc, 'js', 'annual') 

# 2c. create key for cities refGrid
# 2c.i. read the refgrid
refGrid <- fst::read_fst(here::here('data_ancillary', 'generated',  
                                    paste0('refGrid_', 'cities', '.fst')))
# 2c.ii. create the key 
create_baseModelKey(refGrid, 'refGridcities', js.loc, 'js', 'daily') 
create_baseModelKey(refGrid, 'refGridcities', js.loc, 'js', 'annual') 
