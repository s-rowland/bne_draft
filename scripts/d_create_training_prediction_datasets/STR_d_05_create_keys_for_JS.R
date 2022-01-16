# File: STR_d_01_create_keys_for_JS.R
# Author: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 01/14/2021
#
# Contents:
#  N. Notes
#  0. Import Packages and Set Global Objects
#  1. Create Keys for AQS and JS 
#  2. Create Keys for refGrids and JS 

#### ------------------- ####
####       N. NOTES      ####
#### ------------------- ####

# N.1. goal
# The point of this script is to create various "key" files:
# 
# The keys map EPA monitor locations to
# their nearest neighbour JS <lat, lon> coordinate pairs, along with 
# recording the index of the JS lat-lon pair. In this way, a "key" is made, allowing
# someone to extract only the points of interest from a JS .rda file using the
# "js_index" column from the key. 
# the keys are stored in the inputs/[pollutant]/keys folder, and follow this naming convention: 
# [aqs/refGrid] _JS_key_nn_ [timeWindow]

#### ------------------------------------------- ####
####  0. IMPORT PACKAGES AND SET GLOBAL OBJECTS  ####
#### ------------------------------------------- ####

# 0.a. import packages and global objects, if you haven't already done so
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

#### ------------------------------- ####
####  1: CREATE KEYS FOR AQS AND JS  ####
#### ------------------------------- ####

# 1.a. read js locations
jsPath <- here::here('inputs', 'pm25', 'base_models', 'raw', 'JS_annual_raw', 
                     'USGridSite.rds')
js.loc <- loadData(jsPath, "JSSITES") %>% 
  rename(lat = js_lat, lon = js_lon)

# 1.b. read annual AQS
aqs <- read_csv(aqsPath <- here::here('inputs', 'pm25', 'ground_truth', 'formatted', 
                                    'lgc_annual_data_2000-2016_conus.csv'))

# 1.c. create key for annual AQS and JS
create_baseModelKey(ref.df = aqs, refName = 'aqs', baseModel.df = js.loc, 
                    baseModelName = 'js', timeScale = 'annual') 

# 1.d. read daily AQS
aqs <- read_csv(aqsPath <- here::here('inputs', 'pm25', 'ground_truth', 'formatted', 
                                      'lgc_daily_data_2000-2016_conus.csv'))

# 1.e. create key for daily AQS and JS
create_baseModelKey(ref.df = aqs, refName = 'aqs', baseModel.df = js.loc, 
                    baseModelName = 'js', timeScale = 'daily') 

#### ------------------------------------ ####
####  2: CREATE KEYS FOR REFGRIDS AND JS  ####
#### ------------------------------------ ####

# 2.a. create key for conus refGrid
# 2.a.i. read the refgrid
refGrid <- fst::read_fst(here::here('inputs', 'pm25', 'reference_grids',  
                            paste0('refGrid_', 'conus', '.fst')))
# 2.a.ii. create the key 
create_baseModelKey(refGrid, 'refGridConus', js.loc, 'js', 'daily') 
create_baseModelKey(refGrid, 'refGridConus', js.loc, 'js', 'annual') 

# 2.b. create key for NYS refGrid
# 2.b.i. read the refgrid
refGrid <- fst::read_fst(here::here('inputs', 'pm25', 'reference_grids',   
                                    paste0('refGrid_', 'NYS', '.fst')))
# 2.b.ii. create the key 
create_baseModelKey(refGrid, 'refGridNYS', js.loc, 'js', 'daily') 
create_baseModelKey(refGrid, 'refGridNYS', js.loc, 'js', 'annual') 

# 2.c. create key for cities refGrid
# 2.c.i. read the refgrid
refGrid <- fst::read_fst(here::here('inputs', 'pm25', 'reference_grids',  
                                    paste0('refGrid_', 'cities', '.fst')))
# 2.c.ii. create the key 
create_baseModelKey(refGrid, 'refGridcities', js.loc, 'js', 'daily') 
create_baseModelKey(refGrid, 'refGridcities', js.loc, 'js', 'annual') 
