# File: LGC_d_02a_make_training_data.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 08/21/21
#
# Contents:
# 0. Package Imports & Global Variables
# 1. EPA & JS Daily Data
# 2. Setting Up CMAQ, GS, CACES loop
# 3. CMAQ, GS, CACES loop
# 4. AV data loop
# 5. Save outputs

##** ------------------------------------- **##
#### 0. PACKAGE IMPORTS & GLOBAL VARIABLES ####
#### ------------------------------------- ####

# 0a. load packages and functions required for this script
if(!exists("ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', 
                    "a_00_import_packages_set_global_objects.R"))
}

yyyy <- 2008
# set merra path
path.merra <- here::here('inputs','pm25',  'base_models', 'daily', 'raw', 'me', 
paste0('daily', yyyy, 'adjPM25sum_v2.nc'))


dayOfYear <- 1
me <- raster::raster(path.merra, band = as.numeric(dayOfYear))
plot(me)

me <- raster::raster('~/Desktop/merra/daily2005adjPM25sum_v2.nc', band = as.numeric(dayOfYear))
cellStats(me,'max')


