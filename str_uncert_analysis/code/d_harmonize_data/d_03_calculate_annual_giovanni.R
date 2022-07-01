# File: b_06_conduct_external_validation.R
# Authors:
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 03/17/22
#
# Contents:
#  N. notes
#  0. preparation
#  1. download PRISM data
#  2. calculate seasonal averages

#### ------------------- ####
####  Table of Contents  ####
#### ------------------- ####

# N: Notes
# 0: Preparation 
# 1: 

#### ---------- ####
####  N: NOTES  ####
#### ---------- ####

# This script takes about X amount of time


#### ---------------- ####
####  0: Preparation  ####
#### ---------------- ####

# 0.a. import relevant packages, etc, based on whole project
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up',
                    'a_00_import_packages_set_global_objects.R'))
}

# 0.b. import packages and set objects specific to this subproject
if(!exists('ran_a_00_uncert')){
  here::i_am('README.md')
  source(here::here('str_uncert_analysis', 'code', 
                    '0_00_config_env_uncert_analysis.R'))
}

#### --------------- ####
####   2. Main Loop  ####
#### --------------- ####


yyyy.list <- 2010:2015


refGrid <- fst::read_fst(here::here('inputs', 'pm25', 'reference_grids',  
                                         paste0('refGrid_', 'conus', '.fst')))
yyyy <- 2010
cloud <- raster(here::here(dir.proj, 'data', 'explanatory_variables', 'raw', 'GIOVANNI', 
                       paste0('GIOVANNI-timeAvgMap_MYD08_D3_6_1_Cloud_Fraction_Mean_', yyyy, 
                              '0101-', yyyy, 
                              '1231_130W_24N_65W_52N.tif')))

# 6b Convert projection
cloud <- projectRaster(cloud, crs = crs(projCRS.ras))

# 6c Compute value for cloud cover
cloud.ref <- raster::extract(cloud, refGrid, 
                             df = TRUE, factors = TRUE, fun = mean) 

# yay prism
 prism<- raster(here::here(dir.proj, 'data', 'explanatory_variables', 'raw', 'PRISM', 
                           paste0('GIOVANNI-timeAvgMap_MYD08_D3_6_1_Cloud_Fraction_Mean_', yyyy, 
                                  '0101-', yyyy, 
                                  '1231_130W_24N_65W_52N.tif')))

# 6b Convert projection
cloud <- projectRaster(cloud, crs = crs(projCRS.ras))

# 6c Compute value for cloud cover
cloud.ref <- raster::extract(cloud, refGrid, 
                             df = TRUE, factors = TRUE, fun = mean)
