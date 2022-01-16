# File: STR_b_01b_create_refGrids_noJS.R
# Author: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 01/14/2021
#
# Contents:
#  N. Notes
#  0. Import Packages and Set Global Objects
#  1. Create refGrids

#### --------- ####
#### N. NOTES  ####
#### --------- ####

# N.1 runtime 
# This script takes about 16 minutes to run. 

#### ------------------------------------------- ####
####  0. IMPORT PACKAGES AND SET GLOBAL OBJECTS  ####
#### ------------------------------------------- ####

# 0.a. import packages and global objects, if you haven't already done so
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}


#### ------------------- ####
#### 1. CREATE REFGRIDS  ####
#### ------------------- ####

# 1.a. create refGrids for the three principal areas of interest: CONUS, NYS, and select cities
create_refGrid_noJS(AOI = 'conus', makePlot = TRUE, targetDir = 'inputs/pm25/reference_grids')
create_refGrid_noJS(AOI = 'NYS', makePlot = TRUE, targetDir = 'inputs/pm25/reference_grids')
create_refGrid_noJS(AOI = 'cities', makePlot = TRUE, targetDir = 'inputs/pm25/reference_grids')
