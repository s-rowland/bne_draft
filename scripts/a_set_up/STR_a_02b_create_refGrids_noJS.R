# File: STR_b_01b_create_refGrids_noJS.R
# Author: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 01/14/2021
#
# Contents:
#  N. notes
#  0. import packages and set global objects
#  1. create refGrids

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
# Robbie: No inputs/  folder here. Did I miss this as something I had to put into place from your files you sent? I don't see
# Robbie: the files right now...
createRefGridNoJS(AOI = 'conus', makePlot = TRUE, targetDir = 'inputs/pm25/reference_grids')
createRefGridNoJS(AOI = 'NYS', makePlot = TRUE, targetDir = 'inputs/pm25/reference_grids')
createRefGridNoJS(AOI = 'cities', makePlot = TRUE, targetDir = 'inputs/pm25/reference_grids')
createRefGridNoJS(AOI = 'conus01deg', makePlot = TRUE, targetDir = 'inputs/pm25/reference_grids')
