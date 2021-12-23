# File: STR_b_01b_create_refGrids_noJS.R
# Author: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 12/22/2021
#
# Contents:
# N. notes
# 0. import packages and global variables
# 1. create refGrids

#----------------#
#### N. notes ####
#----------------#

# This script takes about 16 minutes to run. 

#-----------------------------------------------#
#### 0. import packages and global variables ####
#-----------------------------------------------#

# 0a Load packages and functions required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_config_env.R"))
}

#--------------------------#
#### 1. create refGrids ####
#--------------------------#

create_refGrid_noJS(AOI = 'conus', makePlot = TRUE, targetDir = '~/Desktop')
create_refGrid_noJS(AOI = 'NYS', makePlot = TRUE, targetDir = '~/Desktop')
create_refGrid_noJS(AOI = 'cities', makePlot = TRUE, targetDir = '~/Desktop')
