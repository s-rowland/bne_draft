# Join Input Models 
# Prepare BNE Inputs
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Readin Training Data
# 2: Join JS Predictions
# 3: Join CACES
# 4: Save Training Dataset

####**************
#### N: Notes ####
####**************

# Right now we are using training and avgscm prediction datasets inherited 
# from the Capstone students' project, so the code it not yet 100% reproducible

# right now we use the nearest neighrbor function rather than spatial overlaps 
# creating polygons of the JS grid exhausts the memory of R. 
# Potentially this could be done in Python. 
# Since everything is a grid, the nearest neighbor function is equivalent to 
# doing a spatial intersection, 
# where we treat the non-JS cells as polygons, and the JS cells as points. 
# This approach is nearly equivalent to spatial overlap of two polygons 
# the only distinction occurs when a JS grid cell overlaps two grid cells of an 
# input model. Given how small the JS cells are, and how smooth PM2.5 is across space 
# this shouldn't make too much of a difference. 

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.rtf")
  source(here::here('scripts', 'a_set_up', "a_00_setUp_env.R"))
}

# 0d Set seed 
set.seed(1234)



####******************************
#### 1: Wrangle Training Data ####
####******************************

# 1a Read AQS data
aqs <- read_csv(here::here('data_ground_truth', 'formatted',
                             paste0('aqs_annual_formatted.csv'))) 

# 1b Read conus shapefile
conus <- st_read(here::here('data_ancillary', 'formatted', 'spatial_outlines', 'conus.shp'), 
                 crs = projString)

# 1c Make training datasets 
# we use a loop and source because loading big datasets within a function eats too 
# much memory
for(i in 2010:2015){
  YYYY <- i
  source(here::here('scripts', 'd_format_training_data', 
                    "d_01a_join_inputs_training_script.R"))
}
