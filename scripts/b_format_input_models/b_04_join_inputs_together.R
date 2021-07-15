# Join Input Models 
# Prepare BNE Inputs
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Read Reference Grid
# 2: Link AVGSCM to Reference Grid 
# 3: Link JS to Reference Grid
# 4: Link CACES to Reference Grid
# 5: Save Prediction Dataset

####**************
#### N: Notes ####
####**************

# Right now we are using training and avgscm prediction datasets inherited 
# from the Capstone students' project, so the code it not yet 100% reproducible

# right now we use the nearest neighbor function rather than spatial overlaps 
# creating polygons of the JS grid exhausts the memory of R. 
# Potentially this could be done in Python. 
# Since everything is a grid, the nearest neighbor function is equivalent to 
# doing a spatial intersection, 
# where we treat the non-JS cells as polygons, and the JS cells as points. 
# This approach is nearly equivalent to spatial overlap of two polygons 
# the only distinction occurs when a JS grid cell overlaps two grid cells of an 
# input model. Given how small the JS cells are, and how smooth PM2.5 is across space 
# this shouldn't make too much of a difference. 

# adding the area-weighted averaging, and doing it for daily in a smart way 
# (similar to that temp code), should be the next step

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.rtf")
  source(here::here('scripts', 'a_set_up', "a_00_setUp_env.R"))
}

####****************************
#### 1: Read Reference Grid ####
####****************************

# start 12:40
# 0b Set Year 
for(i in 2010:2015){
  YYYY <- i
  source(here::here('scripts', 'b_format_input_models', 
                    "b_04a_join_inputs_together_script.R"))
}
