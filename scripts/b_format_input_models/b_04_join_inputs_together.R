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
# 5: Extract Predictions for all Times

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
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_setUp_env.R"))
}

# here I would add code where we do the same sort of join as in 
# the b_04a_join_inputs_together_scripts
# but all we keep are the relevant indicies (row numbers) for each prediction model) 

# 0b Set the time that we will use to pull the first round of the input models. 
refTime <- 2010

####****************************
#### 1: Read Reference Grid ####
####****************************

# 1a Read Reference grid file
refGrid <- read_fst(here::here('BNE_inputs', 'referenceGrid', 
                               'refGrid_JS_1percent.fst'))

# 1b Convert to simple features 
# note: the st_nn() takes much much longer if you use lat/lon instead of projected points. 
refGrid <- refGrid %>% 
  st_as_sf(., coords = c("lon", "lat"), crs=st_crs('epsg:4326')) %>% 
  st_transform(crs= st_crs(projString))

####**************************************
#### 2: Link AVGSCM to Reference Grid ####
####**************************************

# 2a Readin predictions dataset
avgscm  <- read_csv(here::here('BNE_inputs', 'inputModels', 'formatted', 'AVGSCM_annual_formatted',
                               paste0('avgscm_', refTime, '.fst'))) 

# 2b Rename columns 
avgscm <- avgscm %>% 
  mutate(cellIndex = row_number())

# 2c Convert avgscm to simple features
avgscm <- st_as_sf(avgscm, coords = c("lon", "lat"), 
                            crs=st_crs('epsg:4326')) %>% 
  st_transform(crs= st_crs(projString))

# 2d Assign to Reference grid 
# 2d.i For each Reference grid cell, identify the nearest avgscm point. 
refGrid$AVGSCMcellIndex <- unlist(st_nn(refGrid, avgscm, k=1))

# Clean 
rm(avgscm)

####**********************************
#### 3: Link JS to Reference Grid ####
####**********************************

# 3a Read JS 
js <- read_fst(here::here('BNE_inputs', 'inputModels', 'formatted', 'JS_annual_formatted',
                          paste0('JS_annual_', refTime, '_formatted.fst')))

# 3b Convert to simple features
js <- js %>% 
  st_as_sf(., coords = c("lon", "lat"), crs=st_crs('epsg:4326')) %>% 
  st_transform(crs= st_crs(projString))

# 3c Combine
refGrid$JScellIndex <- unlist(st_nn(refGrid, js, k=1))

# 3d Clean 
rm(js)

####*************************************
#### 4: Link CACES to Reference Grid ####
####*************************************

# 4a Readin CACES
caces <- readr::read_csv(here::here('BNE_inputs', 'inputModels', 'raw', 'CC_annual_raw',
                                    paste0('CACES_annual_', refTime, '_blockGrp_raw.csv')))

# 4b Rename columns 
caces <- caces %>% 
  mutate(cellIndex = row_number())

# 4c Convert caces to simple features
caces <- st_as_sf(caces, coords = c("lon", "lat"), 
                   crs=st_crs('epsg:4326')) %>% 
  st_transform(crs= st_crs(projString))

# 4d Assign to Reference grid 
# 4d.i For each Reference grid cell, identify the nearest avgscm point. 
refGrid$CCcellIndex <- unlist(st_nn(refGrid, caces, k=1))

# 4e Clean 
rm(caces)

####******************************************
#### 5: Extract Predictions for all Times ####
####******************************************

# 5a 
for(timeStep in 2010:2015){
  source(here::here('scripts', 'b_format_input_models', 
                    "b_04a_join_inputs_together_script.R"))
}

rm(refTime)
