# Join Input Models 
# Prepare BNE Inputs
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Wrangle AQS Data
# 4: Find AV Grid Cells Nearest to Each Monitor Location
# 4: Find GS Grid Cells Nearest to Each Monitor Location
# 4: Find CM Grid Cells Nearest to Each Monitor Location
# 3: Find JS Grid Cells Nearest to Each Monitor Location
# 4: Find CACES Grid Cells Nearest to Each Monitor Location
# 5: Make Training Datasets

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
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_setUp_env.R"))
}

# 0d Set seed 
set.seed(1234)

# 0e Set the time that we will use to pull the first round of the input models. 
refTime <- 2010

####******************************
#### 1: Wrangle Training Data ####
####******************************

# 1a Read AQS data
aqs <- read_csv(here::here('BNE_inputs', 'groundTruth', 'formatted',
                             paste0('aqs_annual_formatted.csv'))) 

# 1b Read conus shapefile
conus <- st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 'conus.shp'), 
                 crs = projString)

# add ID 
monitorLocations <- aqs %>% 
  dplyr::select(lat, lon) %>% 
  distinct() %>%
  mutate(monID = row_number())

# make simple feature
monitorLocations.sf <- monitorLocations %>% 
  st_as_sf(coords = c("lon", "lat"), crs=st_crs('epsg:4326')) %>% 
  st_transform(crs=st_crs(projString))

####*******************************
#### 2: Wrangle AV Predictions ####
####*******************************

# need the original AV data in order to do this properly






####*******************************
#### 3: Wrangle JS Predictions ####
####*******************************

# honestly, we could turn this bit into a function too. 
# 3a Readin JS
js <- read_fst(here::here('BNE_inputs', 'inputModels', 'formatted', 'JS_annual_formatted',
                          paste0('JS_annual_', refTime, '_formatted.fst'))) 

# 3b convert JS to simple features
js.point.sf <- js %>% 
  st_as_sf(coords = c("lon", "lat"), crs=st_crs('epsg:4326')) %>% 
  st_transform(crs=st_crs(projString))

# 3c Assign JS predictions to training data 
# 3c.i Identify which JS predict pairs to each monitoring location 
monitorLocations$JScellIndex <- unlist(st_nn(monitorLocations.sf, js.point.sf, k=1))

####***************************************************************
#### 4: Find CACES Grid Cells Nearest to Each Monitor Location ####
####***************************************************************

# 4a Readin JS
caces <- readr::read_csv(here::here('BNE_inputs', 'inputModels',  'raw', 'CC_annual_raw',
                                    paste0('CACES_annual_', refTime, '_blockGrp_raw.csv')))

# 4b convert JS to simple features
caces.point.sf <- caces %>% 
  st_as_sf(coords = c("lon", "lat"), crs=st_crs('epsg:4326')) %>% 
  st_transform(crs=st_crs(projString))

# 4c Assign JS predictions to training data 
# 4c.i Identify which JS predict pairs to each monitoring location 
monitorLocations$CCcellIndex <- unlist(st_nn(monitorLocations.sf, caces.point.sf, k=1))

####*******************************
#### 5: Make Training Datasets ####
####*******************************

# 1c Make training datasets 
# we use a loop and source because loading big datasets within a function eats too 
# much memory
for(timeStep in 2010:2015){
  source(here::here('scripts', 'd_format_training_data', 
                    "d_01a_join_inputs_training_script.R"))
}
