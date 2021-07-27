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
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_set_up_env.R"))
}

# 0b Set Time Step 
#timeStep <- 2011

####********************
#### 1: Rename Grid ####
####*******************
predGrid <- refGrid %>% 
  st_transform(crs= st_crs('epsg:4326')) 
predGrid <- predGrid %>%
  as.data.frame() %>% 
  mutate(lon = st_coordinates(predGrid)[,1], 
         lat = st_coordinates(predGrid)[,2]) %>%
  dplyr::select(-geometry)

####***********************************
#### 2: Wrangle AVGSCM Predictions ####
####***********************************

# 2a Readin predictions dataset
avgscm  <- read_csv(here::here('BNE_inputs', 'input_models', 'formatted', 'AVGSCM_annual_formatted',
                               paste0('avgscm_', timeStep, '.fst'))) 

# 2b Rename columns 
avgscm <- avgscm %>% 
  mutate(cell_index = row_number()) %>% 
  dplyr::select(-lat, -lon)

# 2c Combine 
predGrid <- predGrid%>% 
  inner_join(avgscm, by = c('AVGSCM_cell_index'= 'cell_index')) 

####**********************************
#### 3: Link JS to Reference Grid ####
####**********************************

# 3a Read JS 
js <- read_fst(here::here('BNE_inputs', 'input_models', 'formatted', 'JS_annual_formatted',
                          paste0('JS_annual_', timeStep, '_formatted.fst')))

# 3b Rename columns 
js <- js %>% 
  mutate(cell_index = row_number()) %>% 
  dplyr::select(-lat, -lon)

# 3c Combine 
predGrid <- predGrid%>% 
  inner_join(js, by = c('JS_cell_index'= 'cell_index')) 

# 3d Clean  
rm(js)

####*************************************
#### 4: Link CACES to Reference Grid ####
####*************************************

# 5a Readin CACES
# so while we are doing extra tasks, we will not create any bad data
caces <- readr::read_csv(here::here('BNE_inputs', 'input_models', 'raw', 'CC_annual_raw',
             paste0('CACES_annual_', timeStep, '_blockGrp_raw.csv')))

# 5b Rename columns 
caces <- caces %>% 
  rename(CC = pred_wght) %>% 
  mutate(cell_index = row_number()) %>% 
  dplyr::select(-lat, -lon)

# 3c Combine 
predGrid <- predGrid%>% 
  inner_join(caces, by = c('CC_cell_index'= 'cell_index')) 

# 3d Clean 
rm(caces)

####**********************************
#### 9: Create Prediction Dataset ####
####**********************************

# 9a Save prediction dataset
predGrid %>%
  mutate(time = timeStep) %>%
  dplyr::select(lon, lat, time, AV, GS, CM, JS, CC) %>%
  write_csv(here::here('BNE_inputs', 'input_models', 'combined', 'annual',
                       paste0('Predictions_', timeStep, '_' , 'AVGSCMJSCC', '_all.csv')))

# 9b Save the number of observations
data.frame(Count = nrow(refGrid)) %>%
  write_csv(here::here('BNE_inputs', 'input_models', 'combined', 'annual',
                       paste0('PredCount_', timeStep, '_' , 'AVGSCMJSCC', '_all.csv')))
