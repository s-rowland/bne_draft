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

# 0b Set Year 
#timeStep <- 2010

# 0d Set seed 
set.seed(1234)

####******************************
#### 1: Wrangle Training Data ####
####******************************

# 1a Readin training data 
train <- aqs %>% 
  filter(time == timeStep)

# 1b Rename columns 
# note in future version we drop lines with AV, etc
train <- train %>% 
  rename(aqs = pm25_obs, 
         AV = pred_AV, GS = pred_GS, SC = pred_SC, CM = pred_CM) %>% 
  dplyr::select(-SC)

# 1c Restrict to conus
train <- train %>% 
  inner_join(monitorLocations, by =c('lat', 'lon'))

####*******************************
#### 2: Wrangle JS Predictions ####
####*******************************

# 2a Readin JS
# ideally here we would only read in the JS based on their position, 
# but after a quick search I didn't find way to do this in R 
js <- read_fst(here::here('BNE_inputs', 'inputModels', 'formatted', 'JS_annual_formatted',
                          paste0('JS_annual_', timeStep, '_formatted.fst'))) 

js <- js %>% 
  mutate(cellIndex = row_number()) %>% 
  dplyr::select(JS, cellIndex)
# combine
train <- train %>% 
  inner_join(js, by = c('JScellIndex' = 'cellIndex'))

# 2d clean up 
rm(js)

####**********************
#### 3: Wrangle CACES ####
####**********************

# 3a Readin CACES
caces <- readr::read_csv(here::here('BNE_inputs', 'inputModels', 'raw', 'CC_annual_raw',
                                    paste0('CACES_annual_', timeStep, '_blockGrp_raw.csv')))


caces <- caces %>% 
  rename(CC = pred_wght) %>% 
  mutate(cellIndex = row_number()) %>% 
  dplyr::select(CC, cellIndex)
# combine
train <- train %>% 
  inner_join(caces, by = c('CCcellIndex' = 'cellIndex'))

# 2d clean up 
rm(caces)
####******************************
#### 4: Save Training Dataset ####
####******************************

# 4b Randomize the order of the rows 
# this helps us with cross validation later on
rows <- sample(nrow(train))
train <- train[rows,]

# 4c Save dataset
train %>% 
  mutate(cellID = row_number()) %>%
  dplyr::select(lat, lon, time, aqs, AV, GS, CM, JS, CC, cellID) %>%
  write_csv(here::here('BNE_inputs', 'training', 'combined', 
                       paste0('Training_annual_', timeStep, '_','AVGSCMJSCC', '_all', '.csv')))
