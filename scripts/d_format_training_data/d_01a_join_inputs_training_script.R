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
#YYYY <- 2010

# 0d Set seed 
set.seed(1234)

####******************************
#### 1: Wrangle Training Data ####
####******************************

# 1a Readin training data 
train <- aqs %>% 
  filter(time == YYYY)

# 1b Rename columns 
train <- train %>% 
  rename(aqs = pm25_obs, 
         AV = pred_AV, GS = pred_GS, SC = pred_SC, CM = pred_CM) %>% 
  dplyr::select(-SC)

# 1c Create simple features version of training data 
train.point.sf <- train %>% 
  st_as_sf(., coords = c("lon", "lat"), crs=st_crs('epsg:4326'))  %>% 
  st_transform(crs=st_crs(projString))

# 1e Restrict to conus
conus4326 <- conus %>% 
  st_transform(crs=st_crs(projString))

train.point.sf  <- train.point.sf  %>% 
  st_join(conus4326, st_intersects) %>% 
  filter(!is.na(g)) %>% 
  dplyr::select(-g, -m)

# 1f Project points
train.point.sf <- train.point.sf %>% 
  st_transform(crs=st_crs(projString))

####*******************************
#### 2: Wrangle JS Predictions ####
####*******************************

# 2a Readin JS
js <- read_fst(here::here('data_input_models', 'formatted', 'JS_annual_formatted',
                          paste0('JS_annual_', YYYY, '_formatted.fst'))) 

# 2b convert JS to simple features
js.point.sf <- js %>% 
  st_as_sf(coords = c("lon", "lat"), crs=st_crs('epsg:4326')) %>% 
  st_transform(crs=st_crs(projString))

# 2c Assign JS predictions to training data 
# 2c.i Identify which JS predict pairs to each monitoring location 
train.point.sf$cellIndex <- unlist(st_nn(train.point.sf, js.point.sf, k=1))
js <- js.point.sf %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry) %>% 
  mutate(cellIndex = row_number())
# 2c.iii Combine 
train.point.sf <- train.point.sf %>% 
 inner_join(js, by = 'cellIndex') %>% 
  dplyr::select(-cellIndex)

# 2d clean up 
rm(js)

####**********************
#### 3: Wrangle CACES ####
####**********************

# 3a Readin CACES
caces <- readr::read_csv(here::here('data_input_models', 'raw', 'CC_annual_raw',
                                    paste0('CACES_annual_', YYYY, '_blockGrp_raw.csv')))

# 3b Rename columns 
caces <- caces %>% 
  rename(CC = pred_wght) 

# 3c Keep only columns of interest 
caces <- caces %>% 
  dplyr::select(lat, lon, CC)

# 3d Convert caces to simple features
caces.point.sf <- st_as_sf(caces, coords = c("lon", "lat"), 
                            crs=st_crs('epsg:4326')) %>% 
  st_transform(crs=st_crs(projString))

# 3e Restrict to conus
#caces.point.sf <- caces.point.sf %>% 
  #st_join(conus, st_intersects) %>% 
  #filter(!is.na(g)) %>% 
 # dplyr::select(-g, -m) %>% 
  #mutate(cellIndex = row_number())

# 3f Join to training data
# 3f.i Identify which CACES cell pairs to each monitoring location 
train.point.sf$cellIndex <- unlist(st_nn(train.point.sf, caces.point.sf, k=1))
caces <- caces.point.sf %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry) %>% 
  mutate(cellIndex = row_number())
# 3f.iii Combine 
train.point.sf <- train.point.sf %>% 
  inner_join(caces, by = 'cellIndex') %>% 
  dplyr::select(-cellIndex)

####******************************
#### 4: Save Training Dataset ####
####******************************

# 4a Convert training dataset to dataframe 
train.point.sf <- train.point.sf %>% 
  st_transform(., crs=st_crs('epsg:4326')) 
train.point <- train.point.sf %>% 
  as.data.frame() %>% 
  mutate(lon = st_coordinates(train.point.sf)[,1], 
         lat = st_coordinates(train.point.sf)[,2])
  
# 4b Randomize the order of the rows 
# this helps us with cross validation later on
rows <- sample(nrow(train.point))
train.point <- train.point[rows,]

# 4c Save dataset
train.point %>% 
  mutate(cellID = row_number()) %>%
  dplyr::select(lat, lon, time, aqs, AV, GS, CM, JS, CC, cellID) %>%
  write_csv(here::here('data_training', 'combined', 
                       paste0('Training_annual_', YYYY, '_','avgscmjscc', '_all', '.csv')))
