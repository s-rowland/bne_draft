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
  source(here::here('scripts', 'a_set_up', "a_00_setUp_env.R"))
}

# 0b Set Year 
#YYYY <- 2011

####****************************
#### 1: Read Reference Grid ####
####****************************

# 1a Read Reference grid file
refGrid <- read_fst(here::here('data_ancillary', 'final', 
                               'refGrid_JS_1percent.fst'))

# 1b Convert to simple features 
# note: the st_nn() takes much much longer if you use lat/lon instead of projected points. 
refGrid <- refGrid %>% 
  st_as_sf(., coords = c("lon", "lat"), crs=st_crs('epsg:4326')) %>% 
  st_transform(crs= st_crs(projString))

####***********************************
#### 2: Wrangle AVGSCM Predictions ####
####***********************************

# 2a Readin predictions dataset
avgscm  <- read_csv(here::here('data_input_models', 'formatted', 'AVGSCM_annual_formatted',
                               paste0('avgscm_', YYYY, '.fst'))) 

# 2b Rename columns 
avgscm <- avgscm %>% 
  mutate(cellIndex = row_number())

# 2c Convert avgscm to simple features
avgscm.point.sf <- st_as_sf(avgscm, coords = c("lon", "lat"), 
                            crs=st_crs('epsg:4326')) %>% 
  st_transform(crs= st_crs(projString))

# 2d Restrict to conus
# only removes 5 obs ... not worth it
#avgscm.point.sf <- avgscm.point.sf %>% 
#  st_join(conus, st_intersects) %>% 
  #filter(!is.na(g)) %>% 
  #dplyr::select(-g, -m)

# 2e Assign to Reference grid 
# 2e.i For each Reference grid cell, identify the nearest avgscm point. 
refGrid$cellIndex <- unlist(st_nn(refGrid, avgscm.point.sf, k=1))

# make non-spatial version
avgscm <- avgscm.point.sf %>%
  as.data.frame() %>% 
  dplyr::select(-geometry) %>% 
  mutate(cellIndex = row_number())
# 2e.iii Combine 
refGrid <- refGrid %>% 
  inner_join(avgscm, by = 'cellIndex') %>% 
  dplyr::select(-cellIndex)

# 2f Clean up 
rm(avgscm)

####**********************************
#### 3: Link JS to Reference Grid ####
####**********************************

# 3a Read JS 
js <- read_fst(here::here('data_input_models', 'formatted', 'JS_annual_formatted',
                          paste0('JS_annual_', YYYY, '_formatted.fst')))

# 3b Convert to simple features
js.sf <- js %>% 
  st_as_sf(., coords = c("lon", "lat"), crs=st_crs('epsg:4326')) %>% 
  st_transform(crs= st_crs(projString))

# 3c Combine
refGrid$cellIndex <- unlist(st_nn(refGrid, js.sf, k=1))
js <- js.sf %>% 
  mutate(cellIndex = row_number()) %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

refGrid <- refGrid %>% 
  inner_join(js, by = 'cellIndex')
rm(js)
####*************************************
#### 4: Link CACES to Reference Grid ####
####*************************************

# 5a Readin CACES
# so while we are doing extra tasks, we will not create any bad data
caces <- readr::read_csv(here::here('data_input_models', 'raw', 'CC_annual_raw',
             paste0('CACES_annual_', YYYY, '_blockGrp_raw.csv')))

# 5b Rename columns 
caces <- caces %>% 
  rename(CC = pred_wght) 

# 5c Keep only columns of interest 
caces <- caces %>% 
  dplyr::select(lat, lon, CC)

# 5d Convert caces to simple features
caces.sf <- st_as_sf(caces, coords = c("lon", "lat"), 
                            crs=st_crs('epsg:4326')) %>% 
  st_transform(crs= st_crs(projString))

# 5e Restrict to conus
#caces.point.sf <- caces.point.sf %>% 
 # st_join(conus, st_intersects) %>% 
  #filter(!is.na(g)) %>% 
  #dplyr::select(-g, -m)

# 5f Assign to Reference grid 
# again we will use nearest neighbor, which isn't exactly the best, but okay for now 
# 3c Combine
refGrid$cellIndex <- unlist(st_nn(refGrid, caces.sf, k=1))
caces <- caces.sf %>% 
  mutate(cellIndex = row_number()) %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

refGrid <- refGrid %>% 
  inner_join(caces, by = 'cellIndex')

#a <- Sys.time()
#caces.vor.sp <- voronoi(as_Spatial(caces.point.sf))
#caces.vor.sf <- st_as_sf(caces.vor.sp)
#refGrid <- st_intersection(caces.vor.sf, refGrid, join = st_intersects)
#cacestime <-  Sys.time()-a

rm(caces)

####**********************************
#### 9: Create Prediction Dataset ####
####**********************************

# 9a Save prediction dataset
refGrid <- refGrid %>% 
  st_transform(crs= st_crs('epsg:4326')) 
refGrid %>%
  as.data.frame() %>% 
  mutate(lon = st_coordinates(refGrid)[,1], 
         lat = st_coordinates(refGrid)[,2]) %>%
  mutate(time = YYYY) %>%
  dplyr::select(lon, lat, time, AV, GS, CM, JS, CC) %>%
  write_csv(here::here('data_input_models', 'combined', 'annual',
                       paste0('Predictions_', YYYY, '_' , 'AVGSCMJSCC', '_all.csv')))

# 9b Save the number of observations
data.frame(Count = nrow(refGrid)) %>%
  write_csv(here::here('data_input_models', 'combined', 'annual',
                       paste0('PredCount_', YYYY, '_' , 'AVGSCMJSCC', '_all.csv')))
