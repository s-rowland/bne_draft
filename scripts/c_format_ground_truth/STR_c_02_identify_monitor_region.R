# Robbie: Wasn't totally sure what this script did to be honest. Might be worth a quick discussion on Zoom one time.

# Identify Spatial Folds
# Prepare BNE Inputs
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Read Training Data
# 3: Set Threshold
# 4: Create Folds From Hierarchical Cluster

####**************
#### N: Notes ####
####**************


####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_set_up_env.R"))
}

####***************************
#### 1: Read Training Data ####
####***************************

# 1a. list of years 
YYYYlist <- 2010:2015

# 1b. function to read in the aqs+js datasets
read_aqs <- function(YYYY){
  readr::read_csv(here::here('BNE_inputs', 'training_datasets', 'individual_annual', 
                             paste0('training_avgscmjscc_', YYYY, '_all.csv'))) 
}

# 1c. combined years of aqs data
train.full <- purrr::map(YYYYlist, read_aqs) %>% 
  bind_rows()

# 1d. keep only unique location 
train <- train.full %>% 
  dplyr::select(ref_id, lat, lon) %>% 
  dplyr::distinct()

# 2d Convert to simple features
# 2d.i keep df version 
train.df <- train
train <- train %>% 
  sf::st_as_sf(., coords = c("lon", "lat"), crs=st_crs('epsg:4326')) %>% 
  sf::st_transform(., crs=st_crs(projString))

# 2e Add EPA Regions
# 2e.i Readin EPA region table
epaRegion <- readr::read_csv(here::here('data_ancillary', 'final', 'epaRegions.csv'))
# 2e.ii Readin state spatial data
states <- sf::read_sf(here::here('data_ancillary', 'raw', 'Census','cb_2015_us_state_500k', 
                             'cb_2015_us_state_500k.shp')) %>% 
  st_transform(., crs=st_crs(projString))
# 2e.iii Calculate area 
states <- states %>% 
  dplyr::mutate(area = ALAND + AWATER) %>% 
  dplyr::filter(!(STUSPS%in%c('HI', 'AK', 'VI')))
# 2e.iv Combine 
states <- states %>% 
  dplyr::rename(state = STUSPS) %>%
  dplyr::select(-AFFGEOID, -ALAND, -AWATER, -GEOID, -LSAD, -NAME, -STATEFP, -STATENS) %>% 
  dplyr::inner_join(epaRegion, by = 'state')
# 2e.v Join
train <- sf::st_intersection(states, train, join = st_intersects)

# 2f Clean up 
rm(epaRegion)

####**********************
#### 3: Set Threshold ####
####**********************

# 3a Set Threshold 
# in km 
# Robbie: Can you explain what the threshold means here please?
threshold <- 10000

####***********************************************
#### 4: Create Folds From Hierarchical Cluster ####
####***********************************************

# 4a Create hierarchial clusters (not the folders) 
# 4a.i Calculate euclidean distance between all combination of points
# we will use the projected locations in meters
train.loc <- train %>% 
  as.data.frame() %>% 
  tidyr::separate(geometry, c('y', 'x'), sep = ',') %>% 
  dplyr::mutate(y = str_sub(y, 3,), x = str_sub(x, 0, -2)) %>% 
  dplyr::mutate(y = as.numeric(y), x = as.numeric(x)) %>% 
  dplyr::select(y, x)

df.dist <- stats::dist(train.loc, method = "euclidean")
rm(train.loc)
# 4a.ii Create hierarchial cluster solution
# must use single linkage!!
hc.complete <- stats::hclust(df.dist, method = "single")
# 4a.iii Cut tree
hc.cluster <- stats::cutree(hc.complete, h = threshold)
# this shows us the number of members in each cluster
#table(hc.cluster) 
# 4a.iv Assign h cluster 
train$hc <- hc.cluster

# 4b Assign Monitors to Folds
# we use the greedy algorithm described by Just et. al. 
# 4b.i Determine the number of monitors in each spatial cluster
hclust <- train %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry) %>% 
  dplyr::group_by(hc) %>% 
  dplyr::summarize(Mon = n()) %>% 
  dplyr::arrange(desc(Mon))
# 4b.ii Assign each of the largest clusters to a fold
fold_df <- hclust %>% 
  dplyr::slice(1:10) %>% 
  dplyr::mutate(fold = 1:10)
# 4b.iii Remove those clusters from the dataframe of available clusters 
# which we call hclust.sm
hclust.sm <- hclust %>%
  dplyr::slice(11:nrow(hclust)) %>% 
  dplyr::mutate(fold = 0)

# 4b.iv Assign over a loop 
for (i in 1:nrow(hclust.sm)){
  #i <- 1
  # identify the current smallest fold
  smallestFold <- fold_df %>% 
    dplyr::group_by(fold) %>% 
    dplyr::summarize(Count = sum(Mon)) %>%
    dplyr::arrange(Count) %>% 
    dplyr::slice(1:1)
  # add assign the next largest cluster to the smallest fold
  hclust.sm$fold[1] <- smallestFold$fold[1]
  # add that assigned cluster to assigned cluster pile 
  fold_df <- fold_df %>% 
    bind_rows(hclust.sm[1,])
  # remove that assigned cluster from unassigned cluster pile
  hclust.sm <- hclust.sm[2:nrow(hclust.sm),]
}
# 4c.v Assign monitors to folds according to their hcluster 
train <- train %>% 
  dplyr::inner_join(fold_df, by = 'hc')

# 4d Make fold a string
train <- train %>% 
  dplyr::mutate(fold = paste0('fold', str_pad(fold, 2, 'left', '0')))

# 4e Make big dataset 

# split train into leaveOut dataframes
folds.list <- split(train, train$fold)

# create function to do the n+1 removal 
createFoldSets <- function(leaveOut){
  # leaveOut <- folds.list[[2]]
  
  leaveIn <- train %>% 
    dplyr::filter(fold != leaveOut$fold[1]) %>% 
    dplyr::mutate(role = 'Train')
  a <- leaveOut %>% 
    dplyr::mutate(role = 'Test') %>% 
    dplyr::bind_rows(., leaveIn) %>% 
    dplyr::mutate(fold = leaveOut$fold[1])
}

# actually make them 
trainFolds <- purrr::map(folds.list, createFoldSets) %>%
  bind_rows()

# add column fo fold stype 
trainFolds.hc <- trainFolds %>% 
  dplyr::mutate(foldMethod = paste0('hc', threshold/1000))

####*******************
#### 5: Save Folds ####
####*******************

# 5a add the original training data in 
#a <- train %>% 
 # mutate(foldMethod = 'all', fold = 'all', role = 'Train')  %>% 
  #bind_rows(aqsFolds.hc) %>% 
 # dplyr::select(geometry, state, region, aqs, ID, foldMethod, fold, role) %>% 

# 5a Convert the fold dataset to a dataframe from simple features
trainFolds.hc <- trainFolds.hc %>% 
  sf::st_transform(., crs=st_crs('epsg:4326')) 
trainFolds.hc <- trainFolds.hc %>%
  dplyr::mutate(lon = st_coordinates(.)[,1], 
         lat = st_coordinates(.)[,2]) %>%
  as.data.frame() 

# 5b Make a function to save the training and testing components of the fold
# 5b.i Begin function
save_folds <- function(foldNum, YYYY){
  #foldNum <- 1; YYYY <- 2010
  # 5b.ii Make the name of the fold
  activeFold <- paste0('fold', str_pad(foldNum, 2, 'left', '0'))
  # 5b.iii isolate the fold of interest 
  train.full.yyyy <- train.full %>% 
    dplyr::filter(year == YYYY)
  # and add the observations 
  train.fold <- trainFolds.hc %>% 
    dplyr::select(ref_id, fold, state, role) %>%
    dplyr::filter(fold == activeFold) %>% 
    dplyr::inner_join(train.full.yyyy, by = 'ref_id')
  
  # 5b.iii Save the training dataset
  train.fold %>% 
    dplyr::filter(role == 'Train') %>% 
    dplyr::select(lat, lon, obs_pm2_5, av_pred, gs_pred, cmaq_outs_pred, js_pred, caces_pred, ref_id) %>%
    readr::write_csv(here::here('BNE_inputs', 'training_datasets', 'individual_annual', 
                         paste0('training_annual_avgscmjscc_', YYYY, '_', activeFold, '.csv')))
 
   # 5b.iv save the testing dataset
  testFold <- train.fold %>% 
    dplyr::filter(role == 'Test') %>% 
    dplyr::select(lat, lon, av_pred, gs_pred, cmaq_outs_pred, js_pred, caces_pred, obs_pm2_5,ref_id) %>%
    readr::write_csv(here::here('BNE_inputs', 'prediction_datasets', 'individual_annual',
                         paste0('predictions_avgscmjscc_', YYYY, '_', activeFold, '.csv')))
  
  # 5b.vi Then save the number of observations
  data.frame(Count = nrow(testFold)) %>%
    readr::write_csv(here::here('BNE_inputs', 'prediction_datasets', 'individual_annual', 
                         paste0('predCount_avgscmjscc_', YYYY, '_', activeFold, '.csv')))
}

# now actually make the folds
purrr::map2(rep(c(1:10), 6), sort(rep(2010:2015, 10)), save_folds)

#check <- trainFolds.hc %>% group_by(foldMethod, fold, role) %>% summarize(count =n())

