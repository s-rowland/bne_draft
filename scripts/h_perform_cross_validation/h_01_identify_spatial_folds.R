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

# 1a Pick Year
YYYY <- 2010

# 1b Readin training dataset
train <- read_csv(here::here('BNE_inputs', 'training_data', 'combined', 
                                paste0('Training_annual_', YYYY, '_','avgscmjscc', '_all', '.csv')))

# 2d Convert to simple features
# 2d.i keep df version 
train.df <- train
train <- train %>% 
  st_as_sf(., coords = c("lon", "lat"), crs=st_crs('epsg:4326')) %>% 
  st_transform(., crs=st_crs(projString))

# 2e Add EPA Regions
# 2e.i Readin EPA region table
epaRegion <- read_csv(here::here('ancillary_data', 'final', 'epaRegions.csv'))
# 2e.ii Readin state spatial data
states <- read_sf(here::here('ancillary_data', 'raw', 'Census','cb_2015_us_state_500k', 
                             'cb_2015_us_state_500k.shp')) %>% 
  st_transform(., crs=st_crs(projString))
# 2e.iii Calculate area 
states <- states %>% 
  mutate(area = ALAND + AWATER) %>% 
  filter(!(STUSPS%in%c('HI', 'AK')))
# 2e.iv Combine 
states <- states %>% 
  rename(state = STUSPS) %>%
  dplyr::select(-AFFGEOID, -ALAND, -AWATER, -GEOID, -LSAD, -NAME, 
                -STATEFP, -STATENS) %>% 
  inner_join(epaRegion, by = 'state')
# 2e.v Join
train <- st_intersection(states, train, join = st_intersects)

# 2f Clean up 
rm(epaRegion)

####**********************
#### 3: Set Threshold ####
####**********************

# 3a Set Threshold 
# in km 
threshold <- 10000

####***********************************************
#### 4: Create Folds From Hierarchical Cluster ####
####***********************************************

# 4a Create hierarchial clusters (not the folders) 
# 4a.i Calculate euclidean distance between all combination of points
# we will use the projected locations in meters
train.loc <- train %>% 
  as.data.frame() %>% 
  separate(geometry, c('y', 'x'), sep = ',') %>% 
  mutate(y = str_sub(y, 3,), x = str_sub(x, 0, -2)) %>% 
  mutate(y = as.numeric(y), x = as.numeric(x)) %>% 
  dplyr::select(y, x)

df.dist <- dist(train.loc, method = "euclidean")
rm(train.loc)
# 4a.ii Create hierarchial cluster solution
# must use single linkage!!
hc.complete <- hclust(df.dist, method = "single")
# 4a.iii Cut tree
hc.cluster <- cutree(hc.complete, h = threshold)
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
  group_by(hc) %>% 
  summarize(Mon = n()) %>% 
  arrange(desc(Mon))
# 4b.ii Assign each of the largest clusters to a fold
fold_df <- hclust %>% 
  slice(1:10) %>% 
  mutate(fold = 1:10)
# 4b.iii Remove those clusters from the dataframe of available clusters 
# which we call hclust.sm
hclust.sm <- hclust %>%
  slice(11:nrow(hclust)) %>% 
  mutate(fold = 0)

# 4b.iv Assign over a loop 
for (i in 1:nrow(hclust.sm)){
  #i <- 1
  # identify the current smallest fold
  smallestFold <- fold_df %>% 
    group_by(fold) %>% 
    summarize(Count = sum(Mon)) %>%
    arrange(Count) %>% 
    slice(1:1)
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
  inner_join(fold_df, by = 'hc')

# 4d Make fold a string
train <- train %>% 
  mutate(fold = paste0('fold', str_pad(fold, 2, 'left', '0')))

# 4e Make big dataset 

# split train into leaveOut dataframes
folds.list <- split(train, train$fold)

# create function to do the n+1 removal 
createFoldSets <- function(leaveOut){
  # leaveOut <- folds.list[[2]]
  
  leaveIn <- train %>% 
    filter(fold != leaveOut$fold[1]) %>% 
    mutate(role = 'Train')
  a <- leaveOut %>% 
    mutate(role = 'Test') %>% 
    bind_rows(., leaveIn) %>% 
    mutate(fold = leaveOut$fold[1])
}

# actually make them 
trainFolds <- map(folds.list, createFoldSets)

# combine 
trainFolds <- bind_rows(trainFolds)

# add column fo fold stype 
trainFolds.hc <- trainFolds %>% 
  mutate(foldMethod = paste0('hc', threshold/1000))

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
  st_transform(., crs=st_crs('epsg:4326')) 
trainFolds.hc <- trainFolds.hc %>%
  as.data.frame() %>%
  mutate(lon = st_coordinates(trainFolds.hc)[,1], 
         lat = st_coordinates(trainFolds.hc)[,2]) %>% 
  mutate(time = YYYY) 

# 5b Make a function to save the training and testing components of the fold
# 5b.i Begin function
save_folds <- function(foldNum){
  #foldNum <- 1
  # 5b.ii Make the name of the fold
  activeFold <- paste0('fold', str_pad(foldNum, 2, 'left', '0'))
  # 5b.iii Save the training dataset
  trainFolds.hc %>% 
    filter(fold == activeFold) %>% 
    filter(role == 'Train') %>% 
    dplyr::select(lat, lon, time, aqs, AV, GS, CM, JS, CC, cellID) %>%
    write_csv(here::here('BNE_inputs', 'training_data', 'combined', 
                         paste0('Training_annual_', YYYY, '_','avgscmjscc', '_', activeFold, '.csv')))
  # 5b.iv First make the testing dataset
  testFold <- trainFolds.hc %>% 
    filter(fold == activeFold) %>% 
    filter(role == 'Test') 
  # 5b.v Then save it 
  testFold %>% 
    dplyr::select(lon, lat, time, AV, GS, CM, JS, CC, aqs) %>%
    write_csv(here::here('BNE_inputs', 'input_models', 'combined', 'annual',
                         paste0('Predictions_', YYYY, '_' , 'avgscmjscc', '_', activeFold, '.csv')))
  
  # 5b.vi Then save the number of observations
  data.frame(Count = nrow(testFold)) %>%
    write_csv(here::here('BNE_inputs', 'input_models', 'combined', 'annual',
                         paste0('PredCount_', YYYY, '_' , 'avgscmjscc', '_', activeFold, '.csv')))
}

# now actually make the folds
map(c(1:10), save_folds)

#check <- trainFolds.hc %>% group_by(foldMethod, fold, role) %>% summarize(count =n())
