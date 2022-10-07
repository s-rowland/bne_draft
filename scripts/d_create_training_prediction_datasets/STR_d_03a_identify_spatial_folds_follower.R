# Robbie: It would help if there was a kind of master Rscript which shows the order in which these are done (I assume in order of letter and numbers but would also help visualise what's going on)
# Robbie: Maybe the README would do this job
# Sebastian: Great idea, just added. 

# File: STR_d_03_identify_spatial_folds_follower.R
# Authors:
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 08/03/2022
#
# Contents:
#  0. preparation 
#  1. read training data
#  2. set threshold
#  3. create folds from hierarchical cluster
#  4. assign folds to monitors 

#### ---------------- ####
####  0. preparation  ####
#### ---------------- ####

# 0.a. import packages and global objects, if you haven't already done so
if(!exists("ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', 
                    "a_00_import_packages_set_global_objects.R"))
}

#### ----------------------- ####
####  1. read training data  ####
#### ----------------------- ####

# 1.a. readin in combined annual training data 
#train.full <- readr::read_csv(here::here('inputs', 'pm25',  'training_datasets', 
 #                                        paste0(timeWindow, '_combined'), 
#                           trainingFileName)) 

# 1.b. keep only unique location 
train <- train.full %>% 
  dplyr::select(ref_id, lat, lon) %>% 
  dplyr::distinct()

# 1.c. Convert to simple features
# 1.c.i keep df version 
train.df <- train
train <- train %>% 
  sf::st_as_sf(., coords = c("lon", "lat"), crs=st_crs('epsg:4326')) %>% 
  sf::st_transform(., crs=st_crs(projCRS))

#### ------------------ ####
####  2. set threshold  ####
#### ------------------ ####

# 2.a. set threshold 
# in meters
threshold <- 10000

#### ------------------------------------------- ####
####  3. create folds from hierarchical cluster  ####
#### ------------------------------------------- ####

# we first identify which monitors are near each other via hierarchical clustering 
# we then combine those closers to create folds that are similarly-sized
# 3a Create hierarchial clusters (not the folds) 
# 3a.i Calculate euclidean distance between all combination of points
# we will use the projected locations in meters
train.loc <- train %>% 
  as.data.frame() %>% 
  tidyr::separate(geometry, c('y', 'x'), sep = ',') %>% 
  dplyr::mutate(y = str_sub(y, 3,), x = str_sub(x, 0, -2)) %>% 
  dplyr::mutate(y = as.numeric(y), x = as.numeric(x)) %>% 
  dplyr::select(y, x)

df.dist <- stats::dist(train.loc, method = "euclidean")
rm(train.loc)
# 3a.ii Create hierarchial cluster solution
# must use single linkage!!
hc.complete <- stats::hclust(df.dist, method = "single")
# 3.a.iii Cut tree
hc.cluster <- stats::cutree(hc.complete, h = threshold)
# this shows us the number of members in each cluster
#table(hc.cluster) 
# 3.a.iv Assign h cluster 
train$hc <- hc.cluster

# 3.b Assign Monitors to Folds
# we use the greedy algorithm described by Just et. al. 
# 3.b.i Determine the number of monitors in each spatial cluster
hclust <- train %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry) %>% 
  dplyr::group_by(hc) %>% 
  dplyr::summarize(Mon = n()) %>% 
  dplyr::arrange(desc(Mon))
# 3.b.ii Assign each of the largest clusters to a fold
fold_df <- hclust %>% 
  dplyr::slice(1:10) %>% 
  dplyr::mutate(fold = 1:10)
# 3.b.iii Remove those clusters from the dataframe of available clusters 
# which we call hclust.sm
hclust.sm <- hclust %>%
  dplyr::slice(11:nrow(hclust)) %>% 
  dplyr::mutate(fold = 0)

# 3.b.iv Assign over a loop 
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

# 3.c Assign monitors to folds according to their hcluster 
train <- train %>% 
  dplyr::inner_join(fold_df, by = 'hc')

# 3.d Make fold a string
train <- train %>% 
  dplyr::mutate(fold = paste0('fold', str_pad(fold, 2, 'left', '0')))


#### ----------------------------- ####
####  4. assign folds to monitors  ####
#### ----------------------------- ####

# 4.b. assign roles to original training data 
train.full <- train %>% 
  as.data.frame() %>% 
  dplyr::select(fold, ref_id) %>%
  inner_join(train.full, by = 'ref_id')

# 4.c. save 
train.full <- train.full %>% 
  mutate(fold = as.numeric(str_remove_all(fold, 'fold')))

if (timeWindow == 'daily') { 
  train.full <- train.full %>% 
    dplyr::select(lat, lon, julian_day, percent_of_year, obs,
                  pred_av,pred_cm,  pred_js, pred_me, pred_rk, state, ref_id, fold, max_doy) 
}

if (timeWindow == 'annual') { 
  train.full <- train.full %>% 
    rename(yyyy = date_local) %>%
    dplyr::select(lat, lon, yyyy, obs,
                  pred_av, pred_cc, pred_cm, pred_gs,  pred_js, pred_me, pred_rk, 
                  state, ref_id, fold) 
}

if (timeWindow == 'annual_nome') { 
  train.full <- train.full %>% 
    rename(yyyy = date_local) %>%
    dplyr::select(lat, lon, yyyy, obs,
                  pred_av, pred_cc, pred_cm, pred_gs,  pred_js, pred_rk, 
                  state, ref_id, fold) 
  timeWindow <- 'annual'
}


train.full %>% 
  write_csv(here::here('inputs', 'pm25', 'training_datasets', 
                       paste0(timeWindow, '_combined'), saveFileName))

