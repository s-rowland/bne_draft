# File: STR_d_01_make_training_prediction_datasets_daily.R
# Authors:
# Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 03/17/22
#
# Contents:
#  N. notes
#  0. preparation
#  1. create keys
#  2. make daily training data for each year # Robbie: needs to be renumerated
# Sebastian: renumbered here and below
#  3. combined yearly daily training data 
#  4. fill in missing JS
#  5. identify spatial folds

#### ------------------ ####
####       N. notes     ####
#### ------------------ ####


#### -------------- ####
#### 0. preparation ####
#### -------------- ####

# 0.a. load packages, etc
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}


#### ---------------- ####
####  1. create keys  ####
#### ---------------- ####

source(here::here('scripts', 'd_create_training_prediction_datasets', 
                  'STR_d_01a_make_keys_daily_base_models.R'))

#### ------------------------------------------- ####
####  2. make daily training data for each year  ####
#### ------------------------------------------- ####
# Robbie: Just thinking of these years, is it worth putting these as objects in the a_00 script (this applies to other years too if they vary)
# Sebastian: That's not a bad idea. I am not going to implement it for now because 
# it is not a priority, but I'll keep it in mind. 

for (activeYear in 2008:2016) {
  yyyy <- activeYear 
  source(here::here('scripts', 'd_create_training_prediction_datasets', 
                    'STR_d_01b_make_training_predictions_daily.R'))
}

#### ---------------------------------------- ####
####  3. combined yearly daily training data  ####
#### ---------------------------------------- ####

# 3.a identify the training files we made
list.files(here::here('inputs', 'pm25', 'training_datasets', 'daily_yearly'))

# 3.b read in and combine the training files
training.full <-  foreach(
  fileNames = list.files(here::here('inputs', 'pm25', 'training_datasets', 'daily_yearly')),
  .combine = 'rbind'
  
) %do% {
  dta <- read_csv(here::here('inputs', 'pm25', 'training_datasets', 'daily_yearly', 
                             fileNames))
  # we need to add an extra day for leap years
  if (str_sub("training_2005.csv", 10, 13) %in% c(2008, 2012, 2016)) { 
    
    
    # Robbie: explain because of leap year 
    # Sebastian: Added
    dta$max_doy <- 366
  } else {dta$max_doy <- 365}
  dta
  
}

# 3.c save
training.full %>% 
  rename(percent_of_year = day_of_year) %>%
  readr::write_csv(here::here('inputs', 'pm25', 'training_datasets', 'daily_combined', 
                              paste0('training_', 'daily_nofolds', '.csv')))


#### ----------------------- ####
####  4. fill in missing JS  ####
#### ----------------------- ####

# JS is missing at a few points, so we fill in via nearest-neighbor
# 4.a. fill in training data 
source(here::here('scripts', 'd_create_training_prediction_datasets', 
                  'STR_d_01c_fill_in_js_training_dataset_daily.R'))

# 4.b. fill in prediction data 
source(here::here('scripts', 'd_create_training_prediction_datasets', 
                  'STR_d_01d_fill_in_js_prediction_dataset_daily.R'))

#### --------------------------- ####
####  5. identify spatial folds  #### # Robbie: said before but perhaps we can quickly discuss what spatial folds are
#### --------------------------- #### 
# Sebastian: Sure! key idea is that we want the results of our cross-validation 
# to reflect how the model would do predicting concentrations in locations without 
# any monitors. However, most of monitors are near other monitors. PM2.5 
# is spatially smooth, so if the prediction model knows the concentration at one 
# monitor, then it can borrow that information to make a good guess at the nearby
# monitor that belones to the leave-out group. In that case, the prediction 
# model will do better than it actually would in, say, a rural area. 
# to prevent the prediction model from borrowing information across space 
# we define spatial leave-out folds, such that nearby monitors get left out at the same time. 

# 5.a. identify parameters
timeWindow <- 'daily'
trainingFileName <- 'training_daily_nofolds.csv'

train.full <- readr::read_csv(here::here('inputs', 'pm25', 'training_datasets', 'daily_combined', 
                                         paste0('training_', 'daily_nofolds', '.csv')))

# 5.b identify spatial folds
source(here::here('scripts', 'd_create_training_prediction_datasets', 
                  'STR_d_03_identify_spatial_folds_follower.R'))