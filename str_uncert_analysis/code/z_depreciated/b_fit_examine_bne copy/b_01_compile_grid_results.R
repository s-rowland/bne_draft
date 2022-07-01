# File: STR_d_02_make_training_predictions_JS.R
# Authors:
# Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 03/17/23
#
# Contents:
#  N. notes
#  0. Package Imports

#### ------------------ ####
####       N. notes     ####
#### ------------------ ####


#yyyy <- 2010


#### -------------- ####
#### 0. preparation ####
#### -------------- ####

# 0.a. load packages, etc
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

if(!exists('ran_a_00_uncert')){
  here::i_am('README.md')
  source(here::here('str_uncert_analysis', 'code', 
                    '0_00_config_env_uncert_analysis.R'))
}

#### --------------- ####
#### 1. compile grid search results ####
#### --------------- ####

# 1.a. idetnfiy files 
grid.files <- list.files(here::here(dir.proj, 'outputs', 
                                    'b_description_bne_grid_search', 'main_grid_search'))

# 1.b. read them all in 
results <-  foreach(
  fileName = grid.files,
  .combine = 'rbind'
  
) %do% {
  read_csv(here::here(dir.proj, 'outputs', 'b_description_bne_grid_search', 
                      'main_grid_search', fileName)) %>% 
  filter(!is.na(mse)) %>% 
    filter(mse != 0) %>% 
    mutate(fileName = fileName)
}

results <- results %>% 
  filter(str_detect(fileName, 'twoStage'))

# 1.c. identify winner 
winner <- arrange(results, mse)[1,]

# so current winner is 
# scale_Weights_space =2 
# scale_weights_time = 30 
# sclae_bias_space = 1
# scale_bias_time = 15 
# penalty = 0.498
# time_metric = 2 = day of the year

top10 <- arrange(results, mse)[1:10,]



