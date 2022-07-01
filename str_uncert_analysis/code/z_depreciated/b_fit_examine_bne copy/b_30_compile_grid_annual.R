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
                                    'b_description_bne_grid_search', 'annual_grid_search'))




g1 <- read_csv(here::here(dir.proj, 'outputs', 'b_description_bne_grid_search', 
                    'annual_grid_search', grid.files[1])) %>% 
  filter(!is.na(mse)) %>% 
  filter(mse != 0) %>% 
  mutate(fileName = fileName)

g2 <- read_csv(here::here(dir.proj, 'outputs', 'b_description_bne_grid_search', 
                         'annual_grid_search', grid.files[2])) %>% 
  filter(!is.na(mse)) %>% 
  filter(mse != 0) %>% 
  mutate(fileName = fileName)

g3 <- read_csv(here::here(dir.proj, 'outputs', 'b_description_bne_grid_search', 
                         'annual_grid_search', grid.files[3])) %>% 
  filter(!is.na(mse)) %>% 
  filter(mse != 0) %>% 
  mutate(fileName = fileName)

results <- bind_rows(g1, g2, g3)

results <- results %>% 
  mutate(penalty_bias = if_else(is.na(penalty_bias), penalty, penalty_bias))



top10 <- arrange(results, mse)[1:10,]



