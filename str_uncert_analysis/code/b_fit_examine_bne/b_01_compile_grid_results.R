# File: STR_d_02_make_training_predictions_JS.R
# Authors:
# Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 03/17/23
#
# Contents:
#  N. notes
#  0. preparation
#  1. compile grid search results

#### ------------------ ####
####       N. notes     ####
#### ------------------ ####

#### ---------------- ####
####  0. preparation  ####
#### ---------------- ####

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

#### -------------------------------- ####
####  1. compile grid search results  ####
#### -------------------------------- ####

# 1.a. idetnfiy files 
grid.files <- list.files(here::here(dir.proj, outPath, 
                                    'b_description_bne_grid_search'))

# 1.b. read them all in 
results <-  foreach(
  fileName = grid.files,
  .combine = 'rbind'
  
) %do% {
  read_csv(here::here(dir.proj, outPath, 'b_description_bne_grid_search', 
                       fileName)) %>% 
    mutate(fileName = fileName) %>% 
    filter(rmse != 0)
}

results <- results %>% 
  dplyr::select(-opt_stage, -seed, -fileName) %>%
  distinct()

results %>% 
  write_csv(here::here(dir.proj, outPath, 'b_description_bne_grid_search', 
                       'grid_search.csv'))
# 1.c identify top combos by each metric
rmse.top10 <- sort(results$rmse)[1:9]
r2.top10 <- sort(results$r2, decreasing = TRUE)[1:9]
cover.top10 <- sort(results$cover, decreasing = TRUE)[1:9]

results.topbyany <- results %>% 
  filter(rmse%in% rmse.top10 | 
           r2 %in% r2.top10 |
           cover %in% cover.top10) 

rmse.range = sd(results$rmse) 
r2.range = sd(results$r2) 
cover.range = sd(results$cover) 

rmse.min <- min(results$rmse)
r2.max <- max(results$r2)
cover.max <- max(results$cover)

