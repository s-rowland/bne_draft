# File: j_21_eTable4_top_grid_results.R
# BNE Annual Uncertainty Analysis
# Authors:
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 10/04/2022
#
# Contents:
#  N. notes
#  0. preparation
#  1. bring in data
#  2. create table

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

# 0.b. load objects and packages specific for this work
if(!exists('ran_a_00_uncert')){
  here::i_am('README.md')
  source(here::here('str_uncert_analysis', 'code', 
                    '0_00_config_env_uncert_analysis.R'))
}

# 0.c. loac objects for generating plots
if(!exists('ran_j_00')){
  here::i_am('README.md')
  source(here::here('str_uncert_analysis', 'code', 'j_generate_results_for_manuscript',
                    'j_00_set_plotting_features.R'))
}

#### ------------------ ####
####  1. bring in data  ####
#### ------------------ ####

# 1.a bring in all results
results <- read_csv(here::here(dir.proj, outPath, 
                       'grid_search.csv'))

# 1.b remove unecesary columns

# 1.c order by coverage 
results <- results %>% 
  arrange(desc(cover))

# 1.d round variables 
results <- results %>% 
  mutate(rmse = round(rmse, 2), 
         r2 = round(r2, 2), 
         cover = round(cover, 2))

# 1.e keep top 10 
results <- results %>% 
  slice(1:10)

# 1.f save
results %>% 
  write_csv(here::here(dir.proj, 'manuscript', 'eTable3_grid_search.csv'))
