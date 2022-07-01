# File: j_01_table2_dist_main_vars.R
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

table3 <- read_csv(here::here(dir.proj, outPath, 'f_uncert_analysis', 
                              'prop_dev_explained_ySD_psp.csv')) %>% 
  dplyr::select(covarName, prop_dev) %>% 
  rename(prop_dev_SD = prop_dev) %>% 
  dplyr::select(covarName, prop_dev_SD) %>% 
  inner_join(read_csv(here::here(dir.proj, outPath, 'f_uncert_analysis', 
                                 'prop_dev_explained_ySDScaled_psp.csv')) , by = 'covarName') %>% 
  rename(prop_dev_SDScaled = prop_dev, 
         expVarName = covarName)  %>% 
  dplyr::select(expVarName, prop_dev_SD, prop_dev_SDScaled) 
  

# 2.d save 
table3 %>% 
  renameExpVar () %>% 
  mutate(prop_dev_SD = round(prop_dev_SD, 3), 
         prop_dev_SDScaled = round(prop_dev_SDScaled, 3)) %>%
  write_csv(here::here(dir.proj, 'manuscript', 'table3_deviance_explained.csv'))
  

