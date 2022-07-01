# File: j_04_table2_propDevExp.R
# BNE Annual Uncertainty Analysis
# Authors:
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 10/04/2022
#
# Contents:
#  N. notes
#  0. preparation
#  1. reformat table

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

#### ------------------- ####
####  1. reformat table  ####
#### ------------------- ####

# 1.a bring in the table of deviance explained
table2 <- read_csv(here::here(dir.proj, 'outputs', 'f_uncert_analysis',
                              'prop_dev_explained.csv'))

# 1.b make numbers nice 
table2 <- table2 %>% 
  rename(explanatory_var = covarName) %>% 
  dplyr::select(explanatory_var, prop_dev) %>% 
  mutate(prop_dev = round(100*prop_dev, 2))
  
# 2.d save 
table2 %>% 
  write_csv(here::here(dir.proj,  'manuscript', 'table2_prop_deviance.csv'))
  
# 2.e clean envrionment 
rm(fill_table2)
