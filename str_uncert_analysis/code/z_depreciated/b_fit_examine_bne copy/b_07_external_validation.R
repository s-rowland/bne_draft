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

# 1.a. bring in the external vlaidation results 
dta <- read_csv(here::here(dir.proj,'outputs', 'd_bne_results', 'external_validation', 
                           'External_validation_winner.csv'))

# remove nyccas for now 
dta <- dta %>% 
  filter(! (lon > -74.2 & lon < -73.8 & 
              lat > 40.5 & lat < 40.9))


# 1.b. compute global RMSE
dta <- dta %>% 
  mutate(se = (obs- y_mean)^2)
rmse.global <- sqrt(mean(dta$se))

# 1.c. compute global coverage 
dta <- dta %>% 
  mutate(cover_95 = if_else(obs>=y_95CIl & obs <= y_95CIu, 1, 0))

coverage.global <- 100* mean(dta$cover_95)


dta <- dta %>% 
  mutate(se_ens = (obs- ens_mean)^2)
rmse.global <- sqrt(mean(dta$se_ens))
