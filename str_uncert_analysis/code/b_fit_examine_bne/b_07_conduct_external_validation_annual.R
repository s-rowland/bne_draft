# File: b_06_conduct_external_validation.R
# Authors:
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 03/17/22
#
# Contents:
#  N. notes
#  0. preparation
#  1. compute ev metrics 

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

if(!exists('ran_a_00_uncert')){
  here::i_am('README.md')
  source(here::here('str_uncert_analysis', 'code', 
                    '0_00_config_env_uncert_analysis.R'))
}

#### ----------------------- ####
####  1. compute ev metrics  ####
#### ----------------------- ####

# 1.a. bring in the BNE predictions at the external validation sites
ppd <- read_csv(here::here(dir.proj, 'bne_ppd', ppdPath, 
                           'ev_2_0-5_2_0-5_0-5_0-0498_0-1353.csv'))

# 1.b. compute global RMSE
ppd <- ppd %>% 
  mutate(se = (obs- y_mean)^2)
rmse.global <- sqrt(mean(ppd$se))
r2.global <- cor(ppd$y_mean, ppd$obs)^2

# 1.c. compute global coverage 
ppd <- ppd %>% 
  mutate(se_ens = (obs- ens_mean)^2)
rmse.global.ens <- sqrt(mean(ppd$se_ens))
