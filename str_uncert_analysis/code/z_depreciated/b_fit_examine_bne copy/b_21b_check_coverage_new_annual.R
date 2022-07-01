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

#### -------------- ####
#### 1. gather ppds ####
#### -------------- ####

ppd.list <- list.files(here::here(dir.proj, 'BNE_ppd', 'annual', 'coverage_test'))

ppd <- foreach(i=1:length(ppd.list)) %do% {
  a <- read_csv(here::here(dir.proj, 'BNE_ppd', 'annual', 'coverage_test', ppd.list[i]))
  a <- a %>% 
    mutate(paramSet = ppd.list[i])
}

ppd <- ppd %>% bind_rows()


ppd <- ppd %>% 
  mutate(se = (y_mean - obs)^2, 
         se_js = (pred_js - obs)^2, 
         cover = if_else(obs > y_95CIl & obs < y_95CIu, 1, 0))

ppd.paraset <- ppd %>% 
  group_by(paramSet) %>% 
  summarize(rmse = sqrt(mean(se)), 
            rmse_js = sqrt(mean(se_js)), 
             coverage = 100 * mean(cover), 
            mean_y_sd = mean(y_sd),
            mean_pred_y = mean(y_mean),
            mean_w_av = mean(w_mean_av), 
            mean_w_cc = mean(w_mean_cc), 
            mean_w_cm = mean(w_mean_cm) , 
            mean_w_gs = mean(w_mean_gs), 
            mean_w_js = mean(w_mean_js), 
            mean_w_me = mean(w_mean_me), 
            mean_w_rk = mean(w_mean_rk) )



a <- ppd %>% filter(paramSet == 'EV_aqs_21220-0183_0-0183_julianDay_2.csv')
cor(a$ens_sd, a $y_sd)
cor(a$bias_sd, a $y_sd)
