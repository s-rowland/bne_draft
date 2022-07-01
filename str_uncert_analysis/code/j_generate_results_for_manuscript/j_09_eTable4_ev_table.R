# File: j_07_eTable1.R
# BNE Annual Uncertainty Analysis
# Authors:
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 10/04/2022
#
# Contents:
#  N. notes
#  0. preparation
#  a. abstract
#  r1. BNE inputs
#  r2. grid search results 
#  r3. BNE general results
#  r4. uncertainty analysis

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

#### ------------- ####
####  ev stuff  ####
#### ------------- ####

# r2.g EV rmse 
# r2.g.i bring in the external validation results 
ppd.ev <- read_csv(here::here(dir.proj, 'bne_ppd', 'annual', 
                           'ev_2_0-5_2_0-5_0-5_0-0498_0-1353.csv')) %>% 
  mutate(pred_bne = y_mean)

# r2.g.ii compute global RMSE
ppd.ev <- ppd.ev %>% 
  mutate(err_av = obs - pred_av, 
         err_cc = obs - pred_cc,
         err_cm = obs - pred_cm,
         err_gs = obs - pred_gs,
         err_js = obs - pred_js,
         err_me = obs - pred_me,
         err_rk = obs - pred_rk,
         err_bne = obs - y_mean, 
         se_bne = (obs- y_mean)^2)


ppd.ev2 <- ppd.ev %>% 
  pivot_longer(starts_with('pred_')) %>% 
  dplyr::select(name, value, obs) %>% 
  mutate(model = str_sub(name, 6)) %>% 
  group_by(model) %>% 
  summarize(rmse = round(sqrt(mean((value-obs)^2)), 2), 
            me = round(mean(value-obs), 2), 
            me_sd = round(sd(value-obs),2),
            r2 = round(cor(obs, value)^2, 2))


  ppd.ev2$slope <- 0
  ppd.ev2$CI <- 'a'
  
for (i in 1:nrow(ppd.ev2)) {
  
  varName <- paste0('pred_', ppd.ev2$model[i])
  
  dta <- ppd.ev %>% 
   rename(pred_mod := !!varName)
  mod.lm <- lm(obs~pred_mod, data = dta)
  ppd.ev2$slope[i] <- round(summary(mod.lm)$coeff[2, 1], 2)
  ppd.ev2$CI[i] <- paste0(round(summary(mod.lm)$coeff[2, 1] - 1.96*summary(mod.lm)$coeff[2, 2], 2), 
                       ', ',
                       round(summary(mod.lm)$coeff[2, 1] + 1.96*summary(mod.lm)$coeff[2, 2], 2))
}


ppd.ev2 %>% 
  renameBaseM() %>% 
  write_csv(here::here(dir.proj,'manuscript', 'eTable4_external_validation.csv'))

