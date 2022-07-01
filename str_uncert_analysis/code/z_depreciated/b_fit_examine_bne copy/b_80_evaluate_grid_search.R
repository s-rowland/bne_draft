# Evaluate Grid Search
# Fit Annual-CONUS BNE
# BNE Uncertainty Analysis 
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 
# Last updated Oct 24, 2021

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Calculate CV Metrics for Grid Search Models

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_set_up_env.R"))
}

if(!exists("Ran_a_00_uncert")){
  here::i_am("README.md")
  source(here::here('str_uncert_analysis', 'code', 
                    "0_00_set_up_env_uncert_analysis.R"))
}

####****************************************************
#### 1: Calculate CV Metrics for Grid Search Models ####
####****************************************************

# 1a Create table of potential kernel lengths
#param.table <- expand.grid(len_scale_sp = seq(3, 4, 0.5), 
#                           len_scale_t = seq(0.007, 0.009, 0.001))

#param.table <- expand.grid(len_scale_sp = 3.5, 
#                           len_scale_t = c(1, 0.1, 0.01, 0.001, '0.0001', '1e-05', '1e-06'))

param.table <- expand.grid(len_scale_sp = c(10, 3.5, 0.5, 0.1), 
                           len_scale_t = c(50, 10, 1, 0.1, 0.01, 0.001, '0.0001', '1e-05'))

# 1b create object of the input model names 
inputSet <- list(c('av', 'gs', 'cm', 'js', 'cc'))

# 1c calculate the CV metrics
CV.metrics <- pmap(list(
  rep(inputSet, nrow(param.table)), 
  param.table$len_scale_sp, 
  param.table$len_scale_t), 
  calculateCVMetricsSpt) %>% 
  bind_rows()

# 1d save 
CV.metrics %>% 
  mutate(covErr = abs(0.95 - coverage)) %>%
  arrange(covErr) %>%
  arrange(desc(Rsq)) %>%
  readr::write_csv(here::here(dir.proj, 'outputs', 'b_fit_bne', 
                              'cvmetrics_annual_BNE_spT_wideTimewideSpace.csv'))

calculateCVMetricsSpt(c('av', 'gs', 'cm', 'js', 'cc'), 0.5, 'spatialOnly')
