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


# 1b create object of the input model names 
inputSet <- list(c('av', 'gs', 'cm', 'js', 'cc'))

# 1c calculate the CV metrics
CV.metrics <- pmap(list(
  rep(inputSet, 11), 
  rep(2.5, 11), 
  rep('spatialOnly', 11),
  c('all', 1:10)),
  calculateCVMetricsSpt) %>% 
  bind_rows()

# 1d save 
CV.metrics %>% 
  mutate(covErr = abs(0.95 - coverage)) %>%
  #arrange(covErr) %>%
  #arrange(desc(Rsq)) %>%
  readr::write_csv(here::here(dir.proj, 'outputs', 'Fiore_mtg', 
                              'cvmetrics_annual_BNE_sp_regional.csv'))
