# Join Input Models 
# Prepare BNE Inputs
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Calculate CV Metrics for a Run

####**************
#### N: Notes ####
####**************

# Right now we are using training and avgscm prediction datasets inherited 
# from the Capstone students' project, so the code it not yet 100% reproducible

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_set_up_env.R"))
}

####***************************************
#### 1: Calculate CV Metrics for a Run ####
####***************************************

# 1a. calculate metrics for whole training dataset
CV.metrics <- calculateCVMetricsSpt(c('av', 'gs', 'cm', 'js', 'cc'),  3.5, 0.008, 'all', 'all')

# 1c. calculate metrics for specific EPA regions. 
for (i in 1:10){
  CV.metrics[(i+1),] <- calculateCVMetricsSpt(c('av', 'gs', 'cm', 'js', 'cc'), 0.008, 3.5, i, 'all')
}

# 1d. save 
CV.metrics %>% 
  readr::write_csv(here::here('str_app_conus_uncert', 'outputs', 'CV', 
                              'cvmetrics_annual_BNE_spT.csv'))
