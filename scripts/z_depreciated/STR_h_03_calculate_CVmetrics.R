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

# 1a. create empty table of CV metrics
CV.metrics <- data.frame(Region = 'all', season = 'all', ME = 0,  MAE = 0, 
                         RMSE = 0, Rsq = 0, coverage = 0, corr = 0, slope = 0)

# 1b. calculate metrics for whole training dataset
CV.metrics[1,] <- calculateCVMetrics(c('av', 'gs', 'cm', 'js', 'cc'),  3.5, 'all', 'all')

# 1c. calculate metrics for specific regions. 
for (i in 1:10){
  CV.metrics[(i+1),] <- calculateCVMetrics(c('av', 'gs', 'cm', 'js', 'cc'),  3.5, i, 'all')
}

# 1d. save 
CV.metrics %>% 
  readr::write_csv(here::here('str_app_conus_uncert', 'outputs', 'CV', 
                              'cvmetrics_annual_BNE.csv'))
