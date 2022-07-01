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

# 1.a. function to read in the data
readAssignedPPD <- function(yyyy) {
  bne.ppd <- fst::read_fst(here::here(dir.proj, 'data', 'ppd_assigned', ppdPath, 
                                      paste0('bnePPD_expVar_', yyyy, '.fst'))) %>% 
    mutate(time = yyyy, cell_id = row_number(), 
           y_sd_scaled = y_sd / y_mean) 
}

# 1.b. bring in all the years of assigned ppd
bne.ppd <- map_dfr(2010:2015, readAssignedPPD)

# 1.c bring in aqs training data 
aqs <- read_csv(here::here('inputs', 'pm25', 'training_datasets','annual_combined', 
                          'training_cvfolds.csv'))

#### ---------------- ####
####  2. create table ####
#### ---------------- ####

# 2.a get distribution for bne inputs and ppd 
table1 <- data.frame(varName = 'X', mean = 0, sd = 0, min = 0, q1=0, q2=0, q3 =0, max = 0)

# 2.b function to fill table
fill_table1 <- function(table1, varName, varVect) {
  table1.nrow <- nrow(table1)
  table1[table1.nrow+1, ] <- c(varName, 
                               round(mean(varVect), 2), 
                               round(sd(varVect), 2), 
                               round(min(varVect), 2), 
                               round(quantile(varVect, 0.25)[[1]], 2), 
                               round(median(varVect), 2), 
                               round(quantile(varVect, 0.75)[[1]], 2),
                               round(max(varVect), 2)
                               )
  return(table1)
}

# 2.c fill table one with values
table1 <- table1 %>% 
  fill_table1('aqs', aqs$obs) %>% 
  fill_table1('Global LUR', bne.ppd$pred_av) %>% 
  fill_table1('NA LUR', bne.ppd$pred_cc) %>% 
  fill_table1('CMAQ-AQS Fusion', bne.ppd$pred_cm) %>% 
  fill_table1('Bayesian Hierarchial', bne.ppd$pred_gs) %>% 
  fill_table1('ML Ensemble', bne.ppd$pred_js) %>% 
  fill_table1('MERRA', bne.ppd$pred_me) %>% 
  fill_table1('CMAQ-AOD Fusion', bne.ppd$pred_rk) %>% 
  fill_table1('BNE Predicted Concentration', bne.ppd$y_mean) %>% 
  fill_table1('BNE Predictive Uncertainty', bne.ppd$y_sd)  %>% 
  fill_table1('BNE Scaled Uncertainty', bne.ppd$y_sd_scaled) 

# 2.d save 
table1 %>% 
  filter(varName != 'X') %>%
  write_csv(here::here(dir.proj, 'manuscript', 'table2_dist_main_vars.csv'))
  
# 2.e clean envrionment 
rm(table1, fill_table1, aqs, bne.ppd)
