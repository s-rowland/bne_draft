# File: j_07_eTable1.R
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
    mutate(time = yyyy, cell_id = row_number()) 
}

# 1.b. bring in all the years of assigned ppd
bne.ppd <- map_dfr(2010:2015, readAssignedPPD) %>% 
  mutate(y_sd_scaled = y_sd / y_mean)


#### ----------------------- ####
####  2. overall correlation ####
#### ----------------------- ####

# 2.a calculate correlations 
bne.vars <- bne.ppd %>% 
  dplyr::select(y_mean, pred_av, pred_cc, pred_cm, pred_gs, pred_js, pred_me, pred_rk,
                y_sd, y_sd_scaled
                ) %>% 
  filter(complete.cases(.)) 
bne.corr <- cor(bne.vars)

#### -------------- ####
####  5. save plot  ####
#### -------------- ####

png(here::here(dir.proj, outPath, 'manuscript', 'exploring_corr.png'))
corrplot::corrplot(bne.corr, addCoef.col = 'black', number.digits = 1)
dev.off()

