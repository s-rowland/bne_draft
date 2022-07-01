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
  dplyr::select(y_mean, albedo, boundary_h, cloud_cover, 
                elev, mon_dist, pop_d, precip, temp_summer, temp_winter, wind_speed, 
                y_sd, y_sd_scaled
                ) %>% 
  filter(complete.cases(.)) %>% 
  rename(predicted_PM25 = y_mean, boundary_height = boundary_h, elevation= elev, 
         pred_uncert = y_sd, pred_uncert_scaled = y_sd_scaled, 
         pop_density = pop_d, precipitation =precip) 

bne.vars <- bne.vars %>% 
  dplyr::select(predicted_PM25, mon_dist, pop_density, elevation, 
                temp_winter, temp_summer, precipitation, albedo, cloud_cover, 
                boundary_height, wind_speed, pred_uncert, pred_uncert_scaled)

names(bne.vars) <- c('Predicted PM2.5', 
                     'Distance to Nearest Monitor', 
                     'Population Density', 
                     'Elevation', 
                     'Winter Temperature', 
                     'Summer Temperature', 
                     'Precipitation', 
                     'Albedo', 
                     'Cloud Cover',
                     'Mixing Layer Height', 
                     'Wind Speed', 
                     'Absolute Uncertainty', 
                     'Scaled Uncertainty')
bne.corr <- cor(bne.vars, method = 'spearman')

#### -------------- ####
####  5. save plot  ####
#### -------------- ####

png(here::here(dir.proj, 'manuscript', 'eFig8a_expVar_corr_all.png'))
corrplot::corrplot(bne.corr, addCoef.col = 'black', number.digits = 1)
dev.off()

