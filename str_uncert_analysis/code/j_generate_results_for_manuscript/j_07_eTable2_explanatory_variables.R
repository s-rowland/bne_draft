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
  bne.ppd <- fst::read_fst(here::here(dir.proj, 'data', 'ppd_assigned', 'annual', 
                                      paste0('bnePPD_expVar_', yyyy, '.fst'))) %>% 
    mutate(time = yyyy, cell_id = row_number())
}

# 1.b. bring in all the years of assigned ppd
bne.ppd <- map_dfr(2010:2015, readAssignedPPD)

# 1.c. tabluate means and sd


expVar.summary <- bne.ppd %>% 
  summarize(
    pred_mean = mean(y_mean, na.rm =TRUE), 
    pred_sd = sd(y_mean, na.rm =TRUE),
    mon_dist_mean = mean(mon_dist, na.rm =TRUE), 
    mon_dist_sd = sd(mon_dist, na.rm =TRUE),
    pop_d_mean = mean(pop_d, na.rm =TRUE), 
    pop_d_sd = sd(pop_d, na.rm =TRUE),
    elev_mean = mean(elev, na.rm =TRUE), 
    elev_sd = sd(elev, na.rm =TRUE),
    temp_winter_mean = mean(temp_winter, na.rm =TRUE), 
    temp_winter_sd = sd(temp_winter, na.rm =TRUE),
    temp_summer_mean = mean(temp_summer, na.rm =TRUE), 
    temp_summer_sd = sd(temp_summer, na.rm =TRUE),
    wind_speed_mean = mean(wind_speed, na.rm =TRUE), 
    wind_speed_sd = sd(wind_speed, na.rm =TRUE),
    precip_mean = mean(precip, na.rm =TRUE), 
    precip_sd = sd(precip, na.rm =TRUE),
    albedo_mean = mean(albedo, na.rm =TRUE), 
    albedo_sd = sd(albedo, na.rm =TRUE),
    cloud_cover_mean = mean(cloud_cover, na.rm =TRUE), 
    cloud_cover_sd = sd(cloud_cover, na.rm =TRUE),
    boundary_h_mean = mean(boundary_h, na.rm =TRUE), 
    boundary_h_sd = sd(boundary_h, na.rm =TRUE)) 
