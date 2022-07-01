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

#### -------------------------------- ####
#### 1. bring in bne ppd's  ####
#### -------------------------------- ####

# delcare that you ran this script 
ran_j_00 <- 'ran_j_00'

# general plotting theme 
tema <- theme_classic() + 
  theme(axis.title = element_text(size = 15), 
              axis.text = element_text(size = 13), 
              legend.title = element_text(size = 15), 
              legend.text = element_text(size = 13), 
        title= element_text(size = 15))

# key of region names 
key.state.region <- read_csv(here::here('ancillary_data', 'generated', 
                                       'key_state_region.csv'))
key.regionNames <- read_csv(here::here('ancillary_data', 'generated', 
                                  'key_region_regionName.csv'))

# function 

orderRegionNames <- function(dta) {
  dta %>% 
    mutate(region_name = factor(region_name, levels =c(
      "1: New England",  "2: NY Area", "3: DelMarVa", "4: Southeast", 
      "5: Ohio Valley", "6: South Central", "7: Midwest", 
      "8: North Midwest", "9: Pacific Southwest", "10: Pacific Northwest")))
}

renameBaseM <- function(dta) {
  # if we have prediction columns for each base model
  if (str_detect(paste0(names(dta), collapse = ''), 'pred_av')) { 
    dta <- dta %>% 
      rename('AV2021' = pred_av, 
           'SK2020' = pred_cc,
           'VB2012' = pred_cm, 
           'GS2018' = pred_gs, 
           'QD2021' = pred_js, 
           'GM2022' = pred_me, 
           'RK2022' = pred_rk)
  }
  
  if (str_detect(paste0(names(dta), collapse = ''), 'model')) { 
    dta <- dta %>% 
      mutate(model = case_when(
        model == 'av' ~ 'AV2021', 
        model == 'cc' ~ 'SK2020', 
        model == 'cm' ~ 'VB2012', 
        model == 'gs' ~ 'GS2018', 
        model == 'js' ~ 'QD2021', 
        model == 'me' ~ 'GM2022', 
        model == 'rk' ~ 'RK2022', 
        model == 'bne' ~ 'BNE'
      )) %>% 
      mutate(model = factor(model, 
                            levels = c('BNE', 'AV2021', 'SK2020', 'VB2012',  'GS2018', 
                                       'QD2021', 'GM2022', 'RK2022'))) 
    
  }
  return(dta)

}


renameExpVar <- function(dta) {
  # if we have prediction columns for each base model
  if (str_detect(paste0(names(dta), collapse = ''), 'boundary_h')) { 
    dta <- dta %>% 
      rename('Global LUR' = pred_av, 
             'NA-LUR' = pred_cc,
             'CMAQ-AQS Fusion' = pred_cm, 
             'Bayesian Hierchical' = pred_gs, 
             'ML Ensemble' = pred_js, 
             'Global Reanalysis' = pred_me, 
             'CMAQ-AOD Fusion' = pred_rk)
  }
  
  if (str_detect(paste0(names(dta), collapse = ''), 'expVarName')) { 
    dta <- dta %>% 
      mutate(expVarName = case_when(
        expVarName == 'y_mean' ~ 'Predicted Concentration (ug/m^3)', 
        expVarName == 'mon_dist' ~ 'Distance to Nearest Monitor (m)', 
        expVarName == 'pop_d' ~ 'Root of Population Density (persons/1km^2)', 
        expVarName == 'elev' ~ 'Elevation (m)', 
        expVarName == 'temp_winter' ~ 'Winter Temperature (C)', 
        expVarName == 'temp_summer' ~ 'Summer Temperature (C)', 
        expVarName == 'precip' ~ 'Precipitation (mm)', 
        expVarName == 'albedo' ~ 'Albedo (%)', 
        expVarName == 'cloud_cover' ~ 'Cloud Cover (%)', 
        expVarName == 'boundary_h' ~ 'Mixing Layer Height (m)', 
        expVarName == 'wind_speed' ~ 'Wind Speed (m/s)'
      )) %>% 
      mutate(expVarName = factor(expVarName, levels = 
                                   c('Predicted Concentration (ug/m^3)', 
                                     'Distance to Nearest Monitor (m)', 
                                     'Root of Population Density (persons/1km^2)', 
                                     'Elevation (m)', 
                                     'Winter Temperature (C)', 
                                     'Summer Temperature (C)', 
                                     'Precipitation (mm)', 
                                     'Albedo (%)', 
                                     'Cloud Cover (%)',
                                     'Mixing Layer Height (m)', 
                                     'Wind Speed (m/s)' )
                                 ))
  }
  return(dta)
  
}


#### --------- ####
#### 2. colors ####
#### --------- ####

# delcare that you ran this script 
ran_j_00 <- 'ran_j_00'

# stuff
col.Region <- ggsci::pal_d3(palette = 'category10')(10)

#models
cols.models <- c(watlington(16)[1], 
                 watlington(16)[3:7], 
                 watlington(16)[9],
                 watlington(16)[12])
