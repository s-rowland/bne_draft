# Task: Fit uncertiany explanatory variables model
# File: c_01_collect_census_data.R
# SubProject: Analysis of BNE PM2.5 Predictive Uncertainty
# Project: Bayesian Nonparametric Ensemble 
# Author: Sebastian T. Rowland <sr3463@cumc.columbia.edu>

#### ------------------- ####
####  Table of Contents  ####
#### ------------------- ####

# N: Notes
# 0: Preparation 
# 1: 

#### ---------- ####
####  N: NOTES  ####
#### ---------- ####

# This script takes about X amount of time


#### ---------------- ####
####  0: Preparation  ####
#### ---------------- ####

# 0.a. import relevant packages, etc, based on whole project
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up',
                    'a_00_import_packages_set_global_objects.R'))
}

# 0.b. import packages and set objects specific to this subproject
if(!exists('ran_a_00_uncert')){
  here::i_am('README.md')
  source(here::here('str_uncert_analysis', 'code', 
                    '0_00_config_env_uncert_analysis.R'))
}

#### ------------------ ####
####   1. Collect Data  ####
#### ------------------ ####
# function
readPredsNorthEast  <- function(julianDay) {
  # bring in data
  dta <- read_fst(here::here(dir.proj, 'data', 'combined', 'daily', 
                             paste0('pred_vars_2010_',str_pad(julianDay, 3, 'left', '0'), 
                                    '_avcmjsme.fst'))) %>% 
    filter(region %in% c('Region01', 'Region02', 'Region03')) %>% 
    mutate(day = julianDay)
  # compute base model SD
  preds <- dta %>% 
    dplyr::select(starts_with('pred_'))
  dta$base_mean <- apply(preds, 1, mean)
  dta$base_sd <- apply(preds, 1, sd)
  # return the restricted dataframe
  return(dta)
}

# readin all the data
dta <- readPredsNorthEast(1)
dta <- map_dfr(1:31, readPredsNorthEast)

#### ------------------ ####
####   2. Compare deviance explained  ####
#### ------------------ ####


# create the models 

#### ------------------ ####
####   2A Fit Models   ####
#### ------------------ ####

tictoc::tic('end gam')
mod.full <- gam(base_sd ~ s(base_mean) +  s(aqs_dist) +
                  s(popDen_m2) + s(elev) + 
                  s(tMean) + s(windSpeed) + 
                  s(precip) + s(albedo) +
                  s(rh) + s(cloud) +
                  ns(day_index*region, df = 4), 
               data = dta)
tictoc::toc()
# base mean
mod.base_mean <- gam(base_sd ~   s(aqs_dist) +
                  s(popDen_m2) + s(elev) + 
                  s(tMean) + s(windSpeed) + 
                  s(precip) + s(albedo) +
                  s(rh) + s(cloud) +
                    ns(day_index*region, df = 4), 
                data = dta, sp = mod.full$sp[2:10])
# aqs dist 
mod.aqs_dist <- gam(base_sd ~ s(base_mean) + 
                  s(popDen_m2) + s(elev) + 
                  s(tMean) + s(windSpeed) + 
                  s(precip) + s(albedo) +
                  s(rh) + s(cloud) +
                    ns(day_index*region, df = 4), 
                data = dta, sp = c(mod.full$sp[1], mod.full$sp[3:10]))
# pop density 
mod.popDen_m2 <- gam(base_sd ~ s(base_mean) +  s(aqs_dist) +
                  s(elev) + 
                  s(tMean) + s(windSpeed) + 
                  s(precip) + s(albedo) +
                  s(rh) + s(cloud) +
                    ns(day_index*region, df = 4), 
                data = dta, sp = c(mod.full$sp[1:2], mod.full$sp[4:10]))
# elevation 
mod.elev <- gam(base_sd ~ s(base_mean) +  s(aqs_dist) +
                  s(popDen_m2) +  
                  s(tMean) + s(windSpeed) + 
                  s(precip) + s(albedo) +
                  s(rh) + s(cloud) +
                  ns(day_index*region, df = 4), 
                data = dta, sp = c(mod.full$sp[1:3], mod.full$sp[5:10]))
# mean 
mod.tMean <- gam(base_sd ~ s(base_mean) +  s(aqs_dist) +
                  s(popDen_m2) + s(elev) + 
                   s(windSpeed) + 
                  s(precip) + s(albedo) +
                  s(rh) + s(cloud) +
                   ns(day_index*region, df = 4), 
                data = dta, sp = c(mod.full$sp[1:4], mod.full$sp[6:10]))
# windspeed 
mod.windSpeed <- gam(base_sd ~ s(base_mean) +  s(aqs_dist) +
                  s(popDen_m2) + s(elev) + 
                  s(tMean) +  
                  s(precip) + s(albedo) +
                  s(rh) + s(cloud) +
                    ns(day_index*region, df = 4), 
                data = dta, sp = c(mod.full$sp[1:5], mod.full$sp[7:10]))
# precip 
mod.precip <- gam(base_sd ~ s(base_mean) +  s(aqs_dist) +
                  s(popDen_m2) + s(elev) + 
                  s(tMean) + s(windSpeed) + 
                   s(albedo) +
                  s(rh) + s(cloud) +
                    ns(day_index*region, df = 4), 
                data = dta, sp = c(mod.full$sp[1:6], mod.full$sp[8:10]))
# albedo 
mod.albedo <- gam(base_sd ~ s(base_mean) +  s(aqs_dist) +
                  s(popDen_m2) + s(elev) + 
                  s(tMean) + s(windSpeed) + 
                  s(precip) + 
                  s(rh) + s(cloud) +
                    ns(day_index*region, df = 4), 
                data = dta, sp = c(mod.full$sp[1:7], mod.full$sp[9:10]))
# RH 
mod.rh <- gam(base_sd ~ s(base_mean) +  s(aqs_dist) +
                  s(popDen_m2) + s(elev) + 
                  s(tMean) + s(windSpeed) + 
                  s(precip) + s(albedo) +
                s(cloud) +
                ns(day_index*region, df = 4), 
                data = dta, sp = c(mod.full$sp[1:8], mod.full$sp[10]))
# cloud
mod.cloud <- gam(base_sd ~ s(base_mean) +  s(aqs_dist) +
                  s(popDen_m2) + s(elev) + 
                  s(tMean) + s(windSpeed) + 
                  s(precip) + s(albedo) +
                  s(rh) + 
                  ns(day_index*region, df = 4), 
                data = dta, sp = mod.full$sp[1:9])

#### ------------------ ####
####   3 Compare Deviance Expained  ####
#### ------------------ ####

# function
calculatePropDevianceExplained <- function(modFull, activeModel){
  (deviance(activeModel)-deviance(modFull))/deviance(modFull)
}


# make table
propDev <- data.frame(
  var = c('base_mean', 'aqs_dist', 'popDen_m2', 'elev',
          'tMean', 'windSpeed', 'precip', 'albedo', 'rh', 'cloud'), 
  prop_dev = c(
    calcPropDevExpl(mod.full, mod.base_mean),
    calcPropDevExpl(mod.full, mod.aqs_dist),
    calcPropDevExpl(mod.full, mod.popDen_m2),
    calcPropDevExpl(mod.full, mod.elev),
    calcPropDevExpl(mod.full, mod.tMean),
    calcPropDevExpl(mod.full, mod.windSpeed),
    calcPropDevExpl(mod.full, mod.precip),
    calcPropDevExpl(mod.full, mod.albedo),
    calcPropDevExpl(mod.full, mod.rh),
    calcPropDevExpl(mod.full, mod.cloud))) %>% 
  mutate(prop_dev = round(prop_dev, 3))
