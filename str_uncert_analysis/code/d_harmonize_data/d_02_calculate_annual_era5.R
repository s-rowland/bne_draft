# File: b_06_conduct_external_validation.R
# Authors:
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 03/17/22
#
# Contents:
#  N. notes
#  0. preparation
#  1. download PRISM data
#  2. calculate seasonal averages

#### ------------------- ####
####  Table of Contents  ####
#### ------------------- ####

#  0: Preparation 
#  1. compute averages

#### ---------------- ####
####  0. preparation  ####
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

#### --------------------- ####
####  1. compute averages  ####
#### --------------------- ####

# 1.a create dataframe of year-dataset combos
comboDF <- expand_grid(yyyy = 2010:2015, 
                       eraSet = c('land', 'singleLayer', 'boundaryH'))

# 1.b loop over each combination
for (i in 1: nrow(comboDF)) {

  # 1.c compute average
  dgk <- foreach(
    mm = 1:12, 
    .combine = 'rbind'
  ) %do% {
    
    # 1.c.i read in dataset
    dta  <- fst::read_fst(here::here(dir.proj, 'data', 'explanatory_variables', 
                                'intermediate', 'era5', 
                                paste0(comboDF$yyyy[i], '_', pad0(mm), '_', 
                                       comboDF$eraSet[i], '.fst')))
    
    # 1.c.ii create placeholder dataframe with the variables we will average
    namedf <- data.frame(name = names(dta) ) %>% 
      mutate(var = str_remove_all(name, '[0-9]')) %>% 
      dplyr::select(var) %>% 
      distinct() %>% 
      filter(!var %in% c('lat', 'lon'))
    
    if(comboDF$eraSet[i] == 'land') {
      namedf <- data.frame(var = c('wind_speed_', 'rh_'))
    }
    
    # 1.c.iii determine percentage of year from month
    daysInMonth <- lubridate::days_in_month(as.Date(paste0(comboDF$yyyy[i], 
                                                           '-', mm, "-01"), "%Y-%m-%d"))
    if (comboDF$yyyy[i]== 2012) {
      daysInYear= 366
    } else {daysInYear = 365}
    percentOfYear <- daysInMonth/daysInYear
    
    # 1.c.iv calculate some variables 
    # some potential explanatory variables are not directly reported by era,
    # but must be calculated
    
    for (d in 1:daysInMonth) {
      
      windVact <- paste0('windV_', pad0(d))
      windUact <- paste0('windU_', pad0(d))
      tempMact <- paste0('tMean_', pad0(d))
      tempDewact <- paste0('tDew_', pad0(d))
      windSpeedAct <- paste0('wind_speed_', pad0(d))
      rhAct <- paste0('rh_', pad0(d))
      
      dta <- dta %>% 
        rename(windU := all_of(windUact), windV := all_of(windVact), 
               tMean2 := all_of(tempMact), tDew2 := all_of(tempDewact)) %>%
        mutate(tDew2 = tDew2- 273.15, tMean2 = tMean2 - 273.15) %>%
        mutate(!!rhAct := 100* (exp((17.625*tDew2)/(243.04+tDew2))/exp((17.625*T)/(243.04+T))), 
               !!windSpeedAct := sqrt(windU^2 + windV^2)) %>% 
        dplyr::select(-windU, -windV, -tMean2, -tDew2)
    }
    
  # 1.c.v compute the mean for each month, for each variable, and then 
    # rescale by percent of the year for that month.
    for (v in 1:nrow(namedf)) {
      
      varName <- str_sub(namedf$var[v], 0, -2)
      
      dtaVar <- dta %>% 
        dplyr::select(contains(namedf$var[v]))
      
      dta$var_mean <- apply(dtaVar, 1, mean)
      
      dta <- dta %>% 
        dplyr::select(-contains(varName)) %>% 
        mutate(var_mean = var_mean * percentOfYear) %>%
        rename(!!varName := var_mean) 
      
    }
    # 1.c.vi keep only variables of interest
    if(comboDF$eraSet[i] == 'land') {
      dta <- dta %>% 
        dplyr::select(lat, lon, rh, wind_speed)
    }
    
    dta
  }
  
  
  # 1.c.vii compute annual averages via weighted sum
  dgk <- dgk %>% 
    group_by(lat, lon) %>% 
    summarize_all(.funs = sum)
  
  # 1.c.viii save
  dgk %>% 
    fst::write_fst(here::here(dir.proj, 'data', 'explanatory_variables', 'intermediate', 
                              'era5_annual',
                              paste0('annual_',comboDF$eraSet[i], '_', comboDF$yyyy[i], '.fst')))
  print(i)
  gc()

}

