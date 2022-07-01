# File: c_01_collect_census_data.R
# Authors:
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 03/17/22
#
# Contents:
#  N. notes
#  0. preparation
#  1. download population density data

#### ---------------- ####
####  0: preparation  ####
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

#### ------------------------------------- ####
####  1: download population density data  ####
#### ------------------------------------- ####

# 1.a. establish census key
tidycensus::census_api_key("91f9f16db9ff6cae9d28020fd9c2f6dfb973d0db")

# 1.b. review datasets for variables 
v17 <- tidycensus::load_variables(2000, "sf1", cache = TRUE)
# for 2000 the survey is 'sf1' and variable name for total population is 'H010001'
v17 <- tidycensus::load_variables(2010, "sf1", cache = TRUE)
# for 2010 the survey is 'sf1' and variable name for total population is 'H010001'
v17 <- tidycensus::load_variables(2015, "acs5", cache = TRUE)
# for 2015 the survey is 'acs5' and variable name for total population is 'B00001_001'

# 1.c. function to get data for a specific state-year combination 
# the tidycensus functions will only return tract-level data for a given state, 
# not nationwide
getPopDensity <- function(stateAbb, yyyy) {
  
  # 1.c.i. download the data and properly name the variables
  if (yyyy %in% c(2000, 2010)) {
  dta <- tidycensus::get_decennial(state = stateAbb, 
                year = yyyy, 
                geography = "tract", 
                variables = 'P001001', 
                geometry = TRUE) %>% 
    rename(pop_count = value)
    
  } else if (yyyy %in% c(2015)) {
    dta <- tidycensus::get_acs(state = stateAbb,
                      year = yyyy,
                      geography = "tract", 
                      variables = 'B00001_001', 
                      geometry = TRUE) %>% 
      rename(pop_count = estimate)
  } else {print('Pick a different year or define in function.')}
  
  # 1.c.ii. add area and state name 
  dta$area <- st_area(dta)
  dta$state <- stateAbb
  
  # 1.c.iii. compute population density 
  dta <- dta %>% 
    mutate(popDen_m2 = as.numeric(pop_count / area))
  
  # 1.c.iv.  keep only variables of interest 
  dta <- dta %>% 
    rename(geoid = GEOID) %>%
    dplyr::select(geoid, popDen_m2, state, geometry)
  
  # 1.c.v. return result
  return(dta)
}

# 1.d. download the data
# 1.d.i download 2000 data 
purrr::map2_dfr(state.abb, 2000, getPopDensity) %>% 
  sf::st_write(here::here('str_uncert_analysis', 'data', 'explanatory_variables', 'intermediate',
                      'census_decSurvey_2000', 'popDen_2000.shp'))

# 1.d.ii download 2010 data 
purrr::map2_dfr(state.abb, 2010, getPopDensity) %>% 
  sf::st_write(here::here('str_uncert_analysis', 'data', 'explanatory_variables', 'intermediate',
                      'census_decSurvey_2010', 'popDen_2010.shp'))

# 1.d.iii download 2015 data 
purrr::map2_dfr(state.abb, 2015, getPopDensity) %>% 
  sf::st_write(here::here('str_uncert_analysis', 'data', 'explanatory_variables', 'intermediate',
                      'census_acs5_2015', 'popDen_2015.shp'))

# 1.d. clean environment 
rm(getPopDensity)
