# Task: Calculate Population Density
# File: c_03_calculate_pop_density.R
# SubProject: Analysis of BNE PM2.5 Predictive Uncertainty
# Project: Bayesian Nonparametric Ensemble 
# Author: Sebastian T. Rowland <sr3463@cumc.columbia.edu>

#### ------------------- ####
####  Table of Contents  ####
#### ------------------- ####

# N: Notes
# 0: Preparation 
# 1: Calculate Population Density

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

####*************************************
#### 1: Calculate Population Density ####
####*************************************

# 1a Readin Population data
pop <- read_csv(here::here('ancillary_data','raw', 'Census', 
                           'ACSST5Y2015.S0101_2021-03-21T142558', 
                           'ACSST5Y2015.S0101_data_with_overlays_2021-03-21T142433.csv')) 

# 1b Clean up population data 
pop <- pop %>% 
  slice(2:nrow(pop)) %>% 
  mutate(zcta = str_sub(GEO_ID, 10)) %>% 
  mutate(pop = as.numeric(S0101_C01_001E))

# 1b Readin Census ZCTA Spatial data 
zcta.sf <- st_read(here::here('ancillary_data', 'raw', 'Census', 'cb_2015_us_zcta510_500k', 
                           'cb_2015_us_zcta510_500k.shp'))

# 1f Reduce to non-spatial dataframe 
zcta.sf <- zcta.sf %>% 
  mutate(zcta = ZCTA5CE10, 
         area = ALAND10 + AWATER10)

zcta <- zcta.sf %>% 
  inner_join(pop, by = 'zcta')

# 1e Calculate population density 
zcta <- zcta %>% mutate(pop_density = pop / area)

# 1g Save 
# we rename pop_density because the column names for shapefiles have limited 
# number of characters
zcta %>% 
  dplyr::select(zcta, pop_density) %>%
  rename(popD = pop_density) %>% 
  st_write(here::here('uncertainty_factor_analysis_ISEE', 'data', 
                      'intermediate', 'pop_density_zcta.shp'))

# 1h Clean environment 
rm(zcta, zcta.sf, pop)