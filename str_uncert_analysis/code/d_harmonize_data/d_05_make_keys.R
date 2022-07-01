# Task: Harmonize daily predictions and Potential Explanatory Variables
# File: c_02_harmonize_daily.R
# SubProject: Analysis of BNE PM2.5 Predictive Uncertainty
# Project: Bayesian Nonparametric Ensemble 
# Author: Sebastian T. Rowland <sr3463@cumc.columbia.edu>

#### ------------------- ####
####  Table of Contents  ####
#### ------------------- ####

#  N: notes
#  0: preparation 
#  1. bring in refgrid
#  2. create key for ERA5 land data 
#  3. create key for ERA5 singleLayer data
#  4. create key for topography
#  5. create key for pop density
#  6. create key for mixing height
#  7. create key for shoreline

#### ---------- ####
####  N: notes  ####
#### ---------- ####

# This script takes about X amount of time


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

dir.proj <- 'str_uncert_analysis'

#### --------------------- ####
####  1. bring in refgrid  ####
#### --------------------- ####

# 1.a. this is the refgrid along which all of the predictions are generated
refGridConus <- fst::read_fst(here::here('inputs', 'pm25', 'reference_grids',  
                                         paste0('refGrid_', 'conus', '.fst')))


#### ---------------------------------- ####
####  2. create key for popD  ####
#### ---------------------------------- ####

# 2a bring in dataset
popD <- sf::st_read(here::here(dir.proj, 'data', 'explanatory_variables', 'intermediate',
                               'census_decSurvey_2010', 'popDen_2010.shp'))

# 2b get centroids
popD.centroid <- st_centroid(popD)
popD.centroid <- popD.centroid %>% 
  mutate(lat = st_coordinates(popD.centroid)[,2], 
         lon= st_coordinates(popD.centroid)[,1]) %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)%>% 
  filter(!is.na(lat))

# 5.d. create the key for pop density 2010
createKey(ref.df = refGrid, refName = 'refGridConus', 
          baseModel.df = popD.centroid, baseModelName = 'popD2010') 

# 5.e. read in pop density 2015
popD <- sf::st_read(here::here(dir.proj, 'data', 'explanatory_variables', 'intermediate',
                               'census_acs5_2015', 'popDen_2015.shp'))

popD.centroid <- st_centroid(popD)
popD.centroid <- popD.centroid %>% 
  mutate(lat = st_coordinates(popD.centroid)[,2], 
         lon= st_coordinates(popD.centroid)[,1]) %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry) %>% 
  filter(!is.na(lat))

# 5.f. create the key for popdensity 2015
createKey(ref.df = refGrid, refName = 'refGridConus', 
          baseModel.df = popD.centroid, baseModelName = 'popD2015') 


#### ---------------------------------- ####
####  3. create key for elevation ####
#### ---------------------------------- ####

# 4.a. read in Topo
topo <- read_csv(here::here(dir.proj, 'data', 'explanatory_variables', 'intermediate', 
                            'topo_processed.csv'))

# 4.b. create the key for topo
createKey(ref.df = refGrid, refName = 'refGridConus', 
          baseModel.df = topo, baseModelName = 'topo') 



#### ---------------------------------- ####
####  6. create key for wind speed and RH####
#### ---------------------------------- ####

# bring in data 
rh_wind <- read_fst(here::here(dir.proj, 'data', 'explanatory_variables', 'intermediate', 
                               'era5_annual', 'annual_land_2010.fst'))

createKey(ref.df = refGrid, refName = 'refGridConus', 
          baseModel.df = rh_wind, baseModelName = 'rh_wind') 

#### -------------------------------- ####
####  7. create key for mixing height ####
#### -------------------------------- ####

# 7a bring in data 
boundaryH <- read_fst(here::here(dir.proj, 'data', 'explanatory_variables', 'intermediate', 
                               'era5_annual', 'annual_boundaryH_2010.fst'))

createKey(ref.df = refGrid, refName = 'refGridConus', 
          baseModel.df = boundaryH, baseModelName = 'boundaryH') 

