# Task: Harmonize daily predictions and Potential Explanatory Variables
# File: d_07_assignexpVar_annualBNE.R
# SubProject: Analysis of BNE PM2.5 Predictive Uncertainty
# Project: Bayesian Nonparametric Ensemble 
# Author: Sebastian T. Rowland <sr3463@cumc.columbia.edu>

#### ------------------- ####
####  Table of Contents  ####
#### ------------------- ####

#  N: notes
#  0: preparation 
#  1. readin in constant datasets
#  2. main loop
#  2A. bring in bne ppd
#  2B. assign nearest AQS monitor 
#  2C. assign population density 
#  2D. assign elevation 
#  2E. assign seasonal temperature
#  2F. assign wind speed and rh
#  2G. assign precipitation 
#  2H. assign albedo 
#  2I. assign cloud cover 
#  2J. assign boundary layer height
#  2K. assign state and region
#  2L. save dataset

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

#### --------------------------------- ####
####   1. readin in constant datasets  ####
#### --------------------------------- ####

# 1.d. bring in popD layer 
popD10 <- sf::st_read(here::here(dir.proj, 'data', 'explanatory_variables', 'intermediate',
                               'census_decSurvey_2010', 'popDen_2010.shp'))
popD15 <- sf::st_read(here::here(dir.proj, 'data', 'explanatory_variables', 'intermediate',
                                 'census_acs5_2015', 'popDen_2015.shp'))

# 1.e. bring in topography layer
topo <- read_csv(here::here(dir.proj, 'data', 'explanatory_variables', 'intermediate', 
                     'topo_processed.csv'))

# 1.a. bring in the eparegion-state key that we previously generated
epaRegions <- read_csv(here::here('ancillary_data', 'generated', 'epa_regions.csv'))


# aqs 
aqs <- read_csv(here::here('inputs', 'pm25', 'training_datasets', 'annual_combined', 
                           'training_cvfolds.csv')) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326"))

# 1.f. bring in keys 
key.ref.popD10 <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                    'key_nn_refGridConus_popD2010.fst'))
key.ref.popD15 <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                    'key_nn_refGridConus_popD2015.fst'))
key.ref.topo <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                    'key_nn_refGridConus_topo.fst'))
key.ref.windRH <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                    'key_nn_refGridConus_rh_wind.fst'))
key.ref.boundaryH <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                          'key_nn_refGridConus_boundaryH.fst'))

# states 
# 1.a. set excluded areas 
excludedAreas <- c('Alaska', 'Hawaii', 'Puerto Rico', 
                   'Commonwealth of the Northern Mariana Islands', 'Guam', 
                   'American Samoa', 'United States Virgin Islands')

# 1.b. load the base map from our shapefile
states <- st_read(here::here('ancillary_data', 'raw', 'census', 'cb_2015_us_state_500k', 
                          'cb_2015_us_state_500k.shp')) %>% 
  janitor::clean_names() %>%
  rename(state = stusps) %>% 
  mutate(statefp = as.numeric(statefp))

# 1.c. remove regions that will most likely not be included in the 
# contiguous nationwide application 
states <- states[!states$name%in%excludedAreas,]

state.table <- states %>% as.data.frame() %>% dplyr::select(state, statefp)

# convert to raster 
states.ras <- states %>% 
  dplyr::select(statefp) %>% 
  st_transform(crs=sf::st_crs("epsg:4326"))%>% 
  filter(!st_is_empty(.)) %>%
  st_rasterize(.)

#### ----------------------------- ####
####  2. loop to assign variables  ####
#### ----------------------------- ####

# Begin loop 
for (yyyy in 2010:2015) {

  #### ---------------------- ####
  ####  2A. bring in bne ppd  ####
  #### ---------------------- ####
  
  # 2B.a readin the ppd 
  ppd <- read_csv(here::here(dir.proj, 'bne_ppd', ppdPath, 
                             paste0('refGrid_', yyyy, '_2_0-5_2_0-5_0-5_0-0498_0-1353.csv')))
  
  # 2B.b. create ref_id variable to help us keep track of locations
  ppd <- ppd %>% 
    dplyr::mutate(ref_id = row_number()) 
  
  # 2B.c. create sf version
  ppd.sf <- ppd %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326"))
  
  #### ---------------------- ####
  ####  2B. nearest monitor  ####
  #### --------------------- ####
 
  # 2B.a. compute distance to nearest aqs monitor
  yyyy0 <- yyyy
  aqs.yyyy <- aqs %>% filter(yyyy == yyyy0)
  ppd$mon_dist <- nngeo::st_nn(ppd.sf, aqs.yyyy, k=1, returnDist = TRUE)$dist %>% unlist()
  
  #### ------------------------ ####
  ####  2C. population density  ####
  #### ------------------------ ####
  
  # 2C.a. get the correct popD
  if( yyyy != 2015) {
    popD <- popD10
  } else {popD <- popD15}
  
  # 2C.b. convert popd to a raster (specifically a STARS object)
  popD.ras  <- popD %>% 
    st_transform(crs=sf::st_crs("epsg:4326")) %>% 
    filter(!st_is_empty(.)) %>%
    st_rasterize(.)

  # 2C.c extract values at grid centroids
  popD.extracted <- st_extract(popD.ras, ppd.sf)
  ppd$pop_d <- popD.extracted$popDen_m2
  
  #### ---------------- ####
  ####  2D. topography  ####
  #### ---------------- ####
  
  # 2C.a. convert to a raster
  topo.ras  <- topo %>% 
    dplyr::select(-cell_id) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326")) %>%
    filter(!st_is_empty(.)) %>%
    st_rasterize(.)
  
  # 2C.c extract values at grid centroids
  topo.extracted <- st_extract(topo.ras, ppd.sf)
  ppd$elev <- topo.extracted$elev
  
  #### --------------------------------- ####
  ####  2E. assign seasonal temperature  ####
  #### --------------------------------- ####

  # 2E.aa read PRISM data 
  prism.winter <- raster(here::here(dir.proj, 'data', 'explanatory_variables', 
                                    'intermediate', 'PRISM_Seasonal',  
                                    paste0('winter_meanT_', yyyy)))
  prism.summer <- raster(here::here(dir.proj, 'data', 'explanatory_variables',
                                    'intermediate', 'PRISM_Seasonal', 
                                    paste0('summer_meanT_', yyyy)))
  
  # 2E.b. Convert projection 
  prism.winter <- projectRaster(prism.winter, crs = crs(projCRS.ras))
  prism.summer <- projectRaster(prism.summer, crs = crs(projCRS.ras))
  
  # 2E.c extract values
  ppd$temp_winter <- raster::extract(prism.winter, ppd.sf, 
                                  df = TRUE, factors = TRUE, fun = mean)$layer
  ppd$temp_summer <- raster::extract(prism.summer, ppd.sf, 
                                     df = TRUE, factors = TRUE, fun = mean)$layer
  
  #### ------------------------------ ####
  ####  2F. assign wind speed and rh  ####
  #### ------------------------------ ####
  
  # 2F.a. bring in eraLand data
  eraLand <- read_fst(here::here(dir.proj, 'data', 'explanatory_variables', 'intermediate', 
                              'era5_annual', paste0('annual_land_', yyyy, '.fst')))
  
  # 2F.b. convert to raster
  eraLand <-  eraLand %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326")) %>% 
    st_rasterize(.)
    
  # 2F.c extract values at grid centroids
  eraLand.extracted <- st_extract(eraLand, ppd.sf)
  ppd$wind_speed <- eraLand.extracted$wind_speed
  ppd$rh <- eraLand.extracted$rh
  
  #### -------------------------- ####
  ####  2G. assign precipitation  ####
  #### -------------------------- ####
  
  # 2G.a. bring in data 
  precip <- raster(here::here(dir.proj, 'data', 'explanatory_variables', 
                                    'raw', 'PRISM',  
                                    paste0('PRISM_ppt_stable_4kmM3_', yyyy, '_bil'), 
                              paste0('PRISM_ppt_stable_4kmM3_',yyyy,'_bil.bil'))) %>% 
    projectRaster(., crs = crs(projCRS.ras))
  
  # 2G.b. assign
  precip.extract <- raster::extract(precip, ppd.sf, 
                                    df = TRUE, factors = TRUE, fun = mean) 
  names(precip.extract) <- c('id', 'precip')
 ppd$precip<- precip.extract$precip

 #### ------------------- ####
 ####  2H. assign albedo  ####
 #### ------------------- #### 
 
 # 2H.a. bring in data 
 albedo <- raster(here::here(dir.proj, 'data', 'explanatory_variables', 
                             'raw', 'GIOVANNI',  
                             paste0('GIOVANNI-g4.timeAvgMap.NLDAS_NOAH0125_M_2_0_Albedo.', 
                                    yyyy, '0101-', yyyy, '1231.124W_25N_67W_49N.tif'))) %>% 
   projectRaster(., crs = crs(projCRS.ras))
 
 # 2H.b. assign
 albedo.extract <- raster::extract(albedo, ppd.sf, 
                 df = TRUE, factors = TRUE, fun = mean)
 names(albedo.extract) <- c('id', 'albedo')
 
 ppd$albedo <- albedo.extract$albedo
 
 #### ------------------------ ####
 ####  2I. assign cloud cover  ####
 #### ------------------------ #### 
 
 # 2I.a. Read cloud cover data
 cloud <- raster(here::here(dir.proj, 'data', 'explanatory_variables',
                            'raw', 'GIOVANNI',  
                            paste0('GIOVANNI-timeAvgMap_MYD08_D3_6_1_Cloud_Fraction_Mean_', yyyy, 
                                   '0101-', yyyy, 
                                   '1231_130W_24N_65W_52N.tif'))) %>% 
   projectRaster(., crs = crs(projCRS.ras))
 
 # 6c Compute value for cloud cover
 
 
 cloud_cover.extract <- raster::extract(cloud, ppd.sf, 
                                   df = TRUE, factors = TRUE, fun = mean)
 names(cloud_cover.extract) <- c('id', 'cloud_cover')
 
 ppd$cloud_cover <- cloud_cover.extract$cloud_cover

 #### ---------------------------------- ####
 ####  2J. assign boundary layer height  ####
 #### ---------------------------------- #### 
 
  #  2J.a. bring in data 
 boundaryH <- read_fst(here::here(dir.proj, 'data', 'explanatory_variables', 'intermediate', 
                                  'era5_annual', paste0('annual_boundaryH_', yyyy, '.fst')))
 
 # 2J.b. convert to raster
 boundaryH <-  boundaryH %>% 
   sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326")) %>% 
   st_rasterize(.)
 
 # 2J.c extract values at grid centroids
 ppd$boundary_h <- st_extract(boundaryH, ppd.sf)$boundary_layer_height
  
  a <- ppd %>% 
    dplyr::select(boundary_h, lat, lon) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326"))
  
  plot(a)
  
  #### ------------------------------ ####
  ####  2K. assign state and region  ####
  #### ----------------------------- ####
  
  # 2K.a. extract values at grid centroids
  states.extracted <- st_extract(states.ras, ppd.sf)
  ppd$statefp <- states.extracted$statefp
  
  ppd <- ppd %>% 
    inner_join(state.table, by = 'statefp') %>%
    inner_join(read_csv(here::here('ancillary_data', 'generated', 'key_state_region.csv')), by = 'state') 
  
  #### ------------------- ####
  ####   2L: save dataset  ####
  #### ------------------- ####
  
  ppd %>% 
    write_fst(here::here(dir.proj, 'data', 'ppd_assigned', ppdPath, 
                         paste0('bnePPD_expVar_', yyyy, '.fst')))
}

