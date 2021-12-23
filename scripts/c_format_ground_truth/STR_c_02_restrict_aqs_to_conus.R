# File: STR_c_02_restrict_aqs_to_conus.R
# Author: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 12/22/2021

# Contents:
# N. Notes
# 0. Package Imports


#### -------- ####
#### N. NOTES ####
#### -------- ####

# The goal of this script is to restrict the curated AQS monitoring observations to 
# CONUS

#### ------------------- ####
####  0. PACKAGE IMPORTS ####
#### ------------------- ####

if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_config_env.R"))
}

#### ---------------------------- ####
####  1. filter aqs data to conus ####
#### ---------------------------- ####

# 1a. bring in conus shapefile
conus <- sf::st_read(here::here('data_ancillary', 'formatted', 'spatial_outlines', 
                                'conus.shp')) %>% 
  sf::st_transform(., crs=st_crs(projString))

# 1b. make vector of time scales 
timeScales <- c('annual', 'daily')

# loop 
for (timeScale in timeScales){
  
  # 1c. read in EPA AQS data that LGC has prepared:
  aqsPath <- here::here('BNE_inputs', 'ground_truth', 'formatted', 
                        paste0('lgc_', timeScale, '_data_2000-2016.csv'))
  aqs <- loadData(aqsPath, paste0("AQS_", timeScale))
  
  # 1b. make aqs spatial 
  aqs <- aqs %>% 
    sf::st_as_sf(., coords = c("lon", "lat"), crs=sf::st_crs('epsg:4326')) %>% 
    sf::st_transform(., crs=st_crs(projString))
  
  # 1d. restrict aqs to be within the bounding box of conus
  aqs.conus <- aqs %>% 
    dplyr::mutate(lat = sf::st_coordinates(.)[,2], 
                  lon = sf::st_coordinates(.)[,1],) %>% 
    dplyr::filter(lat > sf::st_bbox(conus)$ymin[[1]] & 
                    lat < sf::st_bbox(conus)$ymax[[1]] &
                    lon > sf::st_bbox(conus)$xmin[[1]] & 
                    lon < sf::st_bbox(conus)$xmax[[1]] )
  
  # 1e. make not-spatial 
  aqs.conus <- aqs.conus %>% 
    sf::st_transform(., crs=sf::st_crs('epsg:4326')) %>%
    dplyr::mutate(lat = sf::st_coordinates(.)[,2], 
                  lon = sf::st_coordinates(.)[,1],) %>% 
    as.data.frame() %>% 
    dplyr::select(-geometry)
  
  # 1g. rename pm2.5 variable 
  aqs.conus <- aqs.conus %>% 
    dplyr::rename(obs = obs_pm2_5)
  
  # 1f. save results 
  aqs.conus %>% 
    readr::write_csv(here::here('BNE_inputs', 'ground_truth', 'formatted', 
                                paste0('lgc_', timeScale, '_data_2000-2016_conus.csv')))
  
}
