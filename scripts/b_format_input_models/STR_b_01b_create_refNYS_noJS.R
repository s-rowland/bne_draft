# File: STR_b_02_make_prediction_dataset_annual.R
# Author: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 9/15/2021
#
# Contents:
# N. Notes
# 0. Package Imports & Global Variables
# 1. Create Shapefile of Lower 48 States
# 2. Identify Grid Centroids within CONUS-Inscribed Rectangle
# 3. Restrict Centroids to CONUS

#----------------#
#### N. NOTES ####
#----------------#

# This script takes about 16 minutes to run. 

#---------------------------------------------#
#### 0. PACKAGE IMPORTS & GLOBAL VARIABLES ####
#---------------------------------------------#

# 0a Load packages and functions required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_set_up_env.R"))
}

#----------------------------------------------#
#### 1. CREATE SHAPEFILE OF LOWER 48 STATES ####
#----------------------------------------------#

# 1b. load the base map from our shapefile
states <- sf::st_read(here::here('data_ancillary', 'raw', 'Census', 'cb_2015_us_state_500k', 
                          'cb_2015_us_state_500k.shp'))

# 1c. remove regions that will most likely not be included in the 
# contiguous nationwide application 
stateNames <- c('NY')

states <- states[states$STUSPS%in%stateNames, ]

#-------------------------------------------------------------------#
#### 2. IDENTIFY GRID CENTROIDS WITHIN CONUS-INSCRIBED RECTANGLE ####
#-------------------------------------------------------------------#

dta <- data.frame(lat = NA, lon = NA)

# 2g. find the minimums and maximums
min.lat <- st_bbox(states)$ymin
max.lat <- st_bbox(states)$ymax
min.lon <- st_bbox(states)$xmin
max.lon <- st_bbox(states)$xmax

# 2h. make vectors of all the possible coordiante elements 
# this is the key step were we decide the number of elements, 
# the size of the grids, etc. 
seqLat <- seq(min.lat, max.lat, by = 0.025)
seqLon <- seq(min.lon, max.lon, by = 0.025)

# 2g. combine coordinate elements into grids
dta <- expand.grid(lat = seqLat, lon = seqLon) %>% 
          bind_rows(dta)

# 2h. clean up





#--------------------------------------#
#### 3. RESTRICT CENTROIDS TO CONUS ####
#--------------------------------------#


# 3c. convert to spatial (simple feature)
dta.extr.sf <- dta %>% 
  filter(!is.na(lat)) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs=st_crs("epsg:4326"))

# 3d. transform geographical coordinates to Lambert Azimuth Equal Area Projection
dta.extr.sf <- dta.extr.sf %>% 
  sf::st_transform(crs=st_crs(projString))

# 3e. intersect with CONUS, keep only overlapping points
# an alternative to to just intersect the points that are in the top or botto 
# 20th percentile for lat or for long 

states <- states %>% 
  sf::st_transform(crs=st_crs(projString))

dta.extr.sf.a <- dta.extr.sf %>% 
  sf::st_join(states, join = st_within) %>% 
  dplyr::filter(!is.na(STUSPS)) %>% 
  sf::st_transform(crs=st_crs("epsg:4326"))

refGrid <- dta.extr.sf.a 
plot(refGrid)

# 3f. combine 
refGrid <- dta.extr.sf.a %>%
  dplyr::mutate(lon = st_coordinates(.)[,1], 
         lat = st_coordinates(.)[,2]) %>%
  as.data.frame() %>% 
  dplyr::select(lat, lon) 

# 3g. save the centroids
refGrid %>% 
  dplyr::select(lat, lon) %>%
  fst::write_fst(here::here('data_ancillary', 'generated',  
                            'refNYS.fst'))

# confirm that it looks good. 
refGrid <-
  fst::read_fst(here::here('data_ancillary', 'generated',  
                            'refCities.fst'))
# 3d. transform geographical coordinates to Lambert Azimuth Equal Area Projection
refGrid <- refGrid %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs=st_crs("epsg:4326")) %>% 
  sf::st_transform(crs=st_crs(projString))

plot(refGrid)
