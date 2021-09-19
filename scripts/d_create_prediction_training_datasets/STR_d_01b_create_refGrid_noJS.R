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

# 1a. set excluded areas 
excludedAreas <- c("Alaska", "Hawaii", "Puerto Rico", 
                   "Commonwealth of the Northern Mariana Islands", "Guam", 
                   "American Samoa", "United States Virgin Islands")
# 1b. load the base map from our shapefile
usa <- sf::st_read(here::here('data_ancillary', 'raw', 'Census', 'cb_2015_us_state_500k', 
                          'cb_2015_us_state_500k.shp'))

# 1c. remove regions that will most likely not be included in the 
# contiguous nationwide application 
conusStates <- usa[!usa$NAME%in%excludedAreas,]

#-------------------------------------------------------------------#
#### 2. IDENTIFY GRID CENTROIDS WITHIN CONUS-INSCRIBED RECTANGLE ####
#-------------------------------------------------------------------#

# 2a. convert geometry to character format
# we can more easily manipulate characters
grid <- as.character(conusStates$geometry) 

# 2b. replace various characters so that the coordiantes looks like a list of 
# numbers
grid2 <- stringr::str_replace_all(grid, 'list\\(', ',')
grid3 <- stringr::str_replace_all(grid2, 'c\\(', ',')
grid4 <- stringr::str_replace_all(grid3, ',,,', ',')
grid5 <- stringr::str_replace_all(grid4, ',,', ',')
grid6 <- stringr::str_replace(grid5, ",", '')

# 2c. split up the grids so that each coordinate element is its own string
grid7 <- str_split(grid6, ",")

# 2d. put all of these together into one vector
grid8 <- unlist(grid7)

# 2e. convert to numeric 
# you will get warning: 'NAs introduced by coercion'; this can safely be ignored
loc <- data.frame(coordinates = as.numeric(unlist(grid8)))

# 2f. break up into latitude and longitude
lat <- loc %>% dplyr::filter(coordinates >0)
lon <- loc %>% dplyr::filter(coordinates <0)
 
# 2g. find the minimums and maximums
min.lat <- min(lat$coordinates, na.rm = TRUE)
max.lat <- max(lat$coordinates, na.rm = TRUE)
min.lon <- min(lon$coordinates, na.rm = TRUE)
max.lon <- max(lon$coordinates, na.rm = TRUE)

# 2h. make vectors of all the possible coordiante elements 
# this is the key step were we decide the number of elements, 
# the size of the grids, etc. 
seqLat <- seq(min.lat, max.lat, by = 0.125)
seqLon <- seq(min.lon, max.lon, by = 0.125)

# 2g. combine coordinate elements into grids
dta <- expand.grid(lat = seqLat, lon = seqLon)

# 2h. clean up
rmList <- c(ls(pattern="grid"), ls(pattern="lat"), ls(pattern="lon"), ls(pattern='seq'))
rm(list = rmList)

#--------------------------------------#
#### 3. RESTRICT CENTROIDS TO CONUS ####
#--------------------------------------#

# 3a. Read CONUS shape file
conus <- sf::st_read(here::here('data_ancillary', 'formatted', 'spatial_outlines', 
                            'conus.shp'))

# 3b. identify points with extreme lat or lon that might be outside of conus
dta <- dta %>% 
  dplyr::mutate(lat_extr = if_else(lat < quantile(dta$lat, 0.33)[1] | 
                                  lat > quantile(dta$lat, 0.70)[1], 1, 0), 
         lon_extr = if_else(lon < quantile(dta$lon, 0.2)[1] | 
                            lon > quantile(dta$lon, 0.8)[1], 1, 0)) 

dta.extr <- dta %>% 
  filter(lat_extr + lon_extr > 0) 

dta.noExtr <- dta %>% 
  filter(lat_extr + lon_extr == 0) 
  
# 3c. convert to spatial (simple feature)
dta.extr.sf <- dta.extr %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs=st_crs("epsg:4326"))

# 3d. transform geographical coordinates to Lambert Azimuth Equal Area Projection
dta.extr.sf <- dta.extr.sf %>% 
  sf::st_transform(crs=st_crs(projString))

# 3e. intersect with CONUS, keep only overlapping points
# an alternative to to just intersect the points that are in the top or botto 
# 20th percentile for lat or for long 

dta.extr.sf.a <- dta.extr.sf %>% 
  slice(1:(nrow(dta.extr.sf)/5)) %>% 
  sf::st_join(conus, join = st_within) %>% 
  dplyr::filter(!is.na(g)) %>% 
  sf::st_transform(crs=st_crs("epsg:4326"))

dta.extr.sf.b <- dta.extr.sf %>% 
  slice((1+1*floor(nrow(dta.extr.sf)/5)): (2*floor(nrow(dta.extr.sf)/5))) %>% 
  sf::st_join(conus, join = st_within) %>% 
  dplyr::filter(!is.na(g)) %>% 
  sf::st_transform(crs=st_crs("epsg:4326"))

dta.extr.sf.c <- dta.extr.sf %>% 
  slice((1+2*floor(nrow(dta.extr.sf)/5)): (3*floor(nrow(dta.extr.sf)/5))) %>% 
  sf::st_join(conus, join = st_within) %>% 
  dplyr::filter(!is.na(g)) %>% 
  sf::st_transform(crs=st_crs("epsg:4326"))

dta.extr.sf.d <- dta.extr.sf %>% 
  slice((1+3*floor(nrow(dta.extr.sf)/5)): (4*floor(nrow(dta.extr.sf)/5))) %>% 
  sf::st_join(conus, join = st_within) %>% 
  dplyr::filter(!is.na(g)) %>% 
  sf::st_transform(crs=st_crs("epsg:4326"))

dta.extr.sf.e <- dta.extr.sf %>% 
  slice((1+4*floor(nrow(dta.extr.sf)/5)): (5*floor(nrow(dta.extr.sf)/5))) %>% 
  sf::st_join(conus, join = st_within) %>% 
  dplyr::filter(!is.na(g)) %>% 
  sf::st_transform(crs=st_crs("epsg:4326"))
refGrid <- dta.extr.sf.a %>%
  bind_rows(dta.extr.sf.b) %>%
  bind_rows(dta.extr.sf.c) %>%
  bind_rows(dta.extr.sf.d) %>%
  bind_rows(dta.extr.sf.e)
plot(refGrid)

# 3f. combine 
refGrid <- dta.extr.sf.a %>%
  bind_rows(dta.extr.sf.b) %>%
  bind_rows(dta.extr.sf.c) %>%
  bind_rows(dta.extr.sf.d) %>%
  bind_rows(dta.extr.sf.e) %>%
  dplyr::mutate(lon = st_coordinates(.)[,1], 
         lat = st_coordinates(.)[,2]) %>%
  as.data.frame() %>% 
  dplyr::select(lat, lon) %>%
  bind_rows(dta.noExtr) 

# 3g. save the centroids
refGrid %>% 
  dplyr::select(lat, lon) %>%
  fst::write_fst(here::here('data_ancillary', 'generated',  
                            'refGrid_0125Deg_Centroids.fst'))

