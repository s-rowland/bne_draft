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
cbsa <- sf::st_read(here::here('data_ancillary', 'raw', 'Census', 'tl_2015_us_cbsa', 
                          'tl_2015_us_cbsa.shp'))

# 1c. remove regions that will most likely not be included in the 
# contiguous nationwide application 
cityNames <- c('Boston-Cambridge-Newton, MA-NH Metro Area',
  'New York-Newark-Jersey City, NY-NJ-PA Metro Area',
  'Los Angeles-Long Beach-Anaheim, CA Metro Area',
  'Chicago-Naperville-Elgin, IL-IN-WI Metro Area')

cities <- cbsa[cbsa$NAMELSAD%in%cityNames,]

#-------------------------------------------------------------------#
#### 2. IDENTIFY GRID CENTROIDS WITHIN CONUS-INSCRIBED RECTANGLE ####
#-------------------------------------------------------------------#

dta <- data.frame(lat = NA, lon = NA)

for (i in 1:length(cityNames)){
city <- cities %>% 
  filter(NAMELSAD == cityNames[i])
# 2a. convert geometry to character format
# we can more easily manipulate characters
grid <- as.character(city$geometry) 

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
seqLat <- seq(min.lat, max.lat, by = 0.025)
seqLon <- seq(min.lon, max.lon, by = 0.025)

# 2g. combine coordinate elements into grids
dta <- expand.grid(lat = seqLat, lon = seqLon) %>% 
          bind_rows(dta)

# 2h. clean up
rmList <- c(ls(pattern="grid"), ls(pattern="lat"), ls(pattern="lon"), ls(pattern='seq'))
rm(list = rmList)

}


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

cities <- cities %>% 
  sf::st_transform(crs=st_crs(projString))

dta.extr.sf.a <- dta.extr.sf %>% 
  sf::st_join(cities, join = st_within) %>% 
  dplyr::filter(!is.na(NAMELSAD)) %>% 
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
                            'refCities.fst'))

# confirm that it looks good. 
refGrid <-
  fst::read_fst(here::here('data_ancillary', 'generated',  
                            'refCities.fst'))
# 3d. transform geographical coordinates to Lambert Azimuth Equal Area Projection
refGrid <- refGrid %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs=st_crs("epsg:4326")) %>% 
  sf::st_transform(crs=st_crs(projString))

plot(refGrid)
