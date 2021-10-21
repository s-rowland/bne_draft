# File: LGC_b_04a_filter_JS_make_refGrid.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 8/20/21
#
# Contents:
# N. Notes
# 0. Package Imports
# 1. Read in EPA data
# 2. Read in JS site codes
# 3. Spatial Join
# 4. Predictions Dataset

#### ------------------- ####
####       N. NOTES      ####
#### ------------------- ####
# The point of this script is to create two files:
#
# (1) epa-js_nn_key.csv, which maps EPA monitor locations to
#     their nearest neighbour JS <lat, lon> coordinate pairs, along with 
#     recording the index of the JS pair. In this way, a "key" is made, allowing
#     someone to extract only the points of interest from a JS .rda file using the
#     "js_index" column from epa-js_nn_key.csv (= "nnTable" in this file before writing the csv).
#
# (2) js_preds_ref_grid.csv, which simply contains a random 1% of JS's data to be used 
#     as a reference grid for a predictions dataset. A plot of the spatial distribution 
#     can be found towards the end of the file.

#### ------------------- ####
####  0. PACKAGE IMPORTS ####
#### ------------------- ####

# 0a. load packages and functions required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_set_up_env.R"))
}


#### ------------------- ####
#### 1. READ IN AQS DATA ####
#### ------------------- ####

# 1a. read in AQS data:
aqsPath <- here::here('BNE_inputs', 'ground_truth', 'formatted', 
                      'lgc_annual_data_2000-2016.csv')

aqs <- loadData(aqsPath, "AQS_annual")

#### ------------------- ####
#### 2. READ IN JS sites ####
#### ------------------- ####

jsPath <- here::here('BNE_inputs', 'input_models', 'raw', 'JS_annual_raw', 
                     'USGridSite.rds')
js <- loadData(jsPath, "JSSITES")

#### ------------------- ####
####   3. SPATIAL JOIN   ####
#### ------------------- ####
# the spatial join performed and saved in the lines below
# comprise the possible training data points from JS
# they still need to be temporally joined to the EPA data...
# that will be done in LGC_d01_process_JS.R
epaLocations <- aqs %>%
  dplyr::select(ref_id, lat, lon) %>%
  dplyr::rename(epa_lat = lat, epa_lon = lon) %>%
  dplyr::distinct()
# 1571 x 3

epaLocations.sf <- epaLocations %>%
  sf::st_as_sf(coords = c("epa_lon", "epa_lat"), crs = sf::st_crs("epsg:4326")) %>%
  sf::st_transform(crs = sf::st_crs("epsg:2163"))

jsLocations.sf <- js %>%
  sf::st_as_sf(coords = c("js_lon", "js_lat"), crs = sf::st_crs("epsg:4326")) %>%
  sf::st_transform(crs = sf::st_crs("epsg:2163"))
# this should take ~3min

tic()
jsIndices <- unlist(nngeo::st_nn(epaLocations.sf, jsLocations.sf, k = 1))
toc()
# this should take ~20 seconds

nnTable <- js[jsIndices, ] %>%
  cbind(epaLocations, .) %>%
  tibble::as_tibble() %>%
  dplyr::arrange(js_index) %>%
  dplyr::rename(epa_id = ref_id)

nnTable %>% 
  readr::write_csv(here::here('BNE_inputs','keys', "epa-js_nn_key.csv"))
                 

#### ------------------- ####
#### 4. PREDICTIONS DATA ####
#### ------------------- ####

# the spatial join performed and saved in the lines below
# comprise the possible training data points from JS
# they still need to be temporally joined to the EPA data...
# that will be done in LGC_d01_process_JS.R
refGrid <- fst::read_fst(here::here('data_ancillary', 'generated',  
                           'refCities.fst'))

refGridLocations <- refGrid %>%
  dplyr::mutate(ref_id = row_number()) %>%
  dplyr::select(ref_id, lat, lon) %>%
  dplyr::rename(ref_lat = lat, ref_lon = lon) %>%
  dplyr::distinct()
# 1571 x 3

refGridLocations.sf <- refGridLocations %>%
  sf::st_as_sf(coords = c("ref_lon", "ref_lat"), crs = sf::st_crs("epsg:4326")) %>%
  sf::st_transform(crs = sf::st_crs("epsg:2163"))

jsLocations.sf <- js %>%
  sf::st_as_sf(coords = c("js_lon", "js_lat"), crs = sf::st_crs("epsg:4326")) %>%
  sf::st_transform(crs = sf::st_crs("epsg:2163"))
# this should take ~3min

tic()
jsIndices <- unlist(nngeo::st_nn(refGridLocations.sf, jsLocations.sf, k = 1))
toc()
# this should take ~20 seconds

nnTable <- js[jsIndices, ] %>%
  cbind(refGridLocations, .) %>%
  tibble::as_tibble() %>%
  dplyr::arrange(js_index) %>%
  dplyr::rename(epa_id = ref_id)

nnTable %>% 
  readr::write_csv(here::here('BNE_inputs','keys', "js_preds_refCities.csv"))

# 4b. MAPPING predsKey:

# load the base map from our shapefile
USA.shp <- rgdal::readOGR(dsn = "~/Documents/Research_Marianthi/BNE_project/EPA_data/cb_2016_us_state_500k/", layer = "cb_2016_us_state_500k")

# transform geographical coordinates to Lambert Azimuth Equal Area Projection
USA.aea <- sp::spTransform(USA.shp, sp::CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))

# get rid of regions we don't care about.
# we don't care about alaska, hawaii, puerto rico, virgin islands...
# because they're annoying to deal with when plotting the maps.
# plus, those regions don't have many monitors anyway.
# we don't care about the rest because we have no monitor data for them.
# now we have our map ready in this USA.cont Large SpatialPolygonsDataFrame object
USA.cont <- USA.aea[!USA.aea$NAME %in% c("Alaska", "Hawaii", "Puerto Rico", "Commonwealth of the Northern Mariana Islands", "Guam", "American Samoa", "United States Virgin Islands"), ]

preds.wgs84 <- sp::SpatialPointsDataFrame(coords = predsKey[, c("js_lon", "js_lat")], data = predsKey, proj4string = sp::CRS("+init=epsg:4326"))
preds.aea <- sp::spTransform(preds.wgs84, sp::CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))

predsMap <- tm_shape(USA.cont, projection=sp::CRS(sp::proj4string(USA.cont))) + 
  tm_polygons("white") +
  tm_layout(title = "predsKey plotted (check for uniform distribution)", title.position = c("center", "top"), title.size = 1, frame=FALSE, bg.color="lightblue", inner.margins = c(.2,.2,.1,.02)) + 
  tm_shape(preds.aea, projection = sp::CRS(sp::proj4string(preds.aea))) + 
  tm_symbols(size = 0.001) 

tmap_mode("plot")
predsMap



