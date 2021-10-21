# File: LGC_d_01_make_training_predictions_JS.R
# Authors:
# Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 9/03/21
#
# Contents:
# N. Notes
# 0. Package Imports
# 1. General Setup
# 2. Process JS by month
# 3. Download ZIP
# 4. Unzip Files
# 5. Process JS day by day
# 6. Join w/EPA training data
# 7. Get Ref Grid for predictions data
# 8. Save that month's work
# 9. Delete ZIP and raw JS data

#### ------------------ ####
####       N. NOTES     ####
#### ------------------ ####
# Before running this script, you need to set up two files as described in this tutorial:
# https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget
# 
# (1) ~/.netrc, which should contain only the following on the first line: 
#     "machine urs.earthdata.nasa.gov login [uid_goes_here] password [password_goes_here]"
# (2) ~/.urs_cookies, which can be empty.
#
# Once that is done, this script can be run. The point of this script is to extract
# relevant PM2.5 predictions from the JS model to finish creating the daily training data,
# as well as to create a reference grid for the predictions dataset, based off a random 1% of the JS data...
#
# To find the JS data, visit:
# https://beta.sedac.ciesin.columbia.edu/data/set/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016/data-download#close

#### ------------------ ####
#### 0. PACKAGE IMPORTS ####
#### ------------------ ####

# 0a. load packages and functions required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_set_up_env.R"))
}

#### ------------------ ####
####  1. GENERAL SETUP  ####
#### ------------------ ####
# specify the years and months we are interested in:
years <- c(2010:2015)
timeSteps <- expand.grid(list("year" = years)) %>%
  dplyr::arrange(year)

# define the link to download the data & working directory:
prefix <- "https://beta.sedac.ciesin.columbia.edu/downloads/data/aqdh/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016-"
suffix <- "-rds.zip"
wd <- "~/Downloads/"

# read in EPA data:
aqsPath <- here::here('BNE_inputs', 'ground_truth', 'formatted', 
                      'lgc_annual_data_2000-2016.csv')
epa <- loadData(aqsPath, "AQS_annual")

# restrict AQS monitors to CONUS 
# 1c.i. bring in states shape file 
states <- sf::read_sf(here::here('data_ancillary', 'raw', 'Census','cb_2015_us_state_500k', 
                                 'cb_2015_us_state_500k.shp')) %>% 
  st_transform(., crs=st_crs(projString))
# 1c.ii. restrict to conus 
states <- states %>% 
  dplyr::mutate(area = ALAND + AWATER) %>% 
  dplyr::filter(!(STUSPS%in%c('HI', 'AK', 'VI'))) %>% 
  dplyr::rename(state = STUSPS) %>% 
  dplyr::select(state)
# 1c.iii make epa spatial 
epa <- epa   %>% 
  sf::st_as_sf(., coords = c("lon", "lat"), crs=sf::st_crs('epsg:4326')) %>% 
  sf::st_transform(., crs=st_crs(projString))
# 1c.iii. join
epa <- sf::st_intersection(states, epa, join = st_intersects)
# 1c.iv make not-spatial 
epa <- epa %>% 
  sf::st_transform(., crs=sf::st_crs('epsg:4326')) %>%
  dplyr::mutate(lat = sf::st_coordinates(.)[,2], 
                lon = sf::st_coordinates(.)[,1],) %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry, -state)

# read in JS files:
jsKey <- loadData(here::here('BNE_inputs','keys', "epa-js_nn_key.csv"), "JSEPAKEY")


jsRefGrid <- loadData(here::here('BNE_inputs','keys', "js_preds_refCities.csv"), "JSREF")

# progress bar:
n <- as.numeric(as.Date("2017-01-01") - as.Date("2010-01-01"))

progressBar <- txtProgressBar(min=0, max=6, width=50, style=3)
counter <- 0
pb <- function(p) { setTxtProgressBar(progressBar, p); return(p+1) }
counter <- pb(counter)

# set up output directories:
outDir.epa <- here::here('BNE_inputs','training_datasets', "EPA-JS_training_data")
outDir.ref <- here::here('BNE_inputs','prediction_datasets', "JSrefCities")

dir.create(outDir.epa)
dir.create(outDir.ref)

#### ---------------------------- ####
#### 2. PROCESS JS MONTH BY MONTH ####
#### ---------------------------- ####
for (i in 1:nrow(timeSteps)) {
  
  #### --------------- ####
  #### 3. DOWNLOAD ZIP ####
  #### --------------- ####

  
  #### --------------- ####
  ####  4. UNZIP FILES ####
  #### --------------- ####


  #### ------------------------- ####
  ####  5. PROCESS JS DAY BY DAY ####
  #### ------------------------- ####
  
  # 5b create empty tables to store the predictions of interest and reference grid 
  epa.js <- tibble::tibble()
  refGrid <- tibble::tibble()

  # 5c Read JS predictions
    preds <- readr::read_rds(
      here::here('BNE_inputs', 'input_models', 'raw', 'JS_annual_raw', 
                 paste0('PredictionStep2_Annual_PM25_USGrid_',
                        timeSteps$year[i], '0101_', timeSteps$year[i], '1231.rds')))
    
    preds <- tibble::as_tibble(as.vector(t(preds)))
    names(preds) <- 'js_pred'
    
    #### ---------------------------- ####
    ####  6. JOIN W/EPA TRAINING DATA ####
    #### ---------------------------- ####
    
    # 6a restrict to timeStep
    epa.timeStep <- epa %>% 
      dplyr::filter(year == timeSteps$year[i],)
    
    # 6b we have this condition just in case aqs data is missing for one day 
    if (nrow(epa.timeStep) > 0) {
      
      preds.epa <- preds %>%
        dplyr::slice(jsKey$js_index)
      
      jsExtract <- cbind(jsKey, preds.epa) %>%
        tibble::as_tibble() %>%
        dplyr::select(epa_id, js_pred)
      
      epa.js <- dplyr::inner_join(epa.timeStep, jsExtract, by = c("ref_id" = "epa_id"))
      
    } else {
      
      epa.js <- tibble::tibble()
      
    }
    
    # record:
    epa.js <- epa.js
    
    #### ----------------------------------- ####
    ####  7. GET JS PREDICTIONS FOR REF GRID ####
    #### ----------------------------------- ####
    # 1% of Joel's data
    preds.ref <- preds %>%
      dplyr::slice(jsRefGrid$js_index)
    
    refGrid <- cbind(jsRefGrid, preds.ref) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(year = as.character(timeSteps$year[i])) %>%
      dplyr::rename(lat = js_lat, lon = js_lon, ref_id = js_index) %>%
      dplyr::select(ref_id, lat, lon, year,  js_pred)
    
    # record:
    refGrid <- refGrid
    
    # progress bar:
    counter <- pb(counter)
  
  
  #### -------------------------- ####
  ####  8. SAVE THAT MONTH'S WORK ####
  #### -------------------------- ####
  # once we finish a month, save that month's work:
refGrid %>% 
  readr::write_csv(here::here('BNE_inputs','prediction_datasets', "JSrefGrid", 
                       paste0("js_cities_", timeSteps$year[i], ".csv")))

}
