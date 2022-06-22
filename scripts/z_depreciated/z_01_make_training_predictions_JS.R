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

# 1a. specify the years and months we are interested in:
years <- c(2010:2015)
timeSteps <- expand.grid(list("year" = years)) %>%
  dplyr::arrange(year)

# 1b. read in EPA data:
aqsPath <- here::here('BNE_inputs', 'ground_truth', 'formatted', 
                                  'lgc_annual_data_2000-2016.csv')

aqs <- loadData(aqsPath, "AQS_annual")


# restrict to conus
# 0b. read CONUS shapefile
conus <- sf::st_read(here::here('data_ancillary', 'formatted', 'spatial_outlines', 
                                'conus.shp'))
# 2f.ii. convert projection of CONUS
conus.epsg4326 <- conus %>% 
  sf::st_transform(crs=st_crs('epsg:4326'))
# 2f.iii. intersect JS and CONUS
aqs.sf <- aqs %>% 
  sf::st_as_sf(., coords = c("lon", "lat"), 
               crs=st_crs('epsg:4326'))  %>%
  sf::st_join(conus.epsg4326, st_intersects) %>% 
  dplyr::filter(!is.na(g)) %>% 
  dplyr::select(-g, -m)




  

#aqs <- readr::read_csv(here::here('BNE_inputs', 'ground_truth', 'formatted',
#                           'aqs_annual_2010_formatted.csv')) %>% 
#  mutate(year = as.character(time)) %>% 
#  dplyr::select(-pred_AV, -pred_SC, -pred_GS, -pred_CM, -time) %>% 
#  rename(obs_pm2_5 = pm25_obs)

# 1c. list of years
YYYYlist <- 2010:2015

# 1d. list of codes defining the datasets
datasetCodes <- c('AV', 'GS', 'CMAQOUTS_annual', 'JS_annual', 'CACES')
modelNames <- c("av", "gs", "cmaq_outs", 'js', "caces")
overrides <- c(T, T, F, F, T) # used to be c(T, F, F, F)
censusTrackFiles <- list(NULL, NULL, NULL, NULL, NULL)

#----------------------------------------#
####  2. LOOP OVER YEARS ####
#----------------------------------------#

# 2a. begin loop
for (i in 1:length(YYYYlist)){
  
  #------------------------------------------#
  ####  2A. CREATE YEAR-SPECIFIC ELEMENTS ####
  #------------------------------------------#
  
  # 2A.a. add the year to the refGrid
  train.yyyy <- aqs %>% 
    dplyr::filter(year == as.character(YYYYlist[i]))
  
  # 2A.b. list of paths where the annual data is stored
  paths <- c(
    # AV
    here::here('BNE_inputs', 'input_models', 'raw', 'av_annual_raw', 
               paste0('V4NA03_PM25_NA_', YYYYlist[i], '01_', YYYYlist[i], '12-RH35.nc')),
    # GS 
    here::here('BNE_inputs', 'input_models', 'raw', 
               'gs_annual_raw', 'GBD2016_PREDPOP_FINAL.rdta'), 
    #CMout
    here::here('BNE_inputs', 'input_models', 'formatted', 'cmout_annual_formatted',
               paste0('cmout_annual_',YYYYlist[i], '_formatted.fst')),
    # JS 
   here::here('BNE_inputs', 'input_models', 'formatted', 'js_annual_formatted', 
               paste0('js_annual_', YYYYlist[i], '_formatted.fst')),
    # CACES
    here::here('BNE_inputs', 'input_models', 'raw', 
               'cc_annual_raw', 
               paste0('CACES_annual_', YYYYlist[i], '_blockGrp_raw.csv'))
  )
  
  #-----------------------------------------#
  ####  2B. INNER LOOP OVER INPUT MODELS ####
  #-----------------------------------------#
  
  # 2B.a. begin loop
  for (j in 1:length(datasetCodes)){
    
    # 2B.b. read dataset of input model's predictions
    modelData <- loadData(path = paths[j], dataset = datasetCodes[j]) %>%
      dplyr::filter(year == as.character(YYYYlist[i]))
    
    # 2B.c. remove the fips column if included
    if(stringr::str_detect(ls(modelData)[1], 'fips')){
      modelData <- modelData %>% 
        dplyr::select(-fips)
    }
    
    # 2B.d. conduct spatial join 
    train.yyyy <- spatioTemporalJoin(refData = train.yyyy,
                                       modelData = modelData,
                                       modelName = modelNames[j],
                                       override = overrides[j],
                                       censusTractFile = censusTrackFiles[[j]])
  }
  
  # 2B.e. remove duplicates 
  # for some reason the function creates 147 duplicate rows 
  train.yyyy <- train.yyyy %>% 
    distinct()
  
  #-------------------------#
  ####  2C. SAVE DATASET ####
  #-------------------------#
  
  # 2C.a. save results
  train.yyyy %>% 
    readr::write_csv(here::here('BNE_inputs', 'training_datasets', 'annual', 
                                paste0('train_avgscmjscc_', YYYYlist[i], '_', fold, '.csv')))
  
}