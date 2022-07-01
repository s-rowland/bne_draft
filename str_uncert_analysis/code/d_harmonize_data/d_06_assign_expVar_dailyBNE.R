# Task: Harmonize daily predictions and Potential Explanatory Variables
# File: c_02_harmonize_daily.R
# SubProject: Analysis of BNE PM2.5 Predictive Uncertainty
# Project: Bayesian Nonparametric Ensemble 
# Author: Sebastian T. Rowland <sr3463@cumc.columbia.edu>

#### ------------------- ####
####  Table of Contents  ####
#### ------------------- ####

#  N: Notes
#  0: preparation 
#  1. readin in constant datasets
#  2. main loop
#  2A. process date
#  2B. bring in bne preds
#  2C. population density
#  2D. aqs obs
#  2E. Topography
#  2F. ERA5 land variables
#  2G. ERA single layer variables
#  2H: mixing height 
#  2I: within 20km shoreline


#### ---------- ####
####  N: NOTES  ####
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

# 1.a. okay, first we readin the refgrid 
refGrid <- fst::read_fst(here::here('inputs', 'pm25', 'reference_grids', 
                                      'refGrid_conus.fst'))
# 1.b. for speed, sample 10% of observations 
#refGrid <- refGrid %>% 
 # slice_sample(prop = .1)

# 1.c. bring in aqs data 
aqs <- read_csv(here::here('inputs', 'pm25', 'training_datasets', 'daily_combined', 
                           'training_cvfolds.csv'))

# 1.d. bring in popD layer 
popD <- sf::st_read(here::here(dir.proj, 'data', 'explanatory_variables', 'intermediate',
                               'census_decSurvey_2010', 'popDen_2010.shp'))

# 1.e. bring in topography layer
topo <- read_csv(here::here(dir.proj, 'data', 'explanatory_variables', 'intermediate', 
                     'topo_processed.csv'))

# 1.f. bring in shoreline layer 
shoreline <- read_csv(here::here(dir.proj, 'data', 'explanatory_variables', 'intermediate', 
                                 'shoreline.csv'))

# 1.a. bring in the eparegion-state key that we previously generated
epaRegions <- read_csv(here::here('ancillary_data', 'generated', 'epa_regions.csv'))

# 1.f. bring in keys 
key.ref.popD <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                    'key_nn_refGridConus_popD2000.fst'))
key.ref.topo <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                    'key_nn_refGridConus_topo.fst'))
key.ref.era5Land <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                    'key_nn_refGridConus_eraLand.fst'))
key.ref.era5Single <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                    'key_nn_refGridConus_eraSingle.fst'))
key.ref.era5BoundaryH <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                          'key_nn_refGridConus_eraBoundaryH.fst'))
# 1.g. establising features for the loop 
#years <- c(2010) # for now we only have 2010 data, sorry. 
#n <- length(years)
yyyy <- 2010
#### -------------- ####
####  2. main loop  ####
#### -------------- ####

predFiles <- data.frame(pred_files = list.files(here::here(dir.proj, 'outputs', 
                                                          'd_bne_results/preds'))) %>% 
  filter(str_detect(pred_files, '1stage'))


# Begin loop 
for (i in 1:nrow(predFiles)) {
#for ( dayOfYear in 1:365) {
#for (dayOfYear in 1:2) {
  yyyy <- str_sub(predFiles$pred_files[i], 14, 17)
  dayOfYear.str <- str_sub(predFiles$pred_files[i], 19, 21)
  dayOfYear.num <- as.numeric(dayOfYear.str)
  #### ------------------ ####
  ####  2A. process date  ####
  #### ------------------ ####
  
  # 2A.a. determine the date 
  activeDate <- parse_date_time(paste0('01/01/',yyyy), 'dmy') +
    as.duration((dayOfYear.num-1)*60*60*24)
  
  # 2A.b. extract date components
  yyyy <- year(activeDate)
  mm <- month(activeDate)
  dd <- day(activeDate)
  
  #### ------------------------ ####
  ####  2B. bring in bne preds  ####
  #### ------------------------ ####
  
  # 2B.a readin the predictions 
  # keep only the most relevant variables
  preds.day <- read_csv(here::here('inputs', 'pm25', 'prediction_datasets', 
                               'daily_individual', 
                               paste0('preds_',yyyy, '_', dayOfYear.str, '.csv')))  %>% 
    dplyr::select(lat, lon, pred_av, pred_cm, pred_js, pred_me, pred_rk)
  
  # 2B.b. create ref_id variable to help us keep track of locations
  preds.day <- preds.day %>% 
    dplyr::mutate(ref_id = row_number()) 
  
  #pred.day <- preds.day %>% 
   # sample_slice(n = 50000) 
  
  #### ------------------------ ####
  ####  2C. population density  ####
  #### ------------------------ ####
  
  # 2C.a. apply key to predictions
  preds.day <- preds.day %>% 
    inner_join(key.ref.popD, by = c('lat' = 'ref_lat', 'lon' = 'ref_lon'))
  
  # 2C.b. assign population density
  preds.day <- popD %>% 
    slice(preds.day$baseModel_id) %>% 
    as.data.frame() %>% 
    dplyr::select(popDen_m2, state) %>%
    bind_cols(preds.day)
  
  # 2C.c. clean up dataframe 
  preds.day <- preds.day %>% 
    dplyr::select(-contains('base'))
  
  #### ------------- ####
  ####  2D. aqs obs  ####
  #### ------------- ####
  
  # 2D.a. isolate to aqs of that day
  # consider also looking forward and backwards by 3 days 
  aqs.day <- aqs %>% 
    mutate(ddate = julian_day*60*60*24 + parse_date_time('2005/01/01', 'ymd')) %>%
    filter(year(ddate) == yyyy & month(ddate) == mm & day(ddate) == dd)
  
  # 2D.b. find distance to nearest neighbor
  # 2D.b.i. convert to sf
  aqs.day.sf <- aqs.day %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs("epsg:4326")) 
  
  preds.day.sf <- preds.day %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs("epsg:4326")) 
  # 2D.b.ii. get nn distance
  preds.day$aqs_dist <- nngeo::st_nn(preds.day.sf, aqs.day.sf, k=1, returnDist = TRUE)[2]$dist %>% 
    unlist()
  
  #### ---------------- ####
  ####  2E. Topography  ####
  #### ---------------- ####
  
  # 2E.a. apply key to predictions
  preds.day <- preds.day %>% 
    inner_join(key.ref.topo, by = c('lat' = 'ref_lat', 'lon' = 'ref_lon'))
  
  # 2E.b. assign population density
  preds.day <- topo %>% 
    slice(preds.day$baseModel_id) %>% 
    as.data.frame() %>% 
    dplyr::select(elev) %>%
    bind_cols(preds.day)
  
  # 2E.c. clean up dataframe 
  preds.day <- preds.day %>% 
    dplyr::select(-contains('base'))
  
  #### ------------------------- ####
  ####  2F. ERA5 land variables  ####
  #### ------------------------- ####
  
  # 2F.a. update the era5 datasets when we start the new month. 
  #if (dd == 1) {
    era5.land <- fst::read_fst(here::here(dir.proj, 'data', 'explanatory_variables', 
                                          'intermediate', 'era5', 
                                          paste0(yyyy, '_', pad0(mm), '_', 'land', '.fst')))
  #}
  
  # 2F.b. keep just the columns of interest 
  era5.land.day <- era5.land %>% 
    dplyr::select(contains(pad0(dd)))
  names(era5.land.day) <- str_sub(names(era5.land.day), 1, -4)
  # 2F.c. apply key to predictions
  preds.day <- preds.day %>% 
    inner_join(key.ref.era5Land, by = c('lat' = 'ref_lat', 'lon' = 'ref_lon'))
  
  # 2F.d. assign population density
  preds.day <- era5.land.day %>% 
    slice(preds.day$baseModel_id) %>% 
    bind_cols(preds.day)
  
  # 2F.e. clean up dataframe 
  preds.day <- preds.day %>% 
    dplyr::select(-contains('base'))
  
  #### -------------------------------- ####
  ####  2G. ERA single layer variables  ####
  #### -------------------------------- ####
  
  # 2G.a update the era5 datasets when we start the new month. 
  #if (dd == 1) {
    era5.single <- fst::read_fst(here::here(dir.proj, 'data', 'explanatory_variables', 
                                   'intermediate', 'era5', 
                                   paste0(yyyy, '_', pad0(mm), '_', 'singleLayer', '.fst')))
  #}
  
  # 2G.b. keep just the columns of interest 
  era5.single.day <- era5.single %>% 
    dplyr::select(contains(pad0(dd)))
  names(era5.single.day) <- str_sub(names(era5.single.day), 1, -4)
  
  # 2G.c. apply key to predictions
  preds.day <- preds.day %>% 
    inner_join(key.ref.era5Single, by = c('lat' = 'ref_lat', 'lon' = 'ref_lon'))
  
  # 2G.d. assign population density
  preds.day <- era5.single.day %>% 
    slice(preds.day$baseModel_id) %>% 
    bind_cols(preds.day)
  
  # 2G.e. clean up dataframe 
  preds.day <- preds.day %>% 
    dplyr::select(-contains('base'))
  
  #### ------------------- ####
  ####  2H: mixing height  ####
  #### ------------------- ####
  
  # 2H.a update the era5 datasets when we start the new month. 
  #if (dd == 1) {
    era5.boundaryH <- fst::read_fst(here::here(dir.proj, 'data', 'explanatory_variables', 
                                            'intermediate', 'era5', 
                                            paste0(yyyy, '_', pad0(mm), '_', 
                                                   'boundaryH', '.fst')))
  #}
  
  # 2G.b. keep just the columns of interest 
  era5.boundaryH.day <- era5.boundaryH %>% 
    dplyr::select(contains(pad0(dd)))
  names(era5.boundaryH.day) <- str_sub(names(era5.boundaryH.day), 1, -4)
  
  # 2G.c. apply key to predictions
  preds.day <- preds.day %>% 
    inner_join(key.ref.era5BoundaryH, by = c('lat' = 'ref_lat', 'lon' = 'ref_lon'))
  
  # 2G.d. assign population density
  preds.day <- era5.boundaryH.day %>% 
    slice(preds.day$baseModel_id) %>% 
    bind_cols(preds.day)
  
  # 2G.e. clean up dataframe 
  preds.day <- preds.day %>% 
    dplyr::select(-contains('base'))
  
  #### --------------------------- ####
  ####  2I: within 20km shoreline  ####
  #### --------------------------- ####

  #pred.day <- pred.day %>% 
   # mutate(ref_id = row_number()) 
  
  preds.day$shore <- shoreline$shore
    
  #pred.day.sf <- preds.day %>% 
   # sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326")) %>% 
    #dplyr::select(shore)
  
 # plot(pred.day.sf)
  
  #### -------------------------- ####
  ####  2H: processing variables  ####
  #### -------------------------- ####
  # assign epa region
  preds.day <- preds.day %>%
    inner_join(epaRegions, by = 'state')
  
  # compute RH 
  # from https://bmcnoldy.rsmas.miami.edu/Humidity.html
  preds.day <- preds.day %>% 
    mutate(tDew2 = tDew2- 273.15, tMean2 = tMean2 - 273.15, tMean = tMean - 273.15) %>%
    mutate(rh = 100* (exp((17.625*tDew2)/(243.04+tDew2))/exp((17.625*T)/(243.04+T))))
  
  # compute wind speed 
  preds.day <- preds.day %>% 
    mutate(windSpeed = sqrt(windU^2 + windV^2))
  
  #### ----------------------- ####
  ####   9: Save Result  ####
  #### ----------------------- ####
  
  preds.day %>% 
    write_fst(here::here(dir.proj, 'data', 'combined', 'daily', 
                         paste0('pred_vars_', yyyy, '_',dayOfYear.str, '.fst')))
  print(paste0('completed ', dayOfYear))
}

