# File: STR_d_02_make_training_predictions_JS.R
# Authors:
# Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 03/17/23
#
# Contents:
#  N. notes
#  0. Package Imports
#  1. general set up
#  2. wrangle base models
#  3. make training and prediction Ddta by day
#  3A. process date variables and aqs
#  3B. add av 
#  3C. add cb
#  3D. add cm 
#  3E. add js 
#  3F. add me
#  3G. add rk
#  3H. save datasets
#  4. save training dataset

#### ------------------ ####
####       N. notes     ####
#### ------------------ ####


#yyyy <- 2010


#### -------------- ####
#### 0. preparation ####
#### -------------- ####

# 0.a. load packages, etc
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

# 0.b set up parallelization
### code to paralized the processing among multiple cores
cores <- ceiling(parallel::detectCores() -2) # use half of the available cores # Robbie: Should this be / 2?
myCluster <- snow::makeCluster(cores)
doSNOW::registerDoSNOW(myCluster)


#### ------------------- ####
####  1. general set up  ####
#### ------------------- ####

# 1.a get 
ev <- readr::read_csv(here::here('str_uncert_analysis', 
                          'data', 'external_validation', 'inputs', 
                          'ev_data_unassigned.csv'), col_types = 'nncncc') %>% 
  mutate(ddate_posict = parse_date_time(ddate, 'ymd HMS')) %>% 
  mutate(ev_yyyy = year(ddate_posict), 
         ev_dd = day(ddate_posict), 
         ev_mm = month((ddate_posict)))

# 1.g. get conus bounding box
# 1.g.i bring in conus shapefile
conus <- sf::st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                                'conus.shp')) %>% 
  sf::st_transform(., crs=st_crs('epsg:4326'))
# 1.g.ii get the bounding box 
bbox.conus <- list(xMin = sf::st_bbox(conus)$xmin[[1]], 
                   xMax = sf::st_bbox(conus)$xmax[[1]], 
                   yMin = sf::st_bbox(conus)$ymin[[1]], 
                   yMax = sf::st_bbox(conus)$ymax[[1]])

# 1.h. bring in daily merra caps 
activeCap <- paste0('cap_', yyyy)
me_caps <- readr::read_csv(here::here('ancillary_data', 'formatted', 'processing_support', 
                     'me_daily_caps.csv')) 
me_caps <- me_caps %>% 
  rename(cap := !!activeCap) %>% 
  arrange(as.numeric(lat)) %>%
  arrange(as.numeric(lon) )
rm(activeCap)

key.ev.av <- fst::read_fst(here::here('inputs', 'pm25', 'keys', 'key_nn_evDaily_avDaily.fst')) %>% 
  arrange(ref_lat) %>% 
  arrange(ref_lon)
if (yyyy %in% c(2005, 2006)) {
  key.ev.cm <- fst::read_fst(here::here('inputs', 'pm25', 'keys', 'key_nn_evDaily_cm05Daily.fst'))
} else if (yyyy == 2007) {
  key.ev.cm <- fst::read_fst(here::here('inputs', 'pm25', 'keys', 'key_nn_evDaily_cm07Daily.fst'))
} else if (yyyy == 2008) {
  key.ev.cm <- fst::read_fst(here::here('inputs', 'pm25', 'keys', 'key_nn_evDaily_cm08Daily.fst'))
} else if (yyyy == 2009) {
  key.ev.cm <- fst::read_fst(here::here('inputs', 'pm25', 'keys', 'key_nn_evDaily_cm09Daily.fst'))
} else if (yyyy %in% c(2010, 2011, 2012)) {
  key.ev.cm <- fst::read_fst(here::here('inputs', 'pm25', 'keys', 'key_nn_evDaily_cm10Daily.fst'))
} else if (yyyy %in% c(2013, 2014)) {
  key.ev.cm <- fst::read_fst(here::here('inputs', 'pm25', 'keys', 'key_nn_evDaily_cm13Daily.fst'))
} else if (yyyy %in% c(2015, 2016)) {
  key.ev.cm <- fst::read_fst(here::here('inputs', 'pm25', 'keys', 'key_nn_evDaily_cm15Daily.fst'))
}

key.ev.cm <- key.ev.cm %>% 
  arrange(ref_lat) %>% 
  arrange(ref_lon)
key.ev.js <- fst::read_fst(here::here('inputs', 'pm25', 'keys', 'key_nn_evDaily_jsDaily.fst'))%>% 
  arrange(ref_lat) %>% 
  arrange(ref_lon)
key.ev.me <- fst::read_fst(here::here('inputs', 'pm25', 'keys', 'key_nn_evDaily_meDaily.fst')) %>% 
  arrange(ref_lat) %>% 
  arrange(ref_lon)
key.ev.rk <- fst::read_fst(here::here('inputs', 'pm25', 'keys', 'key_nn_evDaily_rkDaily.fst')) %>% 
  arrange(ref_lat) %>% 
  arrange(ref_lon)

#### ------------------------ ####
####  2. wrangle base models  ####
#### ------------------------ ####

# 2.a. set up cmaq
# 2.a.i. read in 2010 cmaq
cm <- readr::read_csv(here::here('inputs', 'pm25', 'base_models', 'daily', 'raw', 'cmout', 
                          paste0(yyyy, '_pm25_daily_average.txt')) )
# 2.a.ii. clean up cmaq's names to conform to our expectations
if (yyyy < 2015) {
  cm <- cm %>% 
  rename(lat = Latitude, lon = Longitude, 
         pred_cm = 'pm25_daily_average(ug/m3)')  
} else if (yyyy >= 2015) {
  cm <- cm %>% 
    rename(lat = Latitude, lon = Longitude, 
           pred_cm = Prediction)  
} 

# 2.a.iii. including the date variable
if (mode(cm$Date) == 'date'  ) {
  cm <- cm %>% 
    mutate(Date = paste0(str_sub(Date, 6, 10), '-', str_sub(Date, 0, 4)))
}
if (yyyy %in% c(2014, 2013, 2012, 2011, 2010, 2009, 2008, 2007, 2006, 2005, 
                2004)) {
  cm <- cm %>% 
    mutate(ddate_diff = parse_date_time(Date, 'ymd') - parse_date_time(paste(yyyy, '-01-01'), 'ymd')) 
} else {
  cm <- cm %>% 
    mutate(ddate_diff = parse_date_time(Date, 'mdy') - parse_date_time(paste(yyyy, '-01-01'), 'ymd')) 
}

cm <- cm %>% 
  mutate(day_index = 1 + as.numeric(ddate_diff / (24*60*60))) 
# 2.a.iv keep only variables of interest 
cm <- cm %>% 
  dplyr::select(pred_cm, lat, lon, day_index) %>% 
  mutate(pred_cm = as.numeric(pred_cm), lat = as.numeric(lat))

# 2.b set up merra path 
path.merra <- here::here('inputs', 'pm25', 'base_models', 'daily', 'formatted', 'me')

#### --------------------------------------------- ####
####  3. Make Training and Prediction Data by Day  ####
#### --------------------------------------------- ####

# 3.a. set up loop


maxDoY <- 365 
if (yyyy %in% c(2004, 2008, 2012, 2016)) {maxDoY <- 366}
  


Sys.time()
ev.yyyy <- foreach(
  dayOfYear = 1:maxDoY, 
  .combine = 'rbind'
) %do% {
  
  #### ---------------------------- ####
  ####  3A. process date variables  ####
  #### ---------------------------- ####
  
  # 3A.a determine the date 
  activeDate <- lubridate::parse_date_time(paste0('01/01/', yyyy), 'dmy') + 
    lubridate::as.duration((dayOfYear-1)*60*60*24)
  
  # 3A.b. extract date components
  yyyy <- lubridate::year(activeDate)
  mm <- pad0(lubridate::month(activeDate))
  dd <- pad0(lubridate::day(activeDate))
  
  # 3A.c. isolate training to day of interest 
  ev.day <- ev %>% 
    dplyr::filter(as.numeric(ev_yyyy) == yyyy & 
                    pad0(ev_mm) == pad0(mm) & 
                    pad0(ev_dd) == pad0(dd))
  
  ev.day.sf <- ev.day %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326"))
  
  # get keys for just the locations measured for that day
  key.ev.av.day <- ev.day %>% 
    dplyr::select(lat, lon) %>%
    inner_join(key.ev.av, c('lat' = 'ref_lat', 'lon' = 'ref_lon'))
  key.ev.cm.day <- ev.day %>% 
    dplyr::select(lat, lon) %>%
    inner_join(key.ev.cm, c('lat' = 'ref_lat', 'lon' = 'ref_lon'))
  key.ev.js.day <- ev.day %>% 
    dplyr::select(lat, lon) %>%
    inner_join(key.ev.js, c('lat' = 'ref_lat', 'lon' = 'ref_lon'))
  key.ev.me.day <- ev.day %>% 
    dplyr::select(lat, lon) %>%
    inner_join(key.ev.me, c('lat' = 'ref_lat', 'lon' = 'ref_lon'))
  key.ev.rk.day <- ev.day %>% 
    dplyr::select(lat, lon) %>%
    inner_join(key.ev.rk, c('lat' = 'ref_lat', 'lon' = 'ref_lon'))
  
  #### ------------ ####
  ####  3B. add av  ####
  #### ------------ ####
  
  # 3B.a. readin av
  av <- raster::raster(here::here('inputs', 'pm25', 'base_models', 'monthly', 'raw', 'av', 
                          paste0('V5GL02.HybridPM25.NorthAmerica.', yyyy, mm, 
                                 '-', yyyy, mm, '.nc')))
  
  
  # 3B.b. crop to be within CONUS
  av.conus <- raster::crop(av, 
                           raster::extent(bbox.conus$xMin, 
                                          bbox.conus$xMax,
                                          bbox.conus$yMin, 
                                          bbox.conus$yMax))
  # 3B.c. put in tidy dataframe
  av.conus.coords <- raster::coordinates(av.conus)
  av <- data.frame(lon = av.conus.coords[,"x"], 
                   lat = av.conus.coords[,"y"], 
                   pred_av = raster::extract(av.conus, av.conus.coords)) %>% 
    na.omit()
  ev.day <- av %>% 
    dplyr::select(pred_av) %>%
    dplyr::slice(key.ev.av.day$baseModel_id)%>% 
    bind_cols(ev.day)
    
  #### ------------ ####
  ####  3C. add cb  ####
  #### ------------ ####
  
  #### ------------ ####
  ####  3D. add cm  ####
  #### ------------ ####
  
  # 3D.a. filter cmaq predictions to just the active day of interest
  cm.day <- cm %>% 
    dplyr::filter(day_index == dayOfYear)
  
 # assign
  ev.day <- cm.day %>% 
    dplyr::select(pred_cm) %>%
    dplyr::slice(key.ev.cm.day$baseModel_id)%>% 
    bind_cols(ev.day)
  
  #### ------------ ####
  ####  3E. add js  ####
  #### ------------ ####
  
  infix <- paste0(yyyy, mm)

  
  # 3E.b bring in js 
  js.day <- tibble::tibble(pred_js = 
                         as.vector(readRDS(paste0('inputs/pm25/base_models/daily/raw/js/',
                                                  infix, '/PredictionStep2_PM25_USGrid_', 
                       yyyy, mm, dd, '_', yyyy, mm, dd, '.rds'))))
  
  # assign
  ev.day <- js.day %>% 
    dplyr::select(pred_js) %>%
    dplyr::slice(key.ev.cm.day$baseModel_id)%>% 
    bind_cols(ev.day)
  
  #### ------------ ####
  ####  3F. add me  ####
  #### ------------ ####
  
  # 3F.a. bring in the merra predictions for that day
  me.day <- readr::read_csv(paste0(path.merra, '/daily_me_', yyyy, '_', 
                        dayOfYear,  '_blended.csv'))
  
  # assign
  ev.day <- me.day %>% 
    dplyr::select(pred_me) %>%
    dplyr::slice(key.ev.me.day$baseModel_id)%>% 
    bind_cols(ev.day)
  
  #### ------------ ####
  ####  3G. add rk  ####
  #### ------------ ####
  
  # 3G.a. bring in the rk raster
  rk <- ncdf4::nc_open(here::here('inputs', 'pm25', 'base_models', 'daily', 'raw', 'rk', 
                                      paste0('cmaqout_', yyyy, '-', mm, '-', dd, 
                                             '_pm25_daily_avg_lon_lat_UCAR_CONUS.nc')))
  
  # 3G.b. convert to dataframe
  rk.day <- data.frame(
    pred_rk = as.vector(ncdf4::ncvar_get(rk, 'pm25_daily_avg')), 
    lat = as.vector(ncdf4::ncvar_get(rk, 'latitude')), 
    lon = as.vector(ncdf4::ncvar_get(rk, 'longitude')))
    
    
  # assign
  ev.day <- rk.day %>% 
    dplyr::select(pred_rk) %>%
    dplyr::slice(key.ev.rk.day$baseModel_id)%>% 
    bind_cols(ev.day)
  
  #### ------------------- ####
  ####  3H. save datasets  ####
  #### ------------------- ####
  
    # 3H.a. compute two date variables
  ev.day <- ev.day %>% 
      mutate(day_of_year =  dayOfYear/maxDoY,
        julian_day =  as.numeric(activeDate - lubridate::parse_date_time('01/01/2005', 'dmy') )) 
   
    # 3H.b. arrange columns 
  ev.day <- ev.day %>% 
      #rename(lat = ref_lat, lon = ref_lon) %>%
      dplyr::select(lat, lon, day_of_year, julian_day, obs,
                    pred_av, pred_cm, pred_js, pred_me, pred_rk)
    
  # 3H.c. save it! 
  ev.day %>% 
    readr::write_csv(here::here('external_validation_data', 'pm25', 'daily', 
                                'processed_individual', 
                         paste0('ev_', yyyy, '_', stringr::str_pad(dayOfYear, 3, 'left', '0'), '.csv')))
  
  # 3H.g return training data 
  ev.day
}


#### ---------------- ####
####  4. final stuff  ####
#### ---------------- ####

# 4.a. save training dataset
ev.yyyy %>% 
  readr::write_csv(here::here('external_validation_data', 'pm25', 'daily', 
                              'processed_combined', 
                              paste0('ev_', yyyy, '_', stringr::str_pad(dayOfYear, 3, 'left', '0'), '.csv')))


# 4.b. close out cluster 

