# Robbie: A lot of this code seems to repeat. Maybe too much work but in general perhaps more sources/functions could help?
# Robbie: Would potentially prevent making errors from copying and pasting
# Sebastian: Totally agree. Lawrence had set up a much nicer function, 
# Sebastian: I moved away from his function so I could add this key-based 
# approach, which was much faster. But then I never went back and integrated the two. 

# File: STR_d_01b_make_training_prediction_datasets_daily_follower.R
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

# Robbie: to fill in even if briefly
# Sebastian: added in, thanks. 

# This code assigns each centroid of the reference grid, and each training point 
# predictions from the nearest grid centroid, for each prediction model.

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
cores <- ceiling(parallel::detectCores() -2) # use half of the available cores
myCluster <- snow::makeCluster(cores)
doSNOW::registerDoSNOW(myCluster)


#### ------------------- ####
####  1. general set up  ####
#### ------------------- ####

# 1.a. declare the Area of Interest (AOI) 

# Robbie: AOI is 'Area of interest'?
# Sebastian: Correct, I added a comment
AOI <- 'conus'

# 1.b. bring in aqs data
training <- fst::read_fst(here::here('inputs', 'pm25', 'ground_truth', 'formatted',
                                'aqs_daily_curated.fst')) %>% 
  mutate(ddate = parse_date_time(date_local, 'ymd')) %>% 
  mutate(year = as.character(year(ddate)), month = pad0(month(ddate)), day = pad0(day(ddate)))

# 1.d. read in keys
# 1.d.i. AQS keys
key.aqs.av <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                  'key_nn_aqsDaily_avDaily.fst'))
# add cb
if (yyyy == 2005 | yyyy == 2006) {
  key.aqs.cm <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                    'key_nn_aqsDaily_cm05Daily.fst'))
} else if (yyyy == 2007) {
  key.aqs.cm <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                    'key_nn_aqsDaily_cm07Daily.fst'))
} else if (yyyy == 2008) {
  key.aqs.cm <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                    'key_nn_aqsDaily_cm08Daily.fst'))
} else if (yyyy == 2009) {
  key.aqs.cm <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                    'key_nn_aqsDaily_cm09Daily.fst'))
} else if (yyyy %in% c(2010, 2011, 2012, 2014) ) {
  key.aqs.cm <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                    'key_nn_aqsDaily_cm10Daily.fst'))
} else if (yyyy == 2013) {
  key.aqs.cm <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                    'key_nn_aqsDaily_cm13Daily.fst'))
} else if (yyyy >= 2015) {
  key.aqs.cm <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                    'key_nn_aqsDaily_cm15Daily.fst'))
}

key.aqs.js <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                  'key_nn_aqsDaily_jsDaily.fst'))
key.aqs.me <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                  'key_nn_aqsDaily_meDaily.fst'))
key.aqs.rk <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                  'key_nn_aqsDaily_rkDaily.fst'))

# 1.d.ii refGridConus keys
# we need to arrange to make sure every key is in the same order
key.refGridConus.av <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                           'key_nn_refGridConus_avDaily.fst')) %>% 
  arrange(ref_lat) %>% 
  arrange(ref_lon)

if (yyyy ==2005 | yyyy == 2006) {
  key.refGridConus.cm <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                    'key_nn_refGridConus_cm05Daily.fst')) %>% 
    arrange(ref_lat) %>% 
    arrange(ref_lon)
} else if (yyyy == 2007) {
  key.refGridConus.cm <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                             'key_nn_refGridConus_cm07Daily.fst')) %>% 
    arrange(ref_lat) %>% 
    arrange(ref_lon)
} else if (yyyy == 2008) {
  key.refGridConus.cm <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                             'key_nn_refGridConus_cm08Daily.fst')) %>% 
    arrange(ref_lat) %>% 
    arrange(ref_lon)
} else if (yyyy == 2009) {
  key.refGridConus.cm <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                             'key_nn_refGridConus_cm09Daily.fst')) %>% 
    arrange(ref_lat) %>% 
    arrange(ref_lon)
} else if (yyyy %in% c(2010, 2011, 2012, 2014) ) {
  key.refGridConus.cm <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                             'key_nn_refGridConus_cm10Daily.fst')) %>% 
    arrange(ref_lat) %>% 
    arrange(ref_lon)
} else if (yyyy == 2013) {
  key.refGridConus.cm <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                             'key_nn_refGridConus_cm13Daily.fst')) %>% 
    arrange(ref_lat) %>% 
    arrange(ref_lon)
} else if (yyyy >= 2015) {
  key.refGridConus.cm <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                             'key_nn_refGridConus_cm15Daily.fst')) %>% 
    arrange(ref_lat) %>% 
    arrange(ref_lon)
}
key.refGridConus.js <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                           'key_nn_refGridConus_jsDaily.fst')) %>% 
  arrange(ref_lat) %>% 
  arrange(ref_lon)
key.refGridConus.me <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                           'key_nn_refGridConus_meDaily.fst')) %>% 
  arrange(ref_lat) %>% 
  arrange(ref_lon)
key.refGridConus.rk <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                           'key_nn_refGridConus_rkDaily.fst')) %>% 
  arrange(ref_lat) %>% 
  arrange(ref_lon)

# 1.e generate refgrid 
preds <- key.refGridConus.js %>% 
  dplyr::select(ref_lat, ref_lon) %>% 
  arrange(ref_lat) %>% 
  arrange(ref_lon)


# Robbie: there is no progress bar here... I think at least! Tidy up?
# Sebastian: Agreed. Removed the line and renumbered.

# 1.f. get conus bounding box
# 1.f.i bring in conus shapefile
conus <- sf::st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                                'conus.shp')) %>% 
  sf::st_transform(., crs=st_crs('epsg:4326'))
# 1.f.ii get the bounding box 
bbox.conus <- list(xMin = sf::st_bbox(conus)$xmin[[1]], 
                   xMax = sf::st_bbox(conus)$xmax[[1]], 
                   yMin = sf::st_bbox(conus)$ymin[[1]], 
                   yMax = sf::st_bbox(conus)$ymax[[1]])

# 1.g. bring in daily merra caps 
activeCap <- paste0('cap_', yyyy)
me_caps <- read_csv(here::here('ancillary_data', 'formatted', 'processing_support', 
                     'me_daily_caps.csv')) 
me_caps <- me_caps %>% 
  rename(cap := !!activeCap) %>% 
  arrange(as.numeric(lat)) %>%
  arrange(as.numeric(lon) )
rm(activeCap)

#me_caps <- me_caps %>%
#  sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326"))%>% 
#  sf::st_transform(crs=sf::st_crs(projCRS))

#me_caps2 <- me_caps %>% filter(is.na(cap)) %>% mutate(region = 5) %>%dplyr::select(region)
#plot(me_caps2)


#### ------------------------ ####
####  2. wrangle base models  ####
#### ------------------------ ####

# 2.a. set up cmaq
# 2.a.i. read in 2010 cmaq
cm <- read_csv(here::here('inputs', 'pm25', 'base_models', 'daily', 'raw', 'cmout', 
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
if(yyyy %in% c(2008,2012)) {
  cm <- cm %>% 
    mutate(Date = paste0(str_sub(Date, 6, 10), '-', str_sub(Date, 0, 4)))
}

cm <- cm %>% 
  mutate(ddate_diff = parse_date_time(Date, 'mdy') - parse_date_time(paste(yyyy, '-01-01'), 'ymd')) %>% 
  mutate(day_index = 1 + as.numeric(ddate_diff / (24*60*60))) 
# 2.a.iv keep only variables of interest 
cm <- cm %>% 
  dplyr::select(pred_cm, lat, lon, day_index) %>% 
  mutate(pred_cm = as.numeric(pred_cm), lat = as.numeric(lat))

# 2.b. declare the path where merra data is locations 
# all days are in one nc file
path.merra <- here::here('inputs','pm25',  'base_models', 'daily', 'raw', 'me', 
                        paste0('daily', yyyy, 'adjPM25sum_v2.nc'))


# 0.c. strings for js download 
prefix <- "https://beta.sedac.ciesin.columbia.edu/downloads/data/aqdh/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016-"
suffix <- "-rds.zip"
wd <- paste0(here::here('inputs', 'pm25', 'base_models', 'daily', 'raw', 'js'), '/')

# 2.c download the relevant js 
for (mm in pad0(1:12)) {
  infix <- paste0(yyyy, mm)
  list.files(wd)
  # 3E.a. download js if needed
if  ( !(infix %in% list.files(wd))) {
    
    # 3E.a.i. name of monthly file
    url <- paste0(prefix, infix, suffix)
    zipfile <- paste0(wd, infix, ".zip")
    download <- paste0("curl -o ", zipfile, " -b ~/.urs_cookies -c ~/.urs_cookies -L -n ", url)
    # 3E.a.ii. do the download
    system(download)
    # 3E.a.iii unzip the file
    unzip(zipfile, exdir = paste0(wd, infix))
  }
}



#### --------------------------------------------- ####
####  3. Make Training and Prediction Data by Day  ####
#### --------------------------------------------- ####

# 3.a. set up loop
maxDoY <- 365 
if (yyyy %in% c(2004, 2008, 2012, 2016)) {maxDoY <- 366}
  
# 0.c progress bar
pb <- txtProgressBar(min = 0, max = 10, width = 25, style = 3)
progress <- function(p) setTxtProgressBar(pb, p)
opts <- list(progress=progress)

Sys.time()
training.full <- foreach(
  dayOfYear = 1:maxDoY, 
  .combine = 'rbind',
  .options.snow = opts, 
  .inorder = FALSE,
  .export = ls(globalenv()),
  .packages = c('dplyr', 'magrittr')
  
) %dopar% {
  
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
  training.day <- training %>% 
    dplyr::filter(as.numeric(year) == yyyy & 
                    as.character(month) == pad0(mm) & 
                    as.character(day) == pad0(dd))
  
  # 3A.d make day version of preds dataset 
  preds.day <- preds
  
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
  
  # 3B.d. assign to training data 
  # 3B.d.i. attach key to the training data 
  training.day <- training.day %>% 
    dplyr::inner_join(key.aqs.av, by = 'ref_id')
  # 3B.d.ii attach the correct predictions 
  training.day <- av %>% 
    dplyr::slice(training.day$baseModel_id) %>% 
    dplyr::select(pred_av) %>% 
    dplyr::bind_cols(training.day) %>% 
    dplyr::select(-ref_lat, -ref_lon, -starts_with('baseModel_'))
  
  # 3B.e assign to prediction data 
  # remember the key is already in order wrt the prediction dataset 
  # because of the arranging we did earlier. 
  preds.day <- av %>% 
    dplyr::slice(key.refGridConus.av$baseModel_id) %>% 
    dplyr::select(pred_av) %>%
    dplyr::bind_cols(preds.day)
  
  print(paste0('completed av for ', dayOfYear))
  #### ------------ ####
  ####  3C. add cb  ####
  #### ------------ ####
  
  #### ------------ ####
  ####  3D. add cm  ####
  #### ------------ ####
  
  # activate the correct cmaq keys 

  
  # 3D.a. filter cmaq predictions to just the active day of interest
  cm.day <- cm %>% 
    dplyr::filter(day_index == dayOfYear)
  
  # 3D.b. assign to training data 
  # 3D.b.i. attach key to the training data 
  training.day <- training.day %>% 
    dplyr::inner_join(key.aqs.cm, by = 'ref_id')
  # 3D.b.ii attach the correct predictions 
  training.day <- cm.day %>% 
    dplyr::slice(training.day$baseModel_id) %>% 
    dplyr::select(pred_cm) %>% 
    dplyr::bind_cols(training.day) %>% 
    dplyr::select(-ref_lat, -ref_lon, -starts_with('baseModel_'))
  
  # 3D.c assign to prediction data 
  # remember the key is already in order wrt the prediction dataset 
  # because of the arranging we did earlier. 
  preds.day <- cm.day %>% 
    dplyr::slice(key.refGridConus.cm$baseModel_id) %>% 
    dplyr::select(pred_cm) %>%
    dplyr::bind_cols(preds.day)
  
  print(paste0('completed cm for ', dayOfYear))
  
  #### ------------ ####
  ####  3E. add js  ####
  #### ------------ ####
  
  infix <- paste0(yyyy, mm)

  
  # 3E.b bring in js 
  js <- tibble::tibble(pred_js = 
                         as.vector(readRDS(paste0(wd, infix, '/PredictionStep2_PM25_USGrid_', 
                       yyyy, mm, dd, '_', yyyy, mm, dd, '.rds'))))
  
  # 3E.c. assign to training data 
  # 3E.c.i. attach key to the training data 
  training.day <- training.day %>% 
    dplyr::inner_join(key.aqs.js, by = 'ref_id')
  # 3E.c.ii attach the correct predictions 
  training.day <- js %>% 
    dplyr::slice(training.day$baseModel_id) %>% 
    dplyr::select(pred_js) %>% 
    dplyr::bind_cols(training.day) %>% 
    dplyr::select(-ref_lat, -ref_lon, -starts_with('baseModel_'))
  
  # 3E.d assign to prediction data 
  # remember the key is already in order wrt the prediction dataset 
  # because of the arranging we did earlier. 
  preds.day <- js %>% 
    dplyr::slice(key.refGridConus.js$baseModel_id) %>% 
    dplyr::select(pred_js) %>%
    dplyr::bind_cols(preds.day)
  
  print(paste0('completed js for ', dayOfYear))
  
  #### ------------ ####
  ####  3F. add me  ####
  #### ------------ ####
  
  # 3F.a. bring in the merra predictions for that day
  me <- raster::raster(path.merra, band = as.numeric(dayOfYear))# %>%
  #projectRaster(crs ='+init=epsg:4326')
  
  # 3F.b crop to be within CONUS
  me.conus <- raster::crop(me, 
                           raster::extent(bbox.conus$xMin, 
                                          bbox.conus$xMax,
                                          bbox.conus$yMin, 
                                          bbox.conus$yMax))
  # 3F.c put in tidy dataframe
  me.conus.coords <- raster::coordinates(me.conus)
  me <- rbind(tibble::tibble(lon = me.conus.coords[,"x"], 
                             lat = me.conus.coords[,"y"], 
                             pred_me = raster::extract(me.conus, me.conus.coords))) %>% 
    na.omit()
  
  
  #me <- me %>% 
   # dplyr::arrange(as.numeric(desc(lat))) %>%
  #  dplyr::arrange(as.numeric(lon) )
  
  me <- me %>%
    dplyr::bind_cols(dplyr::select(me_caps, cap)) %>% 
    dplyr::mutate(pred_me = if_else(pred_me > cap, cap, pred_me)) %>% 
    dplyr::select(-cap)
  # 3F.f. assign to training data 
  # 3F.f.i. attach key to the training data 
  training.day <- training.day %>% 
    dplyr::inner_join(key.aqs.me, by = 'ref_id')
  # 3F.f.ii attach the correct predictions 
  training.day <- me %>% 
    dplyr::slice(training.day$baseModel_id) %>% 
    dplyr::select(pred_me) %>% 
    dplyr::bind_cols(training.day) %>% 
    dplyr::select(-ref_lat, -ref_lon, -starts_with('baseModel_'))
  
  # 3F.g assign to prediction data 
  # remember the key is already in order wrt the prediction dataset 
  # because of the arranging we did earlier. 
  preds.day <- me %>% 
    dplyr::slice(key.refGridConus.me$baseModel_id) %>% 
    dplyr::select(pred_me) %>%
    dplyr::bind_cols(preds.day)
  
  print(paste0('completed me for ', dayOfYear))
  
  #### ------------ ####
  ####  3G. add rk  ####
  #### ------------ ####
  
  # 3G.a. bring in the rk raster
  rk <- ncdf4::nc_open(here::here('inputs', 'pm25', 'base_models', 'daily', 'raw', 'rk', 
                                      paste0('cmaqout_', yyyy, '-', mm, '-', dd, 
                                             '_pm25_daily_avg_lon_lat_UCAR_CONUS.nc')))
  
  # 3G.b. convert to dataframe
  rk <- data.frame(
    pred_rk = as.vector(ncdf4::ncvar_get(rk, 'pm25_daily_avg')), 
    lat = as.vector(ncdf4::ncvar_get(rk, 'latitude')), 
    lon = as.vector(ncdf4::ncvar_get(rk, 'longitude')))
    
    # 3G.c. close the netCDf object 
    #nc_close(rk)
    
    # 3G.d. keep only rk at the potenital AQS sites 
    rk.atAQS <- rk %>% 
      dplyr::filter(row_number() %in% key.aqs.rk$baseModel_id)
    
    # 3G.e. attach the key 
    training.day <- training.day %>% 
      dplyr::inner_join(key.aqs.rk, by = c('ref_id'))

    # 3G.f. attach rk values 
    training.day <- rk %>% 
      dplyr::slice(training.day$baseModel_id) %>% 
      dplyr::select(-lat, -lon) %>% 
      dplyr::bind_cols(training.day)
    
    # 3G.g. attach rk values to preds 
    preds.day <- rk %>% 
      dplyr::slice(key.refGridConus.rk$baseModel_id) %>% 
      dplyr::select(-lat, -lon) %>% 
      dplyr::bind_cols(preds.day)
        
    print(paste0('completed rk for ', dayOfYear))
    
  #### ------------------- ####
  ####  3H. save datasets  ####
  #### ------------------- ####
  
    # 3H.a. compute two date variables
    preds.day <- preds.day %>% 
      mutate(day_of_year =  dayOfYear/maxDoY,
        julian_day =  as.numeric(activeDate - lubridate::parse_date_time('01/01/2005', 'dmy') )) 
   
    # 3H.b. arrange columns 
    preds.day <- preds.day %>% 
      rename(lat = ref_lat, lon = ref_lon) %>%
      dplyr::select(lat, lon, day_of_year, julian_day, 
                    pred_av, pred_cm, pred_js, pred_me, pred_rk)
    
  # 3H.c. save it! 
  preds.day %>% 
    readr::write_csv(here::here('inputs', 'pm25', 'prediction_datasets', 
                         'daily_individual', 
                         paste0('preds_', yyyy, '_', stringr::str_pad(dayOfYear, 3, 'left', '0'), '.csv')))
  
  # 3H.d. compute two date variables
  training.day <- training.day %>%
    mutate(day_of_year = dayOfYear/maxDoY,
           julian_day =  as.numeric(activeDate - lubridate::parse_date_time('01/01/2005', 'dmy') )) 
  
  # 3H.e. arrange columns
  training.day <- training.day %>%
    dplyr::select(lat, lon, day_of_year, julian_day, obs,
                  pred_av, pred_cm, pred_js, pred_me, pred_rk, state, ref_id)

  # 3H.f report day is done 
  print(activeDate)
  
  # 3H.g return training data 
  training.day
}


#### ---------------- ####
####  4. final stuff  ####
#### ---------------- ####

# 4.a. save training dataset
training.full %>% 
  readr::write_csv(here::here('inputs', 'pm25', 'training_datasets', 'daily_yearly', 
                       paste0('training_', yyyy, '.csv')))

# 4.b. close out cluster 
close(pb)
#stopCluster(my.cluster)

# 4.c. remove js 
#for (mm in pad0(1:12)) {
#  infix <- paste0(yyyy, mm)
#  unlink(paste0(wd, infix), recursive = TRUE)
#  unlink(paste0(wd, infix, ".zip"))
#}
