# File: STR_d_01a_make_keys_daily_base_models.R
# Author: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 03/17/2022
#
# Contents:
#  N. notes
#  0. import packages and set global objects
#  1. process daily AV 
#  2. process daily CB
#  3. process daily CM
#  4. process daily JS
#  5. process monthly AME
#  6. process daily RK
#  7. create keys for AQS 
#  8. create keys for refGrid

#### ------------------- ####
####       N. notes      ####
#### ------------------- ####

# N.1. goal
# The point of this script is to create various "key" files:
# 
# The keys map EPA monitor locations to
# their nearest neighbour JS <lat, lon> coordinate pairs, along with 
# recording the index of the JS lat-lon pair. In this way, a "key" is made, allowing
# someone to extract only the points of interest from a JS .rda file using the
# "js_index" column from the key. 
# the keys are stored in the inputs/[pollutant]/keys folder, and follow this naming convention: 
# [aqs/refGrid] _JS_key_nn_ [timeWindow]

#### ------------------------------------------- ####
####  0. IMPORT PACKAGES AND SET GLOBAL OBJECTS  ####
#### ------------------------------------------- ####

# 0.a. import packages and global objects, if you haven't already done so
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

# 0.b. get conus bounding box
# 0.b.i bring in conus shapefile
conus <- sf::st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                                'conus.shp')) %>% 
  sf::st_transform(., crs=st_crs('epsg:4326'))
# 0.b.ii get the bounding box 
bbox.conus <- list(xMin = sf::st_bbox(conus)$xmin[[1]], 
                   xMax = sf::st_bbox(conus)$xmax[[1]], 
                   yMin = sf::st_bbox(conus)$ymin[[1]], 
                   yMax = sf::st_bbox(conus)$ymax[[1]])

#### ----------------------- ####
####  1. process monthly AV  ####
#### ----------------------- ####

# 1.a. read in the raster for one month
av.ras <- raster::raster(here::here('inputs', 'pm25', 'base_models', 'monthly', 'raw', 'av', 
                                    'V5GL02.HybridPM25.NorthAmerica.201001-201001.nc'))

# 1.b. crop to conus
av.ras <- raster::crop(av.ras, 
                       raster::extent(bbox.conus$xMin,  bbox.conus$xMax,
                                      bbox.conus$yMin, bbox.conus$yMax))
# 1.c. put in tidy dataframe
av.coords <- raster::coordinates(av.ras)
av.pm <- raster::extract(av.ras, av.coords)
av <- rbind(tibble::tibble(lon = av.coords[,"x"], 
                           lat = av.coords[,"y"], 
                           pred_av = av.pm)) %>% 
  na.omit()

#### --------------------- ####
####  3. process daily CM  ####
#### --------------------- ####

# 3.a. readin in cmaq data
cm00 <- read_csv(here::here('inputs', 'pm25', 'base_models', 'daily','raw', 'cmout', 
                            '2000_pm25_daily_average.txt')) 
# 3.b. rename columns and get the locations mfor just one day
cm00 <- cm00 %>% 
  rename(lat = Latitude, lon = Longitude, 
         pred_cm = 'pm25_daily_average(ug/m3)')  %>% 
  filter(as.character(Date) == '2000-01-01') %>% 
  mutate(lat = as.numeric(lat)) %>%
  mutate(id = row_number()) %>% 
  dplyr::select(lat, lon, id)

# 3.a. readin in cmaq data
cm01 <- read_csv(here::here('inputs', 'pm25', 'base_models', 'daily','raw', 'cmout', 
                            '2001_pm25_daily_average.txt')) 
# 3.b. rename columns and get the locations mfor just one day
cm01 <- cm01 %>% 
  rename(lat = Latitude, lon = Longitude, 
         pred_cm = 'pm25_daily_average(ug/m3)')  %>% 
  filter(as.character(Date) == '2001-01-01') %>% 
  mutate(lat = as.numeric(lat)) %>%
  mutate(id = row_number()) %>% 
  dplyr::select(lat, lon, id)

# 3.a. readin in cmaq data
cm02 <- read_csv(here::here('inputs', 'pm25', 'base_models', 'daily','raw', 'cmout', 
                            '2002_pm25_daily_average.txt')) 
# 3.b. rename columns and get the locations mfor just one day
cm02 <- cm02 %>% 
  rename(lat = Latitude, lon = Longitude, 
         pred_cm = 'pm25_daily_average(ug/m3)')  %>% 
  filter(as.character(Date) == '2002-01-01') %>% 
  mutate(lat = as.numeric(lat)) %>%
  mutate(id = row_number()) %>% 
  dplyr::select(lat, lon, id)

# 3.a. readin in cmaq data
cm03 <- read_csv(here::here('inputs', 'pm25', 'base_models', 'daily','raw', 'cmout', 
                            '2003_pm25_daily_average.txt')) 
# 3.b. rename columns and get the locations mfor just one day
cm03 <- cm03 %>% 
  rename(lat = Latitude, lon = Longitude, 
         pred_cm = 'pm25_daily_average(ug/m3)')  %>% 
  filter(as.character(Date) == '2003-01-01') %>% 
  mutate(lat = as.numeric(lat)) %>%
  mutate(id = row_number()) %>% 
  dplyr::select(lat, lon, id)

# 3.a. readin in cmaq data
cm04 <- read_csv(here::here('inputs', 'pm25', 'base_models', 'daily','raw', 'cmout', 
                            '2004_pm25_daily_average.txt')) 
# 3.b. rename columns and get the locations mfor just one day
cm04 <- cm04 %>% 
  rename(lat = Latitude, lon = Longitude, 
         pred_cm = 'pm25_daily_average(ug/m3)')  %>% 
  filter(as.character(Date) == '2004-01-01') %>% 
  mutate(lat = as.numeric(lat)) %>%
  mutate(id = row_number()) %>% 
  dplyr::select(lat, lon, id)

# 3.a. readin in cmaq data
cm05 <- read_csv(here::here('inputs', 'pm25', 'base_models', 'daily','raw', 'cmout', 
                            '2005_pm25_daily_average.txt')) 
# 3.b. rename columns and get the locations mfor just one day
cm05 <- cm05 %>% 
  rename(lat = Latitude, lon = Longitude, 
         pred_cm = 'pm25_daily_average(ug/m3)')  %>% 
  filter(as.character(Date) == '2005-01-01') %>% 
  mutate(lat = as.numeric(lat)) %>%
  mutate(id = row_number()) %>% 
  dplyr::select(lat, lon, id)
# 3.a. readin in cmaq data
cm07 <- read_csv(here::here('inputs', 'pm25', 'base_models', 'daily','raw', 'cmout', 
                            '2007_pm25_daily_average.txt')) 
# 3.b. rename columns and get the locations mfor just one day
cm07 <- cm07 %>% 
  rename(lat = Latitude, lon = Longitude, 
         pred_cm = 'pm25_daily_average(ug/m3)')  %>% 
  filter(as.character(Date) == '2007-01-01') %>% 
  mutate(lat = as.numeric(lat)) %>%
  mutate(id = row_number()) %>% 
  dplyr::select(lat, lon, id)

# 3.a. readin in cmaq data
cm08 <- read_csv(here::here('inputs', 'pm25', 'base_models', 'daily','raw', 'cmout', 
                            '2008_pm25_daily_average.txt')) 
# 3.b. rename columns and get the locations mfor just one day
cm08 <- cm08 %>% 
  rename(lat = Latitude, lon = Longitude, 
         pred_cm = 'pm25_daily_average(ug/m3)')  %>% 
  filter(as.character(Date) == '2008-01-01') %>% 
  mutate(lat = as.numeric(lat)) %>%
  mutate(id = row_number()) %>% 
  dplyr::select(lat, lon, id)

# 3.a. readin in cmaq data
cm09 <- read_csv(here::here('inputs', 'pm25', 'base_models', 'daily','raw', 'cmout', 
                            '2009_pm25_daily_average.txt')) 
# 3.b. rename columns and get the locations mfor just one day
cm09 <- cm09 %>% 
  rename(lat = Latitude, lon = Longitude, 
         pred_cm = 'pm25_daily_average(ug/m3)')  %>% 
  filter(as.character(Date) == '2009-01-01') %>% 
  mutate(lat = as.numeric(lat)) %>%
  mutate(id = row_number()) %>% 
  dplyr::select(lat, lon, id)

# 3.a. readin in cmaq data
cm10 <- read_csv(here::here('inputs', 'pm25', 'base_models', 'daily','raw', 'cmout', 
                            '2010_pm25_daily_average.txt')) 
# 3.b. rename columns and get the locations mfor just one day
cm10 <- cm10 %>% 
  rename(lat = Latitude, lon = Longitude, 
         pred_cm = 'pm25_daily_average(ug/m3)')  %>% 
  filter(as.character(Date) == '2010-01-01') %>% 
  mutate(lat = as.numeric(lat)) %>%
  mutate(id = row_number()) %>% 
  dplyr::select(lat, lon, id)

# 3.a. readin in cmaq data
cm13 <- read_csv(here::here('inputs', 'pm25', 'base_models', 'daily','raw', 'cmout', 
                            '2013_pm25_daily_average.txt')) 
# 3.b. rename columns and get the locations mfor just one day
cm13 <- cm13 %>% 
  rename(lat = Latitude, lon = Longitude, 
         pred_cm = 'pm25_daily_average(ug/m3)')  %>% 
  filter(as.character(Date) == '2013-01-01') %>% 
  mutate(lat = as.numeric(lat)) %>%
  mutate(id = row_number()) %>% 
  dplyr::select(lat, lon, id)

# 3.a. readin in cmaq data
cm15 <- read_csv(here::here('inputs', 'pm25', 'base_models', 'daily','raw', 'cmout', 
                            '2015_pm25_daily_average.txt')) 
# 3.b. rename columns and get the locations mfor just one day
cm15 <- cm15 %>% 
  rename(lat = Latitude, lon = Longitude, 
         pred_cm = Prediction)  %>% 
  filter(as.character(Date) == 'Jan-01-2015') %>% 
  mutate(lat = as.numeric(lat)) %>%
  mutate(id = row_number()) %>% 
  dplyr::select(lat, lon, id)

#### --------------------- ####
####  4. process daily js  ####
#### --------------------- ####

# 4.a read in daily js - here we are using the geotiff files
js <- readRDS(here::here('inputs', 'pm25', 'base_models', 'daily', 'raw', 'js', 
                         'USGridSite.rds')) %>% 
  rename(lat = Lat, lon = Lon)

#### --------------------- ####
####  5. process daily ME  ####
#### --------------------- ####

# 5.a. bring in the merra predictions for one day
me.ras <- raster(here::here('inputs','pm25',  'base_models', 'daily', 'raw', 'me', 
                            'daily2010adjPM25sum_v2.nc'), band = 1) %>% 
  projectRaster(crs ='+init=epsg:4326')

# 5.b. crop to be within CONUS
me.ras <- raster::crop(me.ras, 
                       raster::extent(bbox.conus$xMin, 
                                      bbox.conus$xMax,
                                      bbox.conus$yMin, 
                                      bbox.conus$yMax))
# 5.c. put in tidy dataframe
me.coords <- raster::coordinates(me.ras)
me.pm <- raster::extract(me.ras, me.coords)
me <- rbind(tibble::tibble(lon = me.coords[,"x"], 
                           lat = me.coords[,"y"], 
                           pred = me.pm)) 

#### --------------------- ####
####  6. process daily RK  ####
#### --------------------- ####

# 6.a. read in the raster for one month
# note: we do not need to crop since this is a US-based model
rk.ras <- ncdf4::nc_open(here::here('inputs', 'pm25', 'base_models', 'daily', 'raw', 'rk', 
                                    'cmaqout_2015-01-01_pm25_daily_avg_lon_lat_UCAR_CONUS.nc'))

# 6.b. create datafram
rk <- data.frame(
  pred_rk = as.vector(ncdf4::ncvar_get(rk.ras, 'pm25_daily_avg')), 
  lat = as.vector(ncdf4::ncvar_get(rk.ras, 'latitude')), 
  lon = as.vector(ncdf4::ncvar_get(rk.ras, 'longitude')))

# check
#a <- rk.df %>%  st_as_sf(coords = c('lon', 'lat')) %>% slice_sample(prop = 0.25)
#ggplot(a) + geom_sf(aes(color = pred_rk, fill = pred_rk))

# 6.c close the netCDF object
nc_close(rk.ras)

#### ------------------------ ####
####  7. create keys for AQS  ####
#### ------------------------ ####

# 7.a. clean environment 
rm(av.ras, av.coords, av.pm, js.loc.daily.ras, me.ras)
rm(js.loc.daily, rk.ras)
rm(me.coords)
gc()

# since there are repeated locations for AQS, this key is used to identify 
# relevant base model locations, not all locations. 
# this allows us to effectively filter the daily base models before doing the 
# spatial join 

# 7.b. read daily AQS
ev.daily <- read_csv(here::here('str_uncert_analysis', 
                          'data', 'external_validation', 'inputs', 
                          'ev_data_unassigned.csv'), col_types = 'nncncc') 

# 7.c. reduce to just unique locations
ev.loc.daily <- ev.daily %>% 
  dplyr::select(lat, lon) %>% 
  distinct()

# 7.d create list of base models and their names 
baseModels <- list(av, cm00, cm01, cm02, cm03, cm04, 
                   cm05, cm07, cm08, cm09, cm10, cm13, cm15, js, me, rk)
baseModelNames <- c('avDaily', 'cm00Daily', 'cm01Daily', 'cm02Daily','cm03Daily', 'cm04Daily',
                    'cm05Daily','cm07Daily', 'cm08Daily', 'cm09Daily', 
                    'cm10Daily','cm13Daily',  'cm15Daily', 
                    'jsDaily', 'meDaily', 'rkDaily')

# 7.e create keys for aqs 
for (i in 1: length(baseModels)){
  createKey(ref.df = ev.loc.daily, refName = 'evDaily', 
            baseModel.df = baseModels[[i]], baseModelName = baseModelNames[i]) 
}

