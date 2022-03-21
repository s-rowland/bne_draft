# File: STR_d_01b_make_keys_annual_base_models.R
# Author: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 03/17/2022
#
# Contents:
#  N. notes
#  0. import packages and set global objects
#  1. process annual av
#  2. process annual cb
#  3. process annual cc
#  4. process annual cm
#  5. process annual gs
#  4. process annual js
#  5. process annual me
#  6. process annual rk
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
####  1. process monthly av  ####
#### ----------------------- ####

# 1.a. read in the raster for one month
av <- fst::read_fst(here::here('inputs', 'pm25', 'base_models', 'annual',
                                    'formatted', 'av', 
                                    'av_annual_2010.fst'))

#### ---------------------- ####
####  3. process annual cc  ####
#### ---------------------- ####

# 3.a. bring in cc
cc <- read_csv(here::here('inputs', 'pm25', 'base_models', 'annual', 'raw', 'cc', 
                                   'CACES_annual_2010_blockGrp_raw.csv')) 
               
#### ---------------------- ####
####  4. process annual cm  ####
#### ---------------------- ####

# 4.a. readin in cmaq data
cm <- fst::read_fst(here::here('inputs', 'pm25', 'base_models', 'annual',
                               'formatted', 'cmout', 
                          'cmout_2010_formatted.fst')) 

#### ---------------------- ####
####  5. process annual gs  ####
#### ---------------------- ####

gs <- loadData(path = here::here('inputs', 'pm25', 'base_models', 'annual', 'raw', 'gs',
                          'GBD2016_PREDPOP_FINAL.RData'), 
               dataset = 'GS_annual') %>% 
  filter(year == 2010)

#### ---------------------- ####
####  6. process annual js  ####
#### ---------------------- ####

# 6.a read in annual js - here we are using the geotiff files
js <- readRDS(here::here('inputs', 'pm25', 'base_models', 'annual', 'raw', 'js', 
                                      'USGridSite.rds')) %>% 
  rename(lat = Lat, lon = Lon)

#### ---------------------- ####
####  7. process annual me  ####
#### ---------------------- ####

# 7.a. bring in the merra predictions for one day
me.ras <- raster(here::here('inputs','pm25',  'base_models', 'annual', 'raw', 'me', 
                           '2010adjPM25_v2.nc'), band = 1) %>% 
  projectRaster(crs ='+init=epsg:4326')

# 7.b. crop to be within CONUS
me.ras <- raster::crop(me.ras, 
                      raster::extent(bbox.conus$xMin, 
                                     bbox.conus$xMax,
                                     bbox.conus$yMin, 
                                     bbox.conus$yMax))
# 7.c. put in tidy dataframe
me.coords <- raster::coordinates(me.ras)
me.pm <- raster::extract(me.ras, me.coords)
me <- rbind(tibble::tibble(lon = me.coords[,"x"], 
                              lat = me.coords[,"y"], 
                              pred = me.pm)) 

#### ---------------------- ####
####  8. process annual rk  ####
#### ---------------------- ####

# 8.a. read annual rk, which we have already processed 
rk <- fst::read_fst(here::here('inputs', 'pm25', 'base_models', 'annual',
                               'formatted', 'rk',
                    'rk_annual_2010.fst'))

#### ------------------------ ####
####  9. create keys for AQS  ####
#### ------------------------ ####

# 9.a. clean environment 
rm(me.ras, me.coords)

# since there are repeated locations for AQS, this key is used to identify 
# relevant base model locations, not all locations. 
# this allows us to effectively filter the annual base models before doing the 
# spatial join 

# 9.b. read annual AQS
aqs.annual <- read_fst(here::here('inputs', 'pm25', 'ground_truth', 'formatted', 
                                 'aqs_annual_curated.fst'))

# 9.c. reduce to just unique locations
aqs.loc.annual <- aqs.annual %>% 
  dplyr::select(lat, lon, ref_id) %>% 
  distinct()

# 9.d create list of base models and their names 
baseModels <- list(av, cc, cm, gs, js, me, rk)
baseModelNames <- c('avAnnual','ccAnnual', 'cmAnnual','gsAnnual', 'jsAnnual', 
                    'meAnnual', 'rkAnnual')

# 7.e create keys for aqs 
for (i in 1:length(baseModels)){
  createKey(ref.df = aqs.loc.annual, refName = 'aqsAnnual', 
           baseModel.df = baseModels[[i]], baseModelName = baseModelNames[i]) 
}

#### ---------------------------- ####
####  8. create keys for refGrid  ####
#### ---------------------------- ####

# 8.a. bring in the refGrids 
refGridConus <- fst::read_fst(here::here('inputs', 'pm25', 'reference_grids',  
                                         paste0('refGrid_', 'conus', '.fst')))
refGridNYS <- fst::read_fst(here::here('inputs', 'pm25', 'reference_grids',   
                                       paste0('refGrid_', 'NYS', '.fst')))
refGridCities <- fst::read_fst(here::here('inputs', 'pm25', 'reference_grids',  
                                          paste0('refGrid_', 'Cities', '.fst')))
# make list 
refGrids <- list(refGridConus, refGridNYS, refGridCities)
refGridNames <- c('refGridConus', 'refGridNYS', 'refGridCities')

# 8.b. make the keys 
for (i in 2:length(baseModels)) {
  #for(j in 1:length(refGrids)) {
  j <- 1
    createKey(ref.df = refGrids[[j]], refName = refGridNames[j], 
              baseModel.df = baseModels[[i]], baseModelName = baseModelNames[i])
  
  #}
}
