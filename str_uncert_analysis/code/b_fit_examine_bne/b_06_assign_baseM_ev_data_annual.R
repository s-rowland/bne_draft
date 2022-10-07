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
#  3. make training and prediction data by day
#  3A. process date variables and aqs
#  3B. add av 
#  3C. add cb
#  3D. add cc
#  3E. add cm 
#  3F. add gs
#  3G. add js 
#  3H. add me
#  3I. add rk
#  3J. save datasets
#  4. save training dataset

#### ------------------ ####
####       N. notes     ####
#### ------------------ ####


#### -------------- ####
#### 0. preparation ####
#### -------------- ####

# 0.a. load packages, etc
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

#### ------------------- ####
####  1. general set up  ####
#### ------------------- ####

# 1.a brin in unassigned ev data
ev <- read_csv(here::here('str_uncert_analysis', 
                          'data', 'external_validation', 'inputs', 
                          'ev_data_unassigned_annual.csv'))

# 1.b find all unique ev locations
ev.loc <- ev %>% 
  dplyr::select(lat, lon) %>% 
  distinct()

# 1.c. get conus bounding box
# 1.c.i bring in conus shapefile
conus <- sf::st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                                'conus.shp')) %>% 
  sf::st_transform(., crs=st_crs('epsg:4326'))
# 1.c.ii get the bounding box 
bbox.conus <- list(xMin = sf::st_bbox(conus)$xmin[[1]], 
                   xMax = sf::st_bbox(conus)$xmax[[1]], 
                   yMin = sf::st_bbox(conus)$ymin[[1]], 
                   yMax = sf::st_bbox(conus)$ymax[[1]])

#### ------------------------ ####
####  2. wrangle base models  ####
#### ------------------------ ####

# 2.a. bring in gs
# the one gs file contains data for all years
gs.allYears <- loadData(path =  here::here('inputs', 'pm25', 'base_models', 'annual', 'raw', 
                                           'gs', 'GBD2016_PREDPOP_FINAL.RData'), 
                        dataset = 'GS_annual') %>% 
  rename(pred_gs = pred)

# 2.b js location 
js.loc <- readRDS(here::here('inputs', 'pm25', 'base_models', 'annual', 'raw',
                             'js', 'USGridSite.rds')) %>% 
  rename(lat = Lat, lon =Lon) %>% 
  dplyr::select(lat, lon)

# 2.b find av locations that are at all relevant 
# 2.b.i bring in one year of av 
av.loc <- fst::read_fst(here::here('inputs', 'pm25', 'base_models', 'annual', 'formatted', 'av', 
                               paste0('av_annual_', 2010, '.fst'))) %>% 
  dplyr::select(lat, lon)
  
# 2.b.ii find nearest av points for any EV location
av_any_index <- get.knnx(av.loc, ev.loc, k = 1)$nn.index
rm(av.loc)

# 2.c find js locations that are at all relevant 
# 2.c.i find nearest js points for any EV location
js_any_index <- get.knnx(js.loc, ev.loc, k = 1)$nn.index
# 2.c.ii keep only those potentially-relevant js location
js.loc <- js.loc %>% 
  slice(js_any_index)

#### --------------------------------------------- ####
####  3. make training and prediction data by day  ####
#### --------------------------------------------- ####

# 3.a. set up loop
#Sys.time()
ev.full <- foreach(
  yyyy = 2010:2015, 
  .combine = 'rbind'
) %do% {
  
  #### ---------------------------- ####
  ####  3A. process date variables  ####
  #### ---------------------------- ####
  
  # 3A.a. isolate training to year of interest 
  ev.year <- ev %>% 
    filter(yyyy == !!yyyy)
  
  # 3A.b simple features version
  ev.year.sf <- ev.year %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326")) %>% 
    st_transform(crs = projCRS)
  
  # 3A.c just the locations
  ev.year.loc <- ev.year %>% 
    dplyr::select(lat, lon)
  
  #### ------------ ####
  ####  3B. add av  ####
  #### ------------ ####
  
  # 3B.a. readin av
  av <- fst::read_fst(here::here('inputs', 'pm25', 'base_models', 'annual', 'formatted', 'av', 
                                 paste0('av_annual_', yyyy, '.fst'))) %>% 
    slice(av_any_index)
  
  # 3B.b convert to simple features 
  av.sf <- av %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326")) %>% 
    st_transform(crs = projCRS)
  
  # 3B.c identify nearest CC points 
  av_index <- nngeo::st_nn(ev.year.sf, av.sf, k=1) %>% unlist()
  
  # 3B.d assign cc 
  ev.year <- av %>% 
    dplyr::select(starts_with('pred_')) %>%
    slice(av_index) %>% 
    bind_cols(ev.year)

  #### ------------ ####
  ####  3C. add cb  ####
  #### ------------ ####
  
  #### ------------ ####
  ####  3C. add cc  ####
  #### ------------ ####
  
  # 3C.a. readin cc
  cc <- read_csv(here::here('inputs', 'pm25', 'base_models', 'annual', 'raw', 'cc', 
                            paste0('CACES_annual_', yyyy, '_blockgrp_raw.csv'))) %>% 
    rename(pred_cc = pred_wght)
 
 # 3C.b convert to simple features 
 cc.sf <- cc %>% 
   sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326")) %>% 
   st_transform(crs = projCRS)
  
 # 3C.c identify nearest CC points 
 cc_index <- nngeo::st_nn(ev.year.sf, cc.sf, k=1) %>% unlist()
 
 # 3C.d assign cc 
 ev.year <- cc %>% 
   dplyr::select(starts_with('pred_')) %>%
   slice(cc_index) %>% 
   bind_cols(ev.year)
  
  #### ------------ ####
  ####  3D. add cm  ####
  #### ------------ ####
  
  # 3D.a. get cmaq predictions for year of interest
  cm <- fst::read_fst(here::here('inputs', 'pm25', 'base_models', 'annual', 
                                 'formatted', 'cmout', 
                                 paste0('cmout_', yyyy, '_formatted.fst'))) %>% 
    mutate(pred_cm = pred)
  
 # 3D.b convert to simple features 
 cm.sf <- cm %>% 
   sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326")) %>% 
   st_transform(crs = projCRS)
 
 # 3D.c identify nearest CC points 
 cm_index <- nngeo::st_nn(ev.year.sf, cm.sf, k=1) %>% unlist()
 
 # 3D.d assign cc 
 ev.year <- cm %>% 
   dplyr::select(starts_with('pred_')) %>%
   slice(cm_index) %>% 
   bind_cols(ev.year)
 
  #### ------------ ####
  ####  3E. add gs  ####
  #### ------------ ####
  
  # 3E.a filter gs by year 
  gs <- gs.allYears %>% 
    filter(year == yyyy)
 
 # 3D.b convert to simple features 
 gs.sf <- gs %>% 
   sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326")) %>% 
   st_transform(crs = projCRS)
 
 # 3D.c identify nearest CC points 
 gs_index <- nngeo::st_nn(ev.year.sf, gs.sf, k=1) %>% unlist()
 
 # 3D.d assign cc 
 ev.year <- gs %>% 
   dplyr::select(starts_with('pred_')) %>%
   slice(gs_index) %>% 
   bind_cols(ev.year)
 
  #### ------------ ####
  ####  3E. add js  ####
  #### ------------ ####
  
  # 3E.a. bring in js 
  js <- tibble(pred_js = as.vector(readRDS(here::here('inputs', 'pm25', 'base_models', 'annual', 'raw',
                                                      'js', paste0(yyyy, '.rds'))) ))
  
 # keep only otentially relevant points 
 js <- js %>% 
   slice(js_any_index)
 
 # 3E.b find nearest js points for each EV location
  js_index <- get.knnx(js.loc, ev.year.loc, k = 1)$nn.index
  
  # 3E.c assign js 
  ev.year <- js %>%
    slice(js_index) %>% 
    bind_cols(ev.year)
 
  #### ------------ ####
  ####  3F. add me  ####
  #### ------------ ####
  
  # 3F.a. bring in the merra predictions for that year
  me <- read_csv(here::here('inputs', 'pm25', 'base_models', 'annual', 'formatted', 
                            'me', paste0('annual_me_', yyyy, '.csv')))
  
    # 3F.b convert to simple features 
    me.sf <- me %>% 
      sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326")) %>% 
      st_transform(crs = projCRS)
    
    # 3F.c identify nearest CC points 
    me_index <- nngeo::st_nn(ev.year.sf, me.sf, k=1) %>% unlist()
    
    # 3F.d assign cc 
    ev.year <- me %>% 
      dplyr::select(starts_with('pred_')) %>%
      slice(me_index) %>% 
      bind_cols(ev.year)
  
    #### ------------ ####
    ####  3F. add me_blended  ####
    #### ------------ ####
    
    # 3F.a. bring in the merra predictions for that year
    me <- read_csv(here::here('inputs', 'pm25', 'base_models', 'annual', 'formatted', 
                              'me', paste0('annual_me_', yyyy, '_blended.csv')))
    
    # 3F.b convert to simple features 
    me.sf <- me %>% 
      sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326")) %>% 
      st_transform(crs = projCRS)
    
    # 3F.c identify nearest CC points 
    me_index <- nngeo::st_nn(ev.year.sf, me.sf, k=1) %>% unlist()
    
    # 3F.d assign cc 
    ev.year <- me %>% 
      dplyr::select(starts_with('pred_')) %>%
      rename(pred_mb = pred_me) %>%
      slice(me_index) %>% 
      bind_cols(ev.year)
    
    
    
  #### ------------ ####
  ####  3G. add rk  ####
  #### ------------ ####
  
  # 3G.a. bring in the rk raster
  rk <- fst::read_fst(here::here('inputs', 'pm25', 'base_models', 'annual', 'formatted', 
                                 'rk',
                                 paste0('rk_annual_', yyyy, '.fst')))
  
    # 3F.b convert to simple features 
    rk.sf <- rk %>% 
      sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326")) %>% 
      st_transform(crs = projCRS)
    
    # 3F.c identify nearest CC points 
    rk_index <- nngeo::st_nn(ev.year.sf, rk.sf, k=1) %>% unlist()
    
    # 3F.d assign cc 
    ev.year <- rk %>% 
      dplyr::select(starts_with('pred_')) %>%
      slice(rk_index) %>% 
      bind_cols(ev.year)
    
  #### --------------------- ####
  ####  3H. return datasets  ####
  #### --------------------- ####
  
  # 3H.a. return the training dataset 
  ev.year
  
}

#### -------------------------- ####
####  4. save training dataset  ####
#### -------------------------- ####

#  4.a save evdataset
ev.full %>% 
  dplyr::select(lat, lon, yyyy, obs,
                pred_av, pred_cc, pred_cm, pred_gs, pred_js, pred_me, pred_rk) %>%
  readr::write_csv(here::here('str_uncert_analysis', 
                              'data', 'external_validation', 'inputs',
                              'ev_assigned.csv'))

