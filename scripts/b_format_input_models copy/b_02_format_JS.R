# Format JS
# Prepare BNE Inputs
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Wrangle JS Predictions

####**************
#### N: Notes ####
####**************

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_setUp_env.R"))
}

####*******************************
#### 1: Wrangle JS Predictions ####
####*******************************

# 1a Readin locations
js.loc <- readr::read_rds(here::here('data_input_models', 'raw', 'JS_annual_raw', 
             'USGridSite.rds'))

# 1b Remove unnecesary column
js.loc <- js.loc %>% 
  rename(lat = Lat, lon = Lon) %>% 
  dplyr::select(lat, lon)

# 1c Loop 
for(YYYY in 2010:2015){
  #YYYY <- 2010
  # 1c Readin predictions as a vector
  js.pred <- readr::read_rds(
    here::here('data_input_models', 'raw',  'JS_annual_raw', 
               paste0('PredictionStep2_Annual_PM25_USGrid_',
                      YYYY, '0101_', YYYY, '1231.rds')))
  
  # 1d Determine cut-offpoints 
  # note: right now we split up the data into thirds for computation efficiency/ 
  # memory limits
  cut1a <- floor(length(js.pred)*1/3)
  cut1b <- ceiling(length(js.pred)*1/3)
  cut2a <- floor(length(js.pred)*2/3)
  cut2b <- ceiling(length(js.pred)*2/3)
  cut3 <- length(js.pred)
  
  # 1e Combine loc and pred
  # we split JS into three dataframes to overcome memory constraints
  js.pred1 <- js.pred[1:cut1a]
  js.pred1 <- data.frame(JS = js.pred1, 
                         lat = js.loc$lat[1:cut1a], 
                         lon = js.loc$lon[1:cut1a])
  
  js.pred2 <- js.pred[cut1b:cut2a]
  js.pred2 <- data.frame(JS = js.pred2, 
                         lat = js.loc$lat[cut1b:cut2a], 
                         lon = js.loc$lon[cut1b:cut2a])
  
  js.pred3 <- js.pred[cut2b:cut3]
  js.pred3 <- data.frame(JS = js.pred3, 
                         lat = js.loc$lat[cut2b:cut3], 
                         lon = js.loc$lon[cut2b:cut3])
  
  # 1f Combine the thirds
  js <- bind_rows(js.pred1, js.pred2, js.pred3)
  
  # 1g Convert to simple features
  # note that we do not convert to projString... we want to keep it in coordinates 
  # for later. 
  # Since this is just a basic spatial filter, the projection shouldn't matter much
  js.point.sf <- js %>% 
    st_as_sf(., coords = c("lon", "lat"), 
             crs=st_crs('epsg:4326')) 
  
  # 2i Restrict to conus
  conus.epsg4326 <- conus %>% 
    st_transform(crs=st_crs('epsg:4326'))

  js.point.sf <- js.point.sf %>% 
    st_join(conus.epsg4326, st_intersects) %>% 
    filter(!is.na(g)) %>% 
    dplyr::select(-g, -m)
  
  js <- js.point.sf %>% 
    as.data.frame() %>% 
    dplyr::select(-geometry)
  
  js$lon <- st_coordinates(js.point.sf)[,1]
  js$lat <- st_coordinates(js.point.sf)[,2]
  
  js %>% 
    write_fst(here::here('data_input_models', 'formatted', 'JS_annual_formatted',
                         paste0('JS_annual_', YYYY, '_formatted.fst')))
}
