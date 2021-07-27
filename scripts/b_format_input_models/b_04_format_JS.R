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
  source(here::here('scripts', 'a_set_up', "a_00_set_up_env.R"))
}

# 0b Read CONUS shapefile
conus <- st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                               'conus.shp'))

####*******************************
#### 1: Wrangle JS Predictions ####
####*******************************

# 1a Readin locations
jsLoc <- readr::read_rds(here::here('BNE_inputs', 'input_models', 'raw', 'JS_annual_raw', 
             'USGridSite.rds'))

# 1b Remove unnecesary column
jsLoc <- jsLoc %>% 
  rename(lat = Lat, lon = Lon) %>% 
  dplyr::select(lat, lon)

# 1c Loop 
for(YYYY in 2010:2015){
  #YYYY <- 2010
  # 1c Readin predictions as a vector
  jsPred <- readr::read_rds(
    here::here('BNE_inputs', 'input_models', 'raw', 'JS_annual_raw', 
               paste0('PredictionStep2_Annual_PM25_USGrid_',
                      YYYY, '0101_', YYYY, '1231.rds')))
  
  # 1d Determine cut-offpoints 
  # note: right now we split up the data into thirds for computation efficiency/ 
  # memory limits
  cut1a <- floor(length(jsPred)*1/3)
  cut1b <- ceiling(length(jsPred)*1/3)
  cut2a <- floor(length(jsPred)*2/3)
  cut2b <- ceiling(length(jsPred)*2/3)
  cut3 <- length(jsPred)
  
  # 1e Combine loc and pred
  # we split JS into three dataframes to overcome memory constraints
  jsPred1 <- jsPred[1:cut1a]
  jsPred1 <- data.frame(JS = jsPred1, 
                         lat = jsLoc$lat[1:cut1a], 
                         lon = jsLoc$lon[1:cut1a])
  
  jsPred2 <- jsPred[cut1b:cut2a]
  jsPred2 <- data.frame(JS = jsPred2, 
                         lat = jsLoc$lat[cut1b:cut2a], 
                         lon = jsLoc$lon[cut1b:cut2a])
  
  jsPred3 <- jsPred[cut2b:cut3]
  jsPred3 <- data.frame(JS = jsPred3, 
                         lat = jsLoc$lat[cut2b:cut3], 
                         lon = jsLoc$lon[cut2b:cut3])
  
  # 1f Combine the thirds
  js <- bind_rows(jsPred1, jsPred2, jsPred3)
  
  # 1g Convert to simple features
  # note that we do not convert to projString... we want to keep it in coordinates 
  # for later. 
  # Since this is just a basic spatial filter, the projection shouldn't matter much
  js.sf <- js %>% 
    st_as_sf(., coords = c("lon", "lat"), 
             crs=st_crs('epsg:4326')) 
  
  # 2i Restrict to conus
  conus.epsg4326 <- conus %>% 
    st_transform(crs=st_crs('epsg:4326'))

  js.sf <- js.sf %>% 
    st_join(conus.epsg4326, st_intersects) %>% 
    filter(!is.na(g)) %>% 
    dplyr::select(-g, -m)
  
  js <- js.sf %>% 
    as.data.frame() %>% 
    dplyr::select(-geometry)
  
  js$lon <- st_coordinates(js.sf)[,1]
  js$lat <- st_coordinates(js.sf)[,2]
  
  js %>% 
    write_fst(here::here('BNE_inputs', 'input_models', 'formatted', 'JS_annual_formatted',
                         paste0('JS_annual_', YYYY, '_formatted.fst')))
}
