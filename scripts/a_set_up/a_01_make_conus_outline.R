# Spatial Join of Data 
# BNE Crude Error Assessment 
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# N: Notes
# 0: Preparation 
# 1: Make CONUS Outline

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_set_up_env.R"))
}

####***************************
#### 1: Make CONUS Outline ####
####***************************

# 1a Set excluded areas 
excludedAreas <- c("Alaska", "Hawaii", "Puerto Rico", 
                   "Commonwealth of the Northern Mariana Islands", "Guam", 
                   "American Samoa", "United States Virgin Islands")

# 1b Load the base map from our shapefile
usa <- st_read(here::here('data_ancillary', 'raw', 'Census', 'cb_2015_us_state_500k', 
                          'cb_2015_us_state_500k.shp'))

# 1c Remove regions that will most likely not be included in the 
# contiguous nationwide application 
conusStates <- usa[!usa$NAME%in%excludedAreas,]

# 1d Merge the states into a single polygon
conus <- conusStates %>% 
  mutate(g = 'conus', q = 5) %>%
  group_by(g) %>% 
  summarise(m = mean(q)) %>% 
  st_cast()   

# 1e Transform geographical coordinates to Lambert Azimuth Equal Area Projection
conus <- st_transform(conus, crs=st_crs(projString))

# 1f Save conus shapefile 
conus %>% 
  st_write(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                      'conus.shp'))

# 1g Remove usa file 
rm(usa, conusStates, conus)
