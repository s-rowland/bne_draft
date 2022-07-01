# Task: Regrid Topography Data
# File: c_03_regrid_topography.R
# SubProject: Analysis of BNE PM2.5 Predictive Uncertainty
# Project: Bayesian Nonparametric Ensemble 
# Author: Sebastian T. Rowland <sr3463@cumc.columbia.edu>

#### ------------------- ####
####  Table of Contents  ####
#### ------------------- ####

# N: Notes
# 0: Preparation 
# 1: Calculate Population Density

#### ---------------- ####
####  0: Preparation  ####
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

# 0b set the projection string 
projStringRas <- paste0('+init=', projString)

####*************************************
#### 1: Prepare Reference Grid ####
####*************************************

# 1.a. prepare reference grid 
# 1.a.i. read it in 
refGrid <- fst::read_fst(here::here('inputs', 'pm25', 'reference_grids', 
                                    'refGrid_conus.fst'))
# 1.a.ii. convert to simple features
refGrid <- refGrid %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326"))
refGrid <- refGrid %>% 
  arrange(lat) %>% 
  arrange(lon)

# 1.a.iii. add colum of unique identified
refGrid$cell_id <- 1:nrow(refGrid)

# 1.a.iv. convert to Spatial object
refGrid <- as_Spatial(refGrid)

####*************************************
#### 1: Prepare Reference Grid ####
####*************************************

# First topo
topo1 <- raster(x = here::here(dir.proj, 'data', 'explanatory_variables', 'raw', 
                               'gtopo30', 'gt30w100n40.tif'))

#topo1 <- projectRaster(topo1, crs = crs(projStringRas))
topo1.ref <- raster::extract(topo1, refGrid, df = TRUE, method = 'simple') 
head(topo1.ref)
names(topo1.ref) <- c('cell_id', 'topo')

# second topo file
topo2 <- raster(x = here::here(dir.proj, 'data', 'explanatory_variables', 'raw', 
                               'gtopo30', 'gt30w100n90.tif'))
#topo2 <- projectRaster(topo2, crs = crs(projStringRas))
topo2.ref <- raster::extract(topo2, refGrid, df = TRUE, factors = TRUE, fun = mean) 
names(topo2.ref) <- c('cell_id', 'topo')

# third topo file
topo3 <- raster(x = here::here(dir.proj, 'data', 'explanatory_variables', 'raw', 
                               'gtopo30', 'gt30w140n40.tif'))
#topo3 <- projectRaster(topo3, crs = crs(projStringRas))
topo3.ref <- raster::extract(topo3, refGrid, df = TRUE, factors = TRUE, fun = mean) 
names(topo3.ref) <- c('cell_id', 'topo')

# fourth topo file
topo4 <- raster(x = here::here(dir.proj, 'data', 'explanatory_variables', 'raw', 
                               'gtopo30', 'gt30w140n90.tif'))
#topo4 <- projectRaster(topo4, crs = crs(projStringRas))
topo4.ref <- raster::extract(topo4, refGrid, df = TRUE, factors = TRUE, fun = mean) 
names(topo4.ref) <- c('cell_id', 'topo')

# combine
topo <- bind_rows(topo1.ref, topo2.ref, topo3.ref, topo4.ref) 

# remove missing
topo <- topo %>% filter(!is.na(topo))

####*************************************
#### 1: Prepare Reference Grid ####
####*************************************


topo <- topo %>% 
  group_by(cell_id) %>% 
  summarize(elev = mean(topo))



topo <- refGrid %>% 
  st_as_sf() %>%
  inner_join(topo, by = 'cell_id')

# optional plot to check results

topo.sf <- topo %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326"))
ggplot() + 
  geom_sf(fill = NA)  + 
  geom_sf(data = topo.sf, aes(fill= elev, color = elev), size = 0.32)  


topo %>% 
  mutate(lon = st_coordinates(.)[,1], 
         lat = st_coordinates(.)[,2]) %>%
  as.data.frame() %>% 
  dplyr::select(lat, lon, elev, cell_id) %>%
  write_csv(here::here(dir.proj, 'data', 'explanatory_variables', 'intermediate', 
                       'topo_processed.csv'))



