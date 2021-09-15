# Calculate Population Density
# Uncertainty Analysis for ISEE
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# N: Notes
# 0: Preparation 
# 1: Calculate Population Density

####********************
#### 0: Preparation ####
####********************

# N1: # think about using tidycensus for final version of code

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_set_up_env.R"))
}

if(!exists("Ran_a_00_ISEE")){
  here::i_am("README.md")
  source(here::here('uncertainty_factor_analysis_ISEE', 'scripts', 
                    "a_00_set_up_env_ISEE.R"))
}

# 0b set the projection string 
projStringRas <- paste0('+init=', projString)

####*************************************
#### 1: Calculate Population Density ####
####*************************************

refGrid <- fst::read_fst(here::here('uncertainty_factor_analysis_ISEE', 'data', 'generated',  
                                       'refGrid_025Deg_Centroids.fst')) %>% 
  st_as_sf(coords = c("lon", "lat"), crs=st_crs(projString)) 

refGrid$cell_id <- 1:nrow(refGrid)


# First topo
topo1 <- raster(x = here::here('uncertainty_factor_analysis_ISEE', 'data', 'raw', 
                               'gtopo30', 'gt30w100n40.tif'))
topo1 <- projectRaster(topo1, crs = crs(projStringRas))
topo1.ref <- raster::extract(topo1, refGrid, df = TRUE, factors = TRUE, fun = mean) 
names(topo1.ref) <- c('cell_id', 'topo')

# second topo file
topo2 <- raster(x = here::here('uncertainty_factor_analysis_ISEE', 'data', 'raw', 
                               'gtopo30', 'gt30w100n90.tif'))
topo2 <- projectRaster(topo2, crs = crs(projStringRas))
topo2.ref <- raster::extract(topo2, refGrid, df = TRUE, factors = TRUE, fun = mean) 
names(topo2.ref) <- c('cell_id', 'topo')

# third topo file
topo3 <- raster(x = here::here('uncertainty_factor_analysis_ISEE', 'data', 'raw', 
                               'gtopo30', 'gt30w140n40.tif'))
topo3 <- projectRaster(topo3, crs = crs(projStringRas))
topo3.ref <- raster::extract(topo3, refGrid, df = TRUE, factors = TRUE, fun = mean) 
names(topo3.ref) <- c('cell_id', 'topo')

# fourth topo file
topo4 <- raster(x = here::here('uncertainty_factor_analysis_ISEE', 'data', 'raw', 
                               'gtopo30', 'gt30w140n90.tif'))
topo4 <- projectRaster(topo4, crs = crs(projStringRas))
topo4.ref <- raster::extract(topo4, refGrid, df = TRUE, factors = TRUE, fun = mean) 
names(topo4.ref) <- c('cell_id', 'topo')

# combine
topo <- bind_rows(topo1.ref, topo2.ref, topo3.ref, topo4.ref) 

# remove missing
topo <- topo %>% filter(!is.na(topo))

topo <- topo %>% 
  group_by(cell_id) %>% 
  summarize(elev = mean(topo))

topo.sm <- topo %>% 
  sample_frac(0.5)

topo <- refGrid %>% 
  inner_join(topo, by = 'cell_id')

ggplot() + 
  geom_sf(fill = NA)  + 
  geom_sf(data = topo1, aes(fill= elev, color = elev), size = 0.32)  


topo %>% 
  mutate(lon = st_coordinates(.)[,1], 
         lat = st_coordinates(.)[,2]) %>%
  as.data.frame() %>% 
  dplyr::select(lat, lon, elev, cell_id) %>%
  write_csv(here::here('uncertainty_factor_analysis_ISEE', 'data', 'intermediate', 
                       'topo_processed.csv'))

