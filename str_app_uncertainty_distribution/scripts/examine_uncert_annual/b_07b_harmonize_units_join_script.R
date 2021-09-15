# Spatial Join of Data 
# Uncertainty Analysis for ISEE
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# N: Notes
# 0: Preparation 
# 1: Readin Uncertainty Dataset 
# 2: Process Explanatory Variable Datasets
# 3: Make Monitor Variables
# 4: Compute Area-Weighted Averaged of Explanatory Variables 
# 5: Join Data
# 6: Save Results

####**************
#### N: Notes ####
####**************

# N1: 

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

# 0c Read reference grid 
# for ISEE it is sufficient to use this 0.1 degree grid
# it is sufficient
refGrid <- fst::read_fst(here::here('uncertainty_factor_analysis_ISEE', 'data', 'generated',  
                                    'refGrid_025Deg_Centroids.fst')) %>% 
  st_as_sf(coords = c("lon", "lat"), crs=st_crs(projString)) %>% 
  mutate(cell_id = row_number())

# 0e Set YYYY 
#YYYY <- 2010

####*********************************
#### 1: Add State and EPA Region ####
####*********************************

# 1a Read state data 
states <- read_sf(here::here('ancillary_data', 'raw', 'Census','cb_2015_us_state_500k', 
                             'cb_2015_us_state_500k.shp'))

# 1b Clean up state data 
states <- states %>% 
  rename(state = STUSPS) %>%
  mutate(state_num = as.numeric(STATEFP)) %>% 
  dplyr::select(-AFFGEOID, -ALAND, -AWATER, -GEOID, -LSAD, -NAME, 
                -STATEFP, -STATENS)

# 1c Read EPA region table
epaRegion <- read_csv(here::here('ancillary_data', 'generated', 'epa_regions.csv'))

# 1d Combine 
states  <- states %>% 
  inner_join(epaRegion, by = 'state')

# 1e Convert projection 
states <- states %>% st_transform(crs=st_crs(projString))

# 1f Assign state and region to points. 
# this will also restrict us to be within CONUS boundaries
refGrid <- st_intersection(refGrid, states)

####************************************
#### 2: Process Uncertainty Dataset ####
####************************************

# 2a Filter BNE predictions for just the run of interest
bneout <- readBNEoutputISEE(YYYY) %>%
  dplyr::select(lat, lon, pred_mean, pred_sd) 

# 2b Convert to simple features and change projection
bneout <- bneout %>% 
  st_as_sf(coords = c('lon', 'lat'), crs=st_crs('epsg:4326')) %>% 
  st_transform(crs=st_crs(projString))

# 2c Add cell_id variable to uncert dataframe
bneout <- bneout %>% 
  mutate(bneout_cell_id = row_number()) 
  
# 2d Identify nearest neighbors
refGrid$bneout_cell_id <- unlist(st_nn(refGrid, bneout, k=1))

# 2e Join 
bneout <- bneout %>% 
  as.data.frame() %>% 
  dplyr::select(bneout_cell_id, pred_mean, pred_sd)

refGrid <- refGrid %>% 
  left_join(bneout, by = 'bneout_cell_id')

####*******************************
#### 3: Make Monitor Variables ####
####*******************************

# 3a Read monitor location data 
monitors <- read_csv(here::here('BNE_Inputs', 'ground_truth', 'formatted',
                                paste0('aqs_annual_', YYYY, '_formatted.csv')))

# 3b Make a monitor variable
monitors <- monitors %>% 
  dplyr::select(lat, lon) %>% 
  mutate(monitor = 1)

# 3c Convert monitors to simple feature (spatial data format)
monitors <- monitors %>% 
  st_as_sf(coords = c('lon', 'lat'), crs=st_crs('epsg:4326')) %>% 
  st_transform(crs=st_crs(projString))

# 3d Define function to create monitor-in-buffer countes
countMonInBuffer <- function(ref, mon, radius){
  # mon is in m; can confirm with st_crs(ref)$units
  # radius should be in km; we will multiply to get m
  #ref <- refGrid; mon <- monitors ; radius <- 10
  VarName <- paste0('mon_count_', radius, 'km')
  baseBuff <- st_buffer(ref, radius*1000)
  monInBuff <- st_join(mon, baseBuff, join = st_within) %>%
    group_by(cell_id) %>% 
    summarize(buffCount := n()) %>% 
    as.data.frame() %>% 
    dplyr::select(cell_id, buffCount)
  
  ref <- ref %>% 
    full_join(monInBuff, by = 'cell_id') %>% 
    mutate(!!VarName := if_else(is.na(buffCount), 0, as.numeric(buffCount))) %>% 
    dplyr::select(-buffCount) %>% 
    filter(!is.na(cell_id))
  
  return(ref)
}

# 3e Make the monitor-in-buffer variables
refGrid <- countMonInBuffer(refGrid, monitors, 10)
refGrid <- countMonInBuffer(refGrid, monitors, 20)
refGrid <- countMonInBuffer(refGrid, monitors, 30)

# 3f Define function to create monitor-in-buffer countes
findNearestMon <- function(ref, mon, numNNMon){
  # mon is in m; can confirm with st_crs(ref)$units
  # radius should be in km; we will multiply to get m
  # ref <- refGrid; mon <- monitors ; numNNMon <- 1
  # make variable name
  VarName <- paste0('monDist', numNNMon, '.mean')
  # Find distances of nearest monitors
  monDist <- st_nn(ref, mon, k=numNNMon, returnDist = TRUE)$dist 
  # average those distances
  monDist.mean <- map(monDist, mean)
  #add to the reference grid
  # and convert to km
  ref$monNNdist <- monDist.mean
  ref <- ref %>% 
    mutate(!!VarName := as.numeric(monNNdist) /1000) %>% 
    dplyr::select(-monNNdist)
  
  return(ref)
}

# 3g Actually find the distance to the nearest monitors 
refGrid <- findNearestMon(refGrid, monitors, 1)
refGrid <- findNearestMon(refGrid, monitors, 2)
refGrid <- findNearestMon(refGrid, monitors, 3)
refGrid <- findNearestMon(refGrid, monitors, 4)
refGrid <- findNearestMon(refGrid, monitors, 5)

####**************************************
#### 4: Add Seasonal Mean Temperature ####
####**************************************

# 4a Read PRISM data 
prism.winter <- raster(here::here('uncertainty_factor_analysis_ISEE','data', 
                                  'intermediate', 'PRISM_Seasonal',  
                                  paste0('winter_meanT_', YYYY)))
prism.summer <- raster(here::here('uncertainty_factor_analysis_ISEE','data', 
                                  'intermediate', 'PRISM_Seasonal', 
                                  paste0('summer_meanT_', YYYY)))

# 4b Convert projection 
prism.winter <- projectRaster(prism.winter, crs = crs(projStringRas))
prism.summer <- projectRaster(prism.summer, crs = crs(projStringRas))

# note: each of these extracts takes about an hour to run 
# 4c Compute value for winter-mean temperature
# 4c.i Get means of prism within polygons
prismWin.ref <- raster::extract(prism.winter, refGrid, 
                                df = TRUE, factors = TRUE, fun = mean) 
# 4c.ii Add grid id's back to prism
prismWin.ref$cell_id <- refGrid$cell_id
# 4c.iii Rename variable 
prismWin.ref <- prismWin.ref %>% 
  rename(winter_temp = layer) %>% 
  dplyr::select(-ID)
# 4c.iv Add to refGrid
refGrid <- refGrid %>% 
  left_join(prismWin.ref, by = 'cell_id')

# 4d Compute value for summer-mean temperature
prismSum.ref <- raster::extract(prism.summer, refGrid, 
                                df = TRUE, factors = TRUE, fun = mean) 
# 4d.ii Add grid id's back to prism
prismSum.ref$cell_id <- refGrid$cell_id
# 4d.iii Rename variable 
prismSum.ref <- prismSum.ref %>% 
  rename(summer_temp = layer) %>% 
  dplyr::select(-ID)
# 4d.iv Add to refGrid
refGrid <- refGrid %>% 
  left_join(prismSum.ref, by = 'cell_id')


####*******************************
#### 5: Add Population Density ####
####*******************************

# 5a Read population density data
popD <- st_read(here::here('uncertainty_factor_analysis_ISEE', 'data', 
                           'intermediate', 'pop_density_zcta.shp')) %>% 
  rename(pop_density = popD)

# 5b Change projection 
popD <- popD %>% st_transform(crs=st_crs(projString))

# 5c Join via intersection 
refGrid <- st_intersection(refGrid, popD)

####************************
#### 6: Add Cloud Cover ####
####************************

# 6a Read cloud cover data
cloud <- raster(here::here('uncertainty_factor_analysis_ISEE','data', 
                           'raw', 'GIOVANNI',  
                           paste0('GIOVANNI-timeAvgMap_MYD08_D3_6_1_Cloud_Fraction_Mean_', YYYY, 
                                  '0101-', YYYY, 
                                  '1231_130W_24N_65W_52N.tif')))

# 6b Convert projection
cloud <- projectRaster(cloud, crs = crs(projStringRas))

# 6c Compute value for cloud cover
cloud.ref <- raster::extract(cloud, refGrid, 
                             df = TRUE, factors = TRUE, fun = mean) 
# 6c.ii Add grid id's back to prism
cloud.ref$cell_id <- refGrid$cell_id
# 6c.iii Rename variable 
names(cloud.ref) <- c('ID', 'cloud_cover', 'cell_id')
cloud.ref <- cloud.ref %>% 
  dplyr::select(-ID)
# 6c.iv Add to refGrid
refGrid <- refGrid %>% 
  left_join(cloud.ref, by = 'cell_id')

####**********************
#### 7: Add Elevation ####
####**********************

# 7a Read elevation data
elev <- read_csv(here::here('uncertainty_factor_analysis_ISEE', 'data', 'intermediate', 
                            'topo_processed.csv'))


# elev is already in the correct projection

# 7b Add to refGrid
refGrid <- refGrid %>% 
  left_join(elev, by = 'cell_id')


####*********************
#### 8: Save Dataset ####
####*********************
  
# 8a Save
refGrid %>% 
  mutate(lon = st_coordinates(.)[,1], 
         lat = st_coordinates(.)[,2]) %>%
  as.data.frame() %>% 
  dplyr::select(-geometry) %>%
  write_fst(here::here('uncertainty_factor_analysis_ISEE', 'data', 'processed', 
                       paste0('AVGSCMJSCC_3.5_explanatory_', YYYY, '_025deg.fst')))

