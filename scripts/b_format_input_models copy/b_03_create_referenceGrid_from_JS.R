# Join Input Models 
# Prepare BNE Inputs
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Wrangle Monitor Data
# 2: Wrangle JS 
# 3: Save refGrid

####**************
#### N: Notes ####
####**************

# We might want to adjsut the filter statement to get all of washington state. 
# right now the borders feel a little funny. 

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_setUp_env.R"))
}

# 0b Set seed 
set.seed(1234)

####******************************
#### 1: Wrangle Training Data ####
####******************************

# 1a Readin training data 
readinAqs <- function(YYYY){
  read_csv(here::here('data_ground_truth', 'formatted',
                      paste0('aqs_annual_',YYYY, '_formatted.csv')))
}
aqs <- map(2010:2015, readinAqs) %>%
  bind_rows()

# 1b Create simple features version of training data 
mon.point.sf <- aqs %>% 
  dplyr::select(lat, lon) %>%
  distinct() %>% 
  mutate(monID = row_number()) %>%
  st_as_sf(., coords = c("lon", "lat"), crs=st_crs('epsg:4326')) %>%
  st_transform(crs=st_crs(projString))

# 1d Restrict to conus
mon.point.sf  <- mon.point.sf  %>% 
  st_join(conus, st_intersects) %>% 
  filter(!is.na(g)) %>% 
  dplyr::select(-g, -m)

####*******************************
#### 2: Wrangle JS Predictions ####
####*******************************

# 2a Readin JS
js <- read_fst(here::here('data_input_models', 'formatted', 'JS_annual_formatted',
                          paste0('JS_annual_', 2010, '_formatted.fst'))) %>% 
  mutate(cellIndex = row_number()) %>% 
  mutate(lat2 = lat, lon2 = lon)

# 2b convert JS to simple features
js.point.sf <- st_as_sf(js, coords = c("lon", "lat"), 
                        crs=st_crs('epsg:4326')) %>% 
  st_transform(crs=st_crs(projString))

# 2c Identify which JS prediction pairs to each monitoring location 
mon.point.sf$cellIndex <- unlist(st_nn(mon.point.sf, js.point.sf, k=1))

# 2d Keep only 1% of the locations, plus the locations nearest to the monitors. 
# first, preserve the grids closes to the monitors
js.point.sf.atMon <- js.point.sf %>% 
  filter(cellIndex %in% mon.point.sf$cellIndex)
# then, remove those locations at monitors, 
# remove 99% of observations (in spatial order)
# then add back the locations of the monitors
refGrid.sf <- js.point.sf %>% 
  filter(!(cellIndex %in% mon.point.sf$cellIndex)) %>% 
  #arrange(lon2) %>% 
  arrange(lat2) %>% 
  arrange(lon2) %>% 
  filter(12 == str_sub(row_number(),-2, -1) |
        10 > row_number() | 
           9100980 <  row_number()) %>% 
  bind_rows(js.point.sf.atMon) %>% 
  st_transform(crs=st_crs('epsg:4326')) %>% 
    arrange(lat2) %>% 
    arrange(lon2)

refGrid.sf2 <- refGrid.sf2 %>% 
  st_transform(crs=st_crs(projString))
  # plot to confirm that it evenly removed locations. 
png('~/Desktop/samplefrac.png')
ggplot(refGrid.sf2) + 
    geom_sf(aes(fill = JS, color = JS))  
  dev.off()
####****************************
#### 3: Save Reference Grid ####
####****************************

# 3a Convert back to dataframe 
refGrid <- refGrid.sf %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry) 
# add the coordinates back in 
refGrid$lon <- st_coordinates(refGrid.sf)[,1]
refGrid$lat <- st_coordinates(refGrid.sf)[,2]

# 3b Save data set 
refGrid %>% 
  dplyr::select(-cellIndex, -PredIndex) %>% 
  distinct() %>%
  mutate(cellID = row_number())%>% 
  dplyr::select(lon, lat) %>% 
  write_fst(here::here('data_ancillary', 'final', 
                       'refGrid_JS_1percent.fst'))
