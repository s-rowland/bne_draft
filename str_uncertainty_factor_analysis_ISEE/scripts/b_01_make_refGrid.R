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
# 2: Identify Grid Centroids 
# 3: Restrict to CONUS
# 4: Make Polygons

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
#projString <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 
  #  +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

# 0b Readin Conus
conus <- st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                            'conus.shp'))

####***************************
#### 1: Make CONUS Outline ####
####***************************

# 1a Set excluded areas 
excludedAreas <- c("Alaska", "Hawaii", "Puerto Rico", 
                   "Commonwealth of the Northern Mariana Islands", "Guam", 
                   "American Samoa", "United States Virgin Islands")
# 1b Load the base map from our shapefile
usa <- st_read(here::here('ancillary_data', 'raw', 'Census', 'cb_2015_us_state_500k', 
                          'cb_2015_us_state_500k.shp'))

# 1c Remove regions that will most likely not be included in the 
# contiguous nationwide application 
conusStates <- usa[!usa$NAME%in%excludedAreas,]

####******************************************
#### 2: Identify Potential Grid Centroids ####
####******************************************

# 2a convert geometry to character format
# we can more easily manipulate characters
grid <- as.character(conusStates$geometry) 

# 2b Replace various characters so that the coordiantes looks like a list of 
# numbers
grid2 <- str_replace_all(grid, 'list\\(', ',')
grid3 <- str_replace_all(grid2, 'c\\(', ',')
grid4 <- str_replace_all(grid3, ',,,', ',')
grid5 <- str_replace_all(grid4, ',,', ',')
grid6 <- str_replace(grid5, ",", '')

# 2c Split up the grids so that each coordinate element is its own string
grid7 <- str_split(grid6, ",")

# 2d Put all of these together into one vector
grid8 <- unlist(grid7)

# 2e Convert to numeric 
loc <- data.frame(coordinates = as.numeric(unlist(grid8)))

# 2f Break up into latitude and longitude
lat <- loc %>% filter(coordinates >0)
lon <- loc %>% filter(coordinates <0)

# 2g Find the minimums and maximums
min.lat <- min(lat$coordinates, na.rm = TRUE)
max.lat <- max(lat$coordinates, na.rm = TRUE)
min.lon <- min(lon$coordinates, na.rm = TRUE)
max.lon <- max(lon$coordinates, na.rm = TRUE)

# 2h Make vectors of all the possible coordiante elements 
# this is the key step were we decide the number of elements, 
# the size of the grids, etc. 
seqLat <- seq(min.lat, max.lat, by = 0.25)
seqLon <- seq(min.lon, max.lon, by = 0.25)

# 2g Combine coordinate elements into grids
dta <- data.frame(lat = rep(seqLat, length(seqLon)), 
                  lon = rep(seqLon, length(seqLat)))

# 2h Clean up
rm(list=c(ls(pattern="grid"), ls(pattern="lat"), ls(pattern="lon"), loc))

####**************************
#### 3: Restrict to CONUS ####
####**************************

dta <- dta %>% 
  mutate(lat_extreme = if_else(lat < quantile(dta$lat, 0.2)[1] | 
                                 lat > quantile(dta$lat, 0.8)[1], 1, 0), 
         lon_extreme = if_else(lon < quantile(dta$lon, 0.2)[1] | 
                                 lon > quantile(dta$lon, 0.8)[1], 1, 0))
# 3a Convert to spatial (simple feature)
dta.sf <- st_as_sf(dta, coords = c("lon", "lat"), crs=st_crs("epsg:4326"))

# 3b Transform geographical coordinates to Lambert Azimuth Equal Area Projection
dta.sf <- st_transform(dta.sf, crs=st_crs(projString))

# 3d Intersect with CONUS, keep only overlapping points
# an alternative to to just intersect the points that are in the top or botto 
# 20th percentile for lat or for long 

dta.sf.intersec.extremes <- dta.sf %>% 
  filter(lat_extreme + lon_extreme > 0) %>%
  st_join(conus, join = st_within) #%>%
  #filter(is.na(NAME))
dta.sf.intersec.extremes0 <- dta.sf.intersec.extremes  %>% 
  filter(!is.na(g))

# 3e combine 
grid0 <- dta.sf %>% 
  filter(lat_extreme + lon_extreme == 0) %>% 
  bind_rows(dta.sf.intersec.extremes0) 

# Save the centroids
grid0 %>% 
  mutate(lon = st_coordinates(.)[,1], 
         lat = st_coordinates(.)[,2]) %>%
  as.data.frame() %>% 
  dplyr::select(lat, lon) %>%
  fst::write_fst(here::here('uncertainty_factor_analysis_ISEE', 'data', 'generated',  
                            'refGrid_025Deg_Centroids.fst'))

####**********************
#### 4: Make Polygons ####
####**********************
grid0 <- dta.sf0
grid.v1 <- st_union(grid0)
# now we make the voronoi polygons 
# this geometry collection only has 1 obs 
# we next want to split up this observation 
# into 1 obs per polgyon
grid.v2 <- st_voronoi(grid.v1)
# split into 1 obs per polygon 
# but it is still an sfc rather than an sf. 
grid.v3 <- st_collection_extract(grid.v2)
# convert to sf 
grid.v4 <- st_sf(grid.v3)
# looking at area
grid.v4$area <- st_area(grid.v4)
grid.v4 <- grid.v4 %>% 
  mutate(area = as.numeric(str_sub(area, 0, -3)))

# clip to CONUS outline 
#this line takes about 10 minutes to run when we use 10% of the data. 
# so we will need a real solution for later...
# potentialy spatially aggregating the data
grid.v4 <- grid.v4 %>% 
  mutate(area_extreme = if_else(area > quantile(grid.v4$area, 0.2)[1], 1, 0))
grid.v4.extreme <- grid.v4 %>% filter(area_extreme == 1)
gridt.v5.extreme <- st_intersection(grid.v4.extreme, conus)
grid.v5 <- grid.v4 %>% 
  filter(area_extreme == 0) %>%
  bind_rows(gridt.v5.extreme)

# Save the result 
grid.v5 %>% 
  st_write(here::here('uncertainty_factor_analysis_ISEE', 'data', 'generated', 
                      'refGrid_025Deg_Polygons', 'refGrid_025Deg_Polygons.shp'))
