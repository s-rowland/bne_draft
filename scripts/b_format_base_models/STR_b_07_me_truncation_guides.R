# Robbie: just noticed a few of these have the same description below which need to be updated/included

# File: STR_b_03_average_rk_by_year.R
# Author: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 01/14/2022
#
# Contents:
#  N. Notes
#  0. Import Packages and Global Objects
#  1. Define Function to Get Annual Average
#  2. Average Across the Years

#### ------------------- ####
####       N. NOTES      ####
#### ------------------- ####

# N.1. goal 
# the goal of this function to get CMAQ's estimate of annual average PM2.5 
# concentration. CMAQ does not directly report these values, so we calculate 
# them from the daily values. 

#### ------------------------------------------- ####
####  0. IMPORT PACKAGES AND SET GLOBAL OBJECTS  ####
#### ------------------------------------------- ####

# 0.a. import packages and global objects, if you haven't already done so
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}
  


# 0.e set up parallelization
# 0.e.i get the number of cores
# we subtract one to reserve a core for non-lbic tasks
n.cores <- parallel::detectCores() - 1
# 0.e.ii create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "FORK"
)
#check cluster definition (optional)
print(my.cluster)

# 0.e.iii register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
#check if it is registered (optional)
foreach::getDoParRegistered()

# 1.g. get conus bounding box
# 1.g.i bring in conus shapefile
conus <- sf::st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                                'conus.shp')) %>% 
  sf::st_transform(., crs=st_crs('epsg:4326'))
# 1.g.ii get the bounding box 
bbox.conus <- list(xMin = sf::st_bbox(conus)$xmin[[1]], 
                   xMax = sf::st_bbox(conus)$xmax[[1]], 
                   yMin = sf::st_bbox(conus)$ymin[[1]], 
                   yMax = sf::st_bbox(conus)$ymax[[1]])

#### ------------------------- ####
####  1. make epa region polygons  ####
#### ------------------------- ####

# 1.a. bring in state shapefile 
excludedAreas <- c('Alaska', 'Hawaii', 'Puerto Rico', 
                   'Commonwealth of the Northern Mariana Islands', 'Guam', 
                   'American Samoa', 'United States Virgin Islands')

states <- st_read(here::here('ancillary_data', 'raw', 'census', 'cb_2015_us_state_500k', 
                                    'cb_2015_us_state_500k.shp')) %>% 
  rename(state = STUSPS) %>% 
  filter(!NAME%in%excludedAreas) %>% 
  sf::st_transform(crs=sf::st_crs(projCRS))

# 1.b. assign epa regions 
states <- states %>% 
  inner_join(read_csv(here::here('ancillary_data', 'generated', 'epa_regions.csv')),
             by = 'state')

# 1.d. merge the states into regional polygons
regions <- states %>% 
  mutate(q =5) %>%
  group_by(region) %>% 
  summarise(m = mean(q)) %>% 
  st_cast()   

# 1.d. plot to confirm success
 #plot(regions)

#### ------------------------- ####
####  2. assign epa region to merra cells ####
#### ------------------------- ####

# 2a. bring in one merra 
me.ras <- raster(here::here('inputs','pm25',  'base_models', 'daily', 'raw', 'me', 
                            'daily2010adjPM25sum_v2.nc'), band = 1) %>% 
  projectRaster(crs ='+init=epsg:4326')

# 2.b. crop to be within CONUS
me.ras <- raster::crop(me.ras, 
                       raster::extent(bbox.conus$xMin, 
                                      bbox.conus$xMax,
                                      bbox.conus$yMin, 
                                      bbox.conus$yMax))
# 2.c. put in tidy dataframe
me.coords <- raster::coordinates(me.ras)
me.pm <- raster::extract(me.ras, me.coords)
me <- rbind(tibble::tibble(lon = me.coords[,"x"], 
                           lat = me.coords[,"y"], 
                           pred = me.pm)) 
me <- me %>% 
  mutate(id = row_number())

# 2.d. make simple features 
me <- me %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326"))%>% 
  sf::st_transform(crs=sf::st_crs(projCRS))

# 2.e. join
me.region <- st_join(me, regions, left=FALSE)

# 2.f. deal with boundary
me.exterior <- me %>% 
  filter(! id%in% me.region$id) 


me.exterior$nn <- unlist(nngeo::st_nn(me.exterior, me.region, k=1)) # Robbie: I don't think nngeo is in the list of packages in a_00 
me.exterior$region <- me.region$region[me.exterior$nn]

me.region <- me.region %>%
  bind_rows(me.exterior) %>% 
  dplyr::select(region, id)

#### ---------------------------- ####
####  3. assign yearly max values ####
#### ---------------------------- ####

# 3.a. readin in aqs data 
aqs <- read_fst(here::here('inputs', 'pm25', 'ground_truth', 'formatted',
                          'aqs_daily_curated.fst')) %>% 
  mutate(obs = as.numeric(obs))

# 3.b. assign epa region 
aqs <- aqs %>% 
  inner_join(read_csv(here::here('ancillary_data', 'generated', 'epa_regions.csv')),
                        by = 'state')
aqs.cap <- aqs %>% 
  mutate(yyyy = str_sub(date_local, 0, 4)) %>% 
  group_by(yyyy, region) %>% 
  summarize(cap = max(obs))

# 3.d put in nice format 
aqs.cap <- aqs.cap %>% 
  mutate(yyyy = paste0('cap_', yyyy)) %>%
  pivot_wider(id_cols = region, names_from = yyyy, values_from = cap) 

# 3.e assign to merra 
me.region <- me.region %>% 
  inner_join(aqs.cap, by = 'region') 

me.region <- me.region %>% 
  sf::st_transform(crs=sf::st_crs(plotCRS)) %>%
  mutate(lon = st_coordinates(.)[,1], 
         lat = st_coordinates(.)[,2]) %>%
  as.data.frame() %>% 
  dplyr::select(id, lat, lon, contains('cap')) 

me.region %>%
  arrange(id) %>%
  write_csv(here::here('ancillary_data', 'formatted', 'processing_support', 
                       'me_daily_caps.csv'))
