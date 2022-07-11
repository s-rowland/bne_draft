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
# Robbie: I don't seem to ahve a folder with ancillary_data
conus <- sf::st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                                'conus.shp')) %>% 
  sf::st_transform(., crs=st_crs('epsg:4326'))
# 1.g.ii get the bounding box 
bbox.conus <- list(xMin = sf::st_bbox(conus)$xmin[[1]], 
                   xMax = sf::st_bbox(conus)$xmax[[1]], 
                   yMin = sf::st_bbox(conus)$ymin[[1]], 
                   yMax = sf::st_bbox(conus)$ymax[[1]])

#### ------------------------- ####
####  1. average across years  ####
#### ------------------------- ####

# 1.a. out loop over the years
for (yyyy in 2011:2016) {
#yyyy <- 2011
  # 1.b. inner loop of days within the year
  av.yyyy <- foreach(
    mm.num = 1:12, 
    .combine = 'rbind'
  ) %do% {
   mm <- 12 
    # 1.c. determine the date components of the julian day
   mm <- pad0(mm.num)
  
    # 1.d. bring in av as a raster
   # 3B.a. readin av
   av <- raster(here::here('inputs', 'pm25', 'base_models', 'monthly', 'raw', 'av', 
                           paste0('V5GL02.HybridPM25.NorthAmerica.', yyyy, mm, 
                                  '-', yyyy, mm, '.nc')))
   # 3B.b. crop to be within CONUS
   av.conus <- raster::crop(av, 
                            raster::extent(bbox.conus$xMin, 
                                           bbox.conus$xMax,
                                           bbox.conus$yMin, 
                                           bbox.conus$yMax))
   # 3B.c. put in tidy dataframe
   av.conus.coords <- raster::coordinates(av.conus)
   av <- data.frame(lon = av.conus.coords[,"x"], 
                    lat = av.conus.coords[,"y"], 
                    pred_av = raster::extract(av.conus, av.conus.coords)) %>% # Robbie: are there NA values in any of the rasters? If so they need to be dealt with explicitly here with na.rm=TRUE
     na.omit() # Robbie: I see here that you removed NA values, but is that because you left them in for the extract function? You can avoid this
    
    # 1.g. return dataframe
    av
  
  }
  
  # 1.h. take average and save results. 
  av.yyyy %>% 
    filter(complete.cases(.)) %>% # Robbie: why only complete cases necessary here? are there missing values (related to comment above) that spit out NAs?
    group_by(lat, lon) %>% 
    summarize(pred_av = mean(pred_av)) %>% 
    fst::write_fst(here::here('inputs', 'pm25', 'base_models', 'annual', 'formatted', 'av',
                         paste0('av_annual_', yyyy, '.fst')))
}

# 1.g. end parallelization
stopCluster(my.cluster) # Robbie: very cool suite of functions I should probably get to know these!!!
  