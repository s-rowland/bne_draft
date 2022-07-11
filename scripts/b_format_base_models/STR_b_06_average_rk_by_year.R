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

#### ------------------------- ####
####  1. average across years  ####
#### ------------------------- ####

# 1.a. out loop over the years
for (yyyy in 2010:2016) {

  # 1.b. inner loop of days within the year
  rk.yyyy <- foreach(
    julian_day = 1:365, # Robbie: what about leap years?
    .combine = 'rbind'
  ) %dopar% {
    
    # 1.c. determine the date components of the julian day
    activeDate <- parse_date_time(paste0(yyyy, '-01-01'), 'ymd') + 60*60*24*julian_day
   mm <- pad0(month(activeDate)); dd <- pad0(day(activeDate))
  
    # 1.d. bring in rk as a raster
    rk.ras <- ncdf4::nc_open(here::here('inputs', 'pm25', 'base_models', 'daily', 'raw', 'rk', 
                                        paste0('cmaqout_', yyyy, '-', mm, '-', dd, 
                                               '_pm25_daily_avg_lon_lat_UCAR_CONUS.nc')))
    
    # 1.e. create dataframe from raster
    if (as.numeric(year(activeDate)) == yyyy) {
      rk <- data.frame(
        pred_rk = as.vector(ncdf4::ncvar_get(rk.ras, 'pm25_daily_avg')), 
        lat = as.vector(ncdf4::ncvar_get(rk.ras, 'latitude')), 
        lon = as.vector(ncdf4::ncvar_get(rk.ras, 'longitude'))
      )
      # address leap years
    } else {
      rk <- data.frame(
        pred_rk = NA, 
        lat = NA, 
        lon = NA)
    }
    
    # 1.f. close the netCDf object 
    nc_close(rk.ras)
    
    # 1.g. return dataframe
    rk
  
  }
  
  # 1.h. take average and save results. 
  rk.yyyy %>% 
    filter(complete.cases(.)) %>% 
    group_by(lat, lon) %>% 
    summarize(pred_rk = mean(pred_rk)) %>% 
    write_fst(here::here('inputs', 'pm25', 'base_models', 'annual', 'formatted', 'rk',
                         paste0('rk_annual_', yyyy, '.fst')))
}

# 1.g. end parallelization
stopCluster(my.cluster)
  