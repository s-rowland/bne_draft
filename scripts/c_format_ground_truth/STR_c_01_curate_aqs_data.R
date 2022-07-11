# File: STR_c_01_curate_aqs_data.R
# Author: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 03/17/2022
#
# Contents:
#  N. notes
#  0. import packages and set global objects
#  1. set up foreach loop 
#  2. curate daily observations
#  3. curate annual observations

#### --------- ####
#### N. notes  ####
#### --------- ####

# N.1 daily criteria: 


#### ------------------------------------------ ####
####  0. import packages and set global objects ####
#### ------------------------------------------ ####

# 0.a. import packages and global objects, if you haven't already done so
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

#### ------------------------ ####
####  1. set up foreach loop  ####
#### ------------------------ ####

# 1.a. bring in conus shapefile 
conus <-
  st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                      'conus.shp'))

# 1.b. dataframe of state names
states.df <- data.frame(state = c(state.abb, 'DC'), 
                        state_name = c(state.name, 'District Of Columbia')) 

# 1.c set up parallelization 
# 1.c.i get the number of cores
# we subtract one to reserve a core for non-lbic tasks
n.cores <- parallel::detectCores() - 1
# 1.c.ii create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "FORK"
)
#check cluster definition (optional)
print(my.cluster)

# 1.c.iii register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
#check if it is registered (optional)
foreach::getDoParRegistered()

# 1.d begin loop
aqs <- foreach(
  yyyy = 2005:2016, 
  .combine = 'rbind'
  ) %dopar% {
 
  #### ------------------------------ ####
  ####  2. curate daily observations  ####
  #### ------------------------------ ####
  
  # 2.a. bring in daily aqs data 
  aqs.daily <- read_csv(here::here('inputs', 'pm25', 'ground_truth', 'raw', 'daily', 
                             paste0('daily_88101_', yyyy, '.csv')), 
                  col_types = cols(.default = "c")) %>% 
    janitor::clean_names() %>%
    rename(obs = arithmetic_mean, 
           lat = latitude, 
           lon = longitude) %>%
    mutate(state_code = str_pad(state_code, 2, pad="0"),
           county_code = str_pad(county_code, 3, pad="0"),
           site_num = str_pad(site_num, 4, pad="0"),
           parameter_code = str_pad(parameter_code, 5, pad="0"),
           poc = str_pad(poc, 1, pad="0"),
           monitor_id = str_c(state_code, county_code, site_num, parameter_code, poc, sep = "-"), 
           site_id = str_c(state_code, county_code, site_num, sep = "-"), 
           date_local_formattted = as.POSIXct(date_local, format = "%Y-%m-%d"), 
           year = lubridate::year(date_local_formattted), 
           month = lubridate::month(date_local_formattted), 
           monitor_days = str_c(monitor_id, date_local, sep = "-"),
           monitor_years = str_c(monitor_id, year, sep = "-"),
           lat = as.numeric(lat), 
           lon = as.numeric(lon))
  
# 2.b. apply initial inclusion criteria
# 24-hr observations
# observation percent at or above 75% 
# event type is not excluded
 aqs.daily <- aqs.daily %>% 
  filter(sample_duration %in% c('24 HOUR', '24-HR BLK AVG')) %>% 
   filter(as.numeric(observation_percent) >= 75) %>%
   filter(event_type %in% c('None', 'Included')) %>%
   distinct()

 # 2.c. for each day-site combination, choose the highest POC between 1 or 2
 aqs.daily <- aqs.daily %>% 
   filter(poc %in% c('1', '2')) %>%
   group_by(state_code, county_code, site_num, date_local) %>% 
   summarize(poc = min(poc)) %>% 
   ungroup() %>%
   inner_join(aqs.daily, by = c('state_code', 'county_code', 'site_num', 
                          'date_local', 'poc'))
 
# 2.d. add state initials and restrict to conus
 aqs.daily <- aqs.daily %>% 
   inner_join(states.df, by = c('state_name')) %>% 
   filter(!(state %in% c('AK', 'HI')))
 
 # 2.e. correct datum 
 aqs.daily <- aqs.daily %>% 
   filter(datum == 'NAD83') %>% 
   sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs("epsg:4269")) %>% # Robbie: How do you know these are the correct projections
   sf::st_transform(crs = sf::st_crs("epsg:4326")) %>% # Robbie: As above
   cbind(., sf::st_coordinates(.)) %>%
   dplyr::rename(lon = X, lat = Y) %>% 
   tibble::as_tibble() %>%
   dplyr::select(-geometry) %>% 
   bind_rows(aqs.daily%>%filter(datum=='WGS84')) # combined with points already recorded in epsg:4326
 
# 2.f. keep only relevant variables 
 aqs.daily <- aqs.daily %>% 
   dplyr::select(lat, lon, date_local, obs, site_id, state, poc)  %>%
   distinct()
 
 #### ------------------------------- ####
 ####  3. curate annual observations  ####
 #### ------------------------------- ####
 
# 3.a. bring in annual aqs data 
aqs.annual <- read_csv(here::here('inputs', 'pm25', 'ground_truth', 'raw', 'annual', 
                                 paste0('annual_conc_by_monitor_', yyyy, '.csv')), 
                      col_types = cols(.default = "c")) %>% 
  janitor::clean_names() %>%
  rename(lat = latitude, 
         lon = longitude, 
         date_local = year,
         obs = arithmetic_mean, 
         obs_std = arithmetic_standard_dev) %>%
  mutate(state_code = str_pad(state_code, 2, pad="0"),
         county_code = str_pad(county_code, 3, pad="0"),
         site_num = str_pad(site_num, 4, pad="0"),
         parameter_code = str_pad(parameter_code, 5, pad="0"),
         poc = str_pad(poc, 1, pad="0"),
         monitor_id = str_c(state_code, county_code, site_num, parameter_code, poc, sep = "-"), 
         site_id = str_c(state_code, county_code, site_num, sep = "-"),
         lat = as.numeric(lat), 
         lon = as.numeric(lon))

  # 3.b. apply initial inclusion criteria
  # 88101 parameter code 
  # 24-hr observations
  # observation percent at or above 75% 
  # event type is not excluded
   # poc = 1 (primary)
  aqs.annual <- aqs.annual %>% 
    filter(parameter_code == '88101') %>%
    filter(poc == '1') %>%
    filter(metric_used == 'Daily Mean') %>%
    filter(sample_duration %in% c('24 HOUR', '24-HR BLK AVG')) %>% 
    filter(as.numeric(observation_percent) >= 75) %>%
    filter(event_type %in% c('Events Included', 'No Events')) %>%
    distinct() # Robbie: were there many duplicates? Maybe a brief stat here of what happened? Nice to have, not essential
  
  # 3.c. add state initials and restrict to conus
  aqs.annual <- aqs.annual %>% 
    inner_join(states.df, by = c('state_name')) %>% 
    filter(!(state %in% c('AK', 'HI'))) # Robbie: I always feel bad for people in these states...
  
  # 3.d. correct datum 
  aqs.annual <- aqs.annual %>% 
    filter(datum == 'NAD83') %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs("epsg:4269")) %>%
    sf::st_transform(crs = sf::st_crs("epsg:4326")) %>%
    cbind(., sf::st_coordinates(.)) %>%
    dplyr::rename(lon = X, lat = Y) %>% 
    tibble::as_tibble() %>%
    dplyr::select(-geometry) %>% 
    bind_rows(aqs.annual%>%filter(datum=='WGS84')) # combined with points already recorded in epsg:4326
  
  # 3.e. keep only relevant variables 
  aqs.annual <- aqs.annual %>% 
    dplyr::select(lat, lon, date_local, obs, site_id,  obs_std, state, poc)  %>%
    distinct()
  
  # 3.f. restrict to monitors whose observations are evenly distributed across season
  # at least 3/16th of obs within each season
  # 3.f.i determine total and seasonal number of daily obs for each monitors 
  aqs.annual.check <- aqs.daily %>% 
    mutate(season = case_when(
      month(as.POSIXct(date_local, format = "%Y-%m-%d")) %in% c(12, 1, 2) ~ 'winter', 
      month(as.POSIXct(date_local, format = "%Y-%m-%d")) %in% c(3, 4, 5) ~ 'spring', 
      month(as.POSIXct(date_local, format = "%Y-%m-%d")) %in% c(6, 7, 8) ~ 'summer', 
      month(as.POSIXct(date_local, format = "%Y-%m-%d")) %in% c(9, 10, 11) ~ 'fall')) %>% 
    mutate(count_winter = if_else(season == 'winter', 1, 0), 
           count_spring = if_else(season == 'spring', 1, 0), 
           count_summer = if_else(season == 'summer', 1, 0), 
           count_fall = if_else(season == 'fall', 1, 0)) %>%
    group_by(site_id) %>% 
    summarize(count_tot = n(), 
              count_winter = sum(count_winter), 
              count_spring = sum(count_spring), 
              count_summer = sum(count_summer), 
              count_fall = sum(count_fall)) 
  # 3.f.ii. determine if each seasonal count is at least 3/16 of to total 
  # Robbie: Understood now why you did this from the manuscript thanks!
  aqs.annual.check <- aqs.annual.check %>% 
    mutate(min_seasonal_obs = (3/16) * count_tot) %>% 
    filter(count_winter >= min_seasonal_obs & 
             count_spring >= min_seasonal_obs & 
             count_summer >= min_seasonal_obs & 
             count_fall >= min_seasonal_obs)
  
  # 3.f.iii.i now filter the annual observations
  aqs.annual <- aqs.annual %>% 
    filter(site_id %in% aqs.annual.check$site_id)
  
  # 3.g set up datasets for next step 
  aqs.tot <- aqs.annual %>% 
    mutate(window = 'annual') %>% 
    bind_rows(aqs.daily%>%mutate(window='daily'))
}

#### ---------------- ####
####  4. save result  ####
#### ---------------- ####

# 4.a. save daily data
# dim are 1410064 rows x 8 col
aqs %>% 
  filter(window == 'daily') %>%
  dplyr::select(-obs_std) %>%
  rename(ref_id = site_id) %>%
  write_fst(here::here('inputs', 'pm25', 'ground_truth', 'formatted',
                             'aqs_daily_curated.fst'))

# 4.b. save annual data
# dim are 8378 rows x 8 col
aqs %>% 
  filter(window == 'annual') %>%
  rename(ref_id = site_id) %>%
  write_fst(here::here('inputs', 'pm25', 'ground_truth', 'formatted',
                       'aqs_annual_curated.fst'))

# 4.b. stop parallelizing
stopCluster(my.cluster)
