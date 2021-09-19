# Process Daily AQS Data
# Assess Daily Input Models
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Read Data 
# 2: Curate Monitors 
# 3: Save

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.rtf")
  source(here::here('scripts', 'a_set_up', "a_00_setUp_env.R"))
}

####********************
#### 1: Read Data ####
####********************

# 1a Read the aqs data 
dta <- read_csv(here::here('data_ground_truth', 'raw', 
                           'daily_88101_2010.csv'))
 
# 1b Clean up the names 
dta <- dta %>% 
  clean_names() %>% 
  rename(lat = latitude, lon = longitude, aqs = arithmetic_mean, timeStep = date_local) 
  
####************************
#### 2: Curate Monitors ####
####************************

# 2a Keep only those with observation percent greater than or equal to 75% 
dta <- dta %>% 
  filter(observation_percent >= 75)

# 2b Keep only those with a 24-hr sample duration 
dta <- dta %>% 
  filter(str_detect(sample_duration, "24"))

# 2c Fix the datums 
dta.nad <- dta %>% 
  filter(datum == 'NAD83') %>% 
  st_as_sf(., coords = c("lon", "lat"),
           crs=st_crs("epsg:4269")) %>% 
  st_transform(crs=st_crs("epsg:4326"))

dta.wgs <- dta %>% 
  filter(datum == 'WGS84') %>% 
  st_as_sf(., coords = c("lon", "lat"),
           crs=st_crs("epsg:4326")) 

dta <- dta.wgs %>%
  bind_rows(dta.nad) 

# 2d Restrict to conus
conus.4326 <- conus %>% 
  st_transform(crs=st_crs("epsg:4326"))
dta <- dta %>% 
  st_join(conus.4326, st_intersects) %>% 
  filter(!is.na(g)) %>% 
  dplyr::select(-g, -m)

# 2e Return to dataframe format 
dta <- dta %>% 
  as.data.frame() %>% 
  separate(geometry, c('lon', 'lat'), sep = ',') %>% 
  mutate(lon = str_sub(lon, 3), 
         lat = str_sub(lat, 0, -2))


# 2c Keep only variables of interest 
dta <- dta %>% 
  dplyr::select(lat, lon, aqs, time, site_num)

a <- dta %>% dplyr::select(lat, lon, site_num) %>% distinct() %>% 
  group_by(site_num) %>% summarize(count = n())

# what does Lawrence do if there are two observations per site?
# 2e Save results
dta %>% 
  write_csv(here::here('data_ground_truth', 'formatted',  
                       'daily_aqs_cleaned.csv'))
