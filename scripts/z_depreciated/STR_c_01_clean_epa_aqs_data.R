# File: LGC_c_01_clean_epa_aqs_data.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 12/23/20 (reformatted 9/27/21)

# Contents:
# N. Notes
# 0. Package Imports
# 1. Load daily data
# 2. Load annual data
# 3. Hash tables for annual data
# 4. Hash tables for seasonal data
# 5. Generate datasets
# 6. Save & log all data

#----------------#
#### N. NOTES ####
#----------------#



#--------------------------#
#### 0. PACKAGE IMPORTS ####
#--------------------------#

# 0a. Load packages and functions required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('code', '0_00_set_up_env.R'))
}

#---------------------------#
#### 1: LOAD ANNUAL DATA ####
#---------------------------#

# 1a. read the aqs data 
dta <- read_csv(here::here('data', 'ground_truth', 'raw', 
                           'annual_conc_by_monitor_2011.csv'))

# 1b. clean up the names 
dta <- dta %>% 
  janitor::clean_names() %>% 
  rename(lat = latitude, lon = longitude, aqs = arithmetic_mean) 

#--------------------------#
#### 2: CURATE MONITORS ####
#--------------------------#

# 2a. keep only the monitors with observation percent greater than or equal to 75% 
dta <- dta %>% 
  dplyr::filter(observation_percent >= 75)

# 2b. keep only the monitors with a 24-hr sample duration 
dta <- dta %>% 
  filter(str_detect(sample_duration, "24"))

# 2c. harmonize datums 
dta.nad <- dta %>% 
  filter(datum == 'NAD83') %>% 
  st_as_sf(., coords = c("lon", "lat"), crs=st_crs("epsg:4269")) %>% 
  st_transform(crs=st_crs("epsg:4326"))

dta.wgs <- dta %>% 
  filter(datum == 'WGS84') %>% 
  st_as_sf(., coords = c("lon", "lat"), crs=st_crs("epsg:4326")) 

dta <- dta.wgs %>%
  bind_rows(dta.nad)

dta <- dta %>% 
  mutate(lon = st_coordinates(dta)[,1], 
         lat = st_coordinates(dta)[,2]) %>%
  as.data.frame()
  
  dta <- dta %>% 
    st_as_sf(., coords = c("lon", "lat"), crs=st_crs("epsg:4326")) 
  
  plot(dta)
  