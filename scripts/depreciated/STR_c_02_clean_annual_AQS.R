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
# Note: LGC only uses daily AQS data to make daily and annual training datasets

read_aqs <- function(YYYY, raw){
  # YYYY <- 2012; raw = 'raw'
  
  # set filename
  if (raw == 'raw') {fileName <- paste0('annual_conc_by_monitor_', YYYY, '.csv')}
  #if (raw == 'raw') {fileName <- paste0('daily_88101_', YYYY, '.csv')}
  if (raw == 'formatted') {fileName <- 'lgc_annual_data_2000-2016.csv'}
  
  # readin file
  dta <-  read_csv(here::here('BNE_inputs', 'ground_truth', raw, fileName)) %>% 
    janitor::clean_names() %>% 
    dplyr::rename(lat = latitude, lon = longitude) 
  
  if (raw == 'raw') {
    dta <- dta %>% 
      filter(!state_name %in% excludedAreas) %>% 
      mutate(year = YYYY) %>% 
      filter(lat > 18)
  }
  
  # fix datums
  dta2 <- dta %>% 
    filter(datum == 'NAD83') %>% 
    st_as_sf(coords=c('lat', 'lon'), crs= 4269) %>% 
    st_transform(crs= 4326) 
  
  dta <- dta2 %>% 
    mutate(lat = st_coordinates(dta2)[,1], 
           lon = st_coordinates(dta2)[,2]) %>% 
    as.data.frame() %>% 
    dplyr::select(-geometry) %>% 
    bind_rows(filter(dta, datum != 'NAD83')) %>% 
    #dplyr::select(lat, lon, year) %>% 
    #distinct() %>% 
    mutate(raw = raw)
  
  # get only measurements of interest
  dta <- dta %>% 
    filter(parameter_code == 88101) %>% 
    filter(sample_duration == '24 Hour')
  
  dta <- dta %>% 
    group_by(lat, lon, year) %>% 
    arrange(desc(observation_percent)) %>%
    slice_sample(n=1) %>% 
    ungroup()
  
  dta <- dta %>% 
    filter(observation_percent >= 75)
  
}

####*************************
#### 2: Create AQS Plots ####
####*************************



# 2b keep only pm2.5 measurements with gold standard instrument



# 2c get one monitor per location per year 
aqs.raw <- aqs.raw %>% 
  group_by(lat, lon, year) %>% 
  arrange(desc(observation_percent)) %>%
  slice_sample(n=1) %>% 
  ungroup()
