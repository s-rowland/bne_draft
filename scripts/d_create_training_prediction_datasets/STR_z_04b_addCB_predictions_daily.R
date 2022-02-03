# File: LGC_d_02a_make_training_data.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 08/21/21
#
# Contents:
# 0. Package Imports & Global Variables
# 1. EPA & JS Daily Data
# 2. Setting Up CMAQ, GS, CACES loop
# 3. CMAQ, GS, CACES loop
# 4. AV data loop
# 5. Save outputs

##** ------------------------------------- **##
#### 0. PACKAGE IMPORTS & GLOBAL VARIABLES ####
#### ------------------------------------- ####

# 0a. load packages and functions required for this script
if(!exists("ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', 
                    "a_00_import_packages_set_global_objects.R"))
}

# 0.b. install Cole's package 
#p_load(remotes)
#remotes::install_github("geomarker-io/addPmData")
library(addPmData)

##** ------------------------------------- **##
#### 0. Do it ####
#### ------------------------------------- ####
# 
# make list of hex locations 

cbHexList <- list.files("~/Desktop/pm25-brokamp/")
# get safe hex look up 
safe_hex_lookup <- read_rds(here::here('ancillary_data', 'raw', 'brokamp_objects', 
                                       'safe_hex_lookup.rds'))

dta <- read_csv(here::here('inputs', 'pm25','prediction_datasets', 'daily_individual',
                           'prediction_avcmjs_2010_000_all.csv'))

dta <- dta %>% 
  mutate(h3 = suppressMessages(h3jsr::point_to_h3(dplyr::select(., lon, lat), res = 8))) %>%
  mutate(h3_3 = h3jsr::get_parent(h3, res = 3)) %>%
  mutate(file_name = paste0(h3_3, "_", 2010, "_h3pm.fst")) %>%
  mutate(id = row_number()) 
  
h3.list <- dta %>% 
  filter(file_name %in% cbHexList) %>%
  dplyr::select(h3) %>% 
  distinct()

i <- 1
activeH3 <- h3.list$h3[i]

dta.activeH3 <- dta %>%
  filter(h3 == activeH3)

head(dta.activeH3$id)


# bring in cb data 
cb <- read_fst(here::here('inputs', 'pm25', 'base_models', 'raw', 'cb_daily_raw', 
                          paste0(activeH3, '_2010_h3pm.fst')))
# begin function

d <- 1 




# read in training locations
pred.loc <- read_fst(here::here('inputs', 'pm25', 'keys',
                                 'refGridConus_js_key_nn_daily.fst'))

# create safe hex look up 
safe_hex_lookup <- read_rds(here::here('ancillary_data', 'raw', 'brokamp_objects', 
                                       'safe_hex_lookup.rds'))
# okay next drop training locations without a safe hex 
# 1.b.i get list of safe hex 
cbHexList <- list.files("~/Desktop/pm25-brokamp/")
pred.loc <- pred.loc %>% 
  rename(lat = ref_lat, lon = ref_lon) %>%
  mutate(h3 = suppressMessages(h3jsr::point_to_h3(dplyr::select(., lon, lat), res = 8))) %>%
  mutate(h3_3 = h3jsr::get_parent(h3, res = 3)) %>%
  mutate(year = 2010) %>%
  dplyr::left_join(safe_hex_lookup, by = 'h3_3') %>%
  dplyr::mutate(h3_3 = ifelse(!is.na(safe_hex), safe_hex, h3_3)) %>%
  dplyr::select(-safe_hex) %>% 
  mutate(file_name = paste0( h3_3, "_", year, "_h3pm.fst"))

# keep only training locations with match 
pred.loc.cb <- pred.loc %>% 
  filter(file_name %in% cbHexList)

# okay, we lose 28 training sites 
pred.loc <- pred.loc %>% 
  dplyr::select(ref_id, ref_lat, ref_lon) %>% 
  rename(id = ref_id, lat = ref_lat, lon = ref_lon) %>% 
  mutate(start_date = '2010-01-01', 
         end_date = '2015-12-31') %>% 
  mutate(id2 = row_number() %% 100)

pred.loc.list <- split(pred.loc,  pred.loc$id)