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
# read in training data
train <- read_csv(here::here('inputs', 'pm25', 'training_datasets', 'daily_yearly', 
                             'training_avcmjsme_2010_all.csv'))

ls(train)

# create safe hex look up 
safe_hex_lookup <- read_rds(here::here('ancillary_data', 'raw', 'brokamp_objects', 
                                       'safe_hex_lookup.rds'))
# okay next drop training locations without a safe hex 
# 1.b.i get list of safe hex 
cbHexList <- list.files("~/Desktop/pm25-brokamp/")
train <- train %>% 
  mutate(h3 = suppressMessages(h3jsr::point_to_h3(dplyr::select(., lon, lat), res = 8))) %>%
  mutate(h3_3 = h3jsr::get_parent(h3, res = 3)) %>%
  mutate(year = 2010) %>%
  dplyr::left_join(safe_hex_lookup, by = 'h3_3') %>%
  dplyr::mutate(h3_3 = ifelse(!is.na(safe_hex), safe_hex, h3_3)) %>%
  dplyr::select(-safe_hex) %>% 
  mutate(file_name = paste0( h3_3, "_", year, "_h3pm.fst"))

# keep only training locations with match 
train.safeHex <- train %>% 
  filter(file_name %in% cbHexList)

train.list <- split(train.safeHex, train.safeHex$file_name)



dta <- train.list[1]
dta <- dta %>%
  rename(id = ref_id) %>% 
  mutate(start_date = paste0(year, '-', month, '-', day), 
         end_date = paste0(year, '-', month, '-', day))

# okay, we lose ~1300 training points.  about 1% of data 

pred.loc <- pred.loc %>% 
  dplyr::select(ref_id, ref_lat, ref_lon) %>% 
  rename(id = ref_id, lat = ref_lat, lon = ref_lon) %>% 
  mutate(start_date = '2010-01-01', 
         end_date = '2010-01-01') %>% 
  mutate(id2 = row_number() %% 100)

pred.loc.list <- split(pred.loc,  pred.loc$id)




