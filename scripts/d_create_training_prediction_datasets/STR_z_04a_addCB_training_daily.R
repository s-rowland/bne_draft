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
#### 1. prepare data ####
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
# 1.c.i. get list of safe hex 
cbHexList <- list.files("~/Desktop/pm25-brokamp/")
# 1.c.ii.wrangle the data; rename variables and filter
train <- train %>% 
  mutate(h3 = suppressMessages(h3jsr::point_to_h3(dplyr::select(., lon, lat), res = 8))) %>%
  mutate(h3_3 = h3jsr::get_parent(h3, res = 3)) %>%
  mutate(year = 2010) %>%
  dplyr::left_join(safe_hex_lookup, by = 'h3_3') %>%
  dplyr::mutate(h3_3 = ifelse(!is.na(safe_hex), safe_hex, h3_3)) %>%
  dplyr::select(-safe_hex) %>% 
  mutate(file_name = paste0( h3_3, "_", year, "_h3pm.fst")) %>%
  rename(id = ref_id) %>% 
  mutate(date = paste0(year, '-', month, '-', day))

# 1.d. keep only training locations within a safe hex
train.safeHex <- train %>% 
  filter(file_name %in% cbHexList)

##** ------------------------------------- **##
#### 2. add cb to training data ####
#### ------------------------------------- ####

# 2.a. split data according to file name
train.list <- split(train.safeHex, train.safeHex$file_name)


# 2.b. define function to assign
assign_cb <- function(train.oneHex) {
  read_fst(paste0("~/Desktop/pm25-brokamp/", train.oneHex$file_name[1])) %>% 
    mutate(date = as.character(date)) %>%
    filter(date %in% train.oneHex$date) %>% 
    inner_join(train.oneHex, by = c('date', 'h3'))
}

# 2.c seed loop
train.list.sm <- train.list[1:3]
train.cb <- purrr::map_dfr(train.list.sm , assign_cb)

for (i in 89:92) {
  # extract all the training points within the active h3_3
  iStart <- 1 + 3*i
  iEnd <- 3 + 3*i
  train.list.sm <- train.list[iStart: iEnd]
  train.cb <- purrr::map_dfr(train.list.sm , assign_cb )%>% 
    bind_rows(train.cb)
  print(i)
}



a <- train.safeHex %>% 
  dplyr::select(file_name) %>% 
  distinct() 

a.cb <- train.cb %>% 
  dplyr::select(file_name) %>% 
  distinct() 
b <- a %>% anti_join(a.cb, by = 'file_name')

train2 <- train.safeHex %>% filter(file_name %in% b$file_name)
train.list2 <- split(train2, train2$file_name)
train.cb <- purrr::map_dfr(train.list2, assign_cb )%>% 
  bind_rows(train.cb)

train.cb %>% 
  rename(pred_cb = pm_pred, 
         ref_id = id) %>% 
  dplyr::select(lat, lon, day_index, year, month, day, obs_pm25, 
                pred_av, pred_cm, pred_js, pred_me,pred_cb, ref_id) %>% 
  readr::write_csv(here::here('inputs', 'pm25', 'training_datasets', 'daily_yearly', 
                              paste0('training_avcmjsmecb_', 2010, '_all.csv')))



