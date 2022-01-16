# File: STR_b_03_get_cb_preds.R
# Author: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 1/7/2022
#
# Contents:
# N. Notes
# 0. Configure Environment
# 1. 

#### ------------------- ####
####       N. NOTES      ####
#### ------------------- ####



#### -------------------------- ####
####  0. CONFIGURE ENVIRONMENT  ####
#### -------------------------- ####

# 0.a. configure environment
if(!exists("ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_config_env.R"))
}

# 0.b. install Cole's package 
#p_load(remotes)
#remotes::install_github("geomarker-io/addPmData")
library(addPmData)

#--------------------------------#
#### 1. FUNCTION TO AGGREGATE ####
#--------------------------------#


pred.dataset <- readr::read_csv(here::here('BNE_inputs', 'prediction_datasets', 'combined_daily', 
                                           'dailyPredictionsData_2010-2015',
                                           paste0(2010, '-', '01', 
                                                  '-', '01', '.csv')))

pred.loc <- pred.dataset %>% 
  dplyr::select(js_ref_id, lat, lon) %>% 
  rename(id = js_ref_id) %>% 
  mutate(start_date = '2010-01-01', 
         end_date = '2015-12-31')

pred.loc.list <- split(pred.loc,  pred.loc$id)

d <- tibble::tribble(
  ~id,         ~lat,    ~lon, ~start_date,    ~end_date,
  '55000100280', 39.2, -84.6, '2008-09-09', '2008-09-11',
  '55000100281', 39.2, -84.6, '2007-08-05', '2007-08-08',
  '55000100282', 39.2, -84.6, '2015-08-31', '2015-09-02')

d1 <- bind_rows(pred.loc.list[[1000]], pred.loc.list[[20000]])

d1$lat[1] <- d$lat[1]
d1$lon[1] <- d$lon[1]
# here our goal is just to download all the requred datasets. 

# okay, so there seems to be a problem with my lat and lon



pred.loc <- read_fst('~/Desktop/BNE/bne_draft/BNE_inputs/keys/refGridConus_js_key_nn_daily.fst')
pred.loc <- pred.loc %>% 
  dplyr::select(ref_id, ref_lat, ref_lon) %>% 
  rename(id = ref_id, lat = ref_lat, lon = ref_lon) %>% 
  mutate(start_date = '2010-01-01', 
         end_date = '2015-12-31')
pred.loc.list <- split(pred.loc,  pred.loc$id)

a <- add_pm(pred.loc.list[[1]])
a2 <- add_pm(pred.loc.list[[2]])


for ( i in 1:10){
  a <- add_pm(pred.loc.list[[i]])
}


yy