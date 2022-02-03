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

conus <- sf::st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                                'conus.shp')) %>% 
  sf::st_transform(., crs=st_crs('epsg:4326'))

##** ------------------------------------- **##
#### 1. prepare data ####
#### ------------------------------------- ####
# 
# create safe hex look up 
safe_hex_lookup <- read_rds(here::here('ancillary_data', 'raw', 'brokamp_objects', 
                                       'safe_hex_lookup.rds'))
# okay next drop training locations without a safe hex 
# 1.c.i. get list of safe hex 
cbHexList <- list.files(here::here('inputs', 'pm25', 'base_models', 'raw', 'pm25-brokamp'))

# read in training data
preds <- read_csv(here::here('inputs', 'pm25', 'prediction_datasets', 'annual_individual', 
                             'predictions_jsavgscmccme_2010_conus.csv'))

# 1.c.ii.wrangle the data; rename variables and filter
preds <- preds %>% 
  mutate(h3 = suppressMessages(h3jsr::point_to_h3(dplyr::select(., lon, lat), res = 8))) %>%
  mutate(h3_3 = h3jsr::get_parent(h3, res = 3)) %>%
  mutate(year = 2010) %>%
  dplyr::left_join(safe_hex_lookup, by = 'h3_3') %>%
  dplyr::mutate(h3_3 = ifelse(!is.na(safe_hex), safe_hex, h3_3)) %>%
  dplyr::select(-safe_hex) %>% 
  mutate(file_name = paste0(h3_3, "_", 2010, "_h3pm.fst")) 

# 1.d. keep only training locations within a safe hex
preds.safeHex <- preds %>% 
  filter(file_name %in% cbHexList)


##** ------------------------------------- **##
#### 3. Plot Missing Data ####
#### ------------------------------------- ####

# 2.a. split data according to file name
pred.list <- split(preds.safeHex, preds.safeHex$file_name)
pred.oneHex <- pred.list[[1]]

# 2.b. define function to assign
assign_cb <- function(pred.oneHex) {
  
  read_fst(here::here('inputs', 'pm25', 'base_models', 'formatted', 
                            'cb_annual_formatted',  pred.oneHex$file_name[1])) %>% 
    inner_join(pred.oneHex, by = 'h3_3')
}

# 2.c map it 
pred.cb <- purrr::map_dfr(pred.list, assign_cb)


# plot it 
png(here::here('str_testing', 'outputs', 'test_cb', 'cb_annual_average.png'))
plotOneParameterSpatial(dta = pred.cb, parameterName = 'pred_cb', 
                        borderObj = conus, valueScale = c(0, 5, 10, 15, 20, 24),
                        mainTitle = "Annual Average CB predictions")
dev.off()



png(here::here('str_testing', 'outputs', 'test_cb', 'cm_annual_average.png'))
plotOneParameterSpatial(dta = preds, parameterName = 'pred_cm', 
                        borderObj = conus, valueScale = c(0, 5, 10, 15, 20, 24),
                        mainTitle = "Annual Average CMAQ predictions")
dev.off()

png(here::here('str_testing', 'outputs', 'test_cb', 'av_annual_average.png'))
plotOneParameterSpatial(dta = preds, parameterName = 'pred_av', 
                        borderObj = conus, valueScale = c(0, 5, 10, 15, 20, 24),
                        mainTitle = "Annual Average AV predictions")
dev.off()

png(here::here('str_testing', 'outputs', 'test_cb', 'js_annual_average.png'))
plotOneParameterSpatial(dta = preds, parameterName = 'pred_js', 
                        borderObj = conus, valueScale = c(0, 5, 10, 15, 20, 24),
                        mainTitle = "Annual Average JS predictions")
dev.off()

png(here::here('str_testing', 'outputs', 'test_cb', 'me_annual_average.png'))
plotOneParameterSpatial(dta = preds, parameterName = 'pred_me', 
                        borderObj = conus, valueScale = c(0, 5, 10, 15, 20, 24),
                        mainTitle = "Annual Average MERRA predictions")
dev.off()

##** ------------------------------------- **##
#### 3. Plot Missing Data ####
#### ------------------------------------- ####


preds.noCB <- preds %>% 
  filter(!(file_name %in% cbHexList)) %>% 
  mutate(Missing = 1)

conus <- sf::st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                                'conus.shp')) %>% 
  sf::st_transform(., crs=st_crs('epsg:4326'))

aqs <- read_csv(here::here('inputs', 'pm25', 'training_datasets', 
                          'annual_individual',  'training_avgscmjsccme_2010_all.csv'))

refCRS <- projCRS
png(here::here('str_testing', 'outputs', 'test_cb', 'cb_missing.png'))
plotOneParameterSpatial(dta = preds.noCB, parameterName = 'Missing', 
                        borderObj = conus, extraPointObj = aqs, 
                        mainTitle = "Locations Missing CB Predictions", 
                        legYN = 'legN')
dev.off()





# 2.a. split data according to file name
pred.list <- split(preds.safeHex, preds.safeHex $file_name)


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



