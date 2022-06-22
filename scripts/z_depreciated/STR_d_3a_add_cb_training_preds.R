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

# 0.b set up parallelization
# 0.b.i get the number of cores
# we subtract one to reserve a core for non-lbic tasks
n.cores <-  parallel::detectCores() - 2
# 0.b.ii create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, type = 'FORK'
)
#check cluster definition (optional)
print(my.cluster)

# 0.b.iii register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
#check if it is registered (optional)
foreach::getDoParRegistered()

# 0.c. install Cole's package 
#p_load(remotes)
#remotes::install_github("geomarker-io/addPmData")
library(addPmData)

##** ------------------------------------- **##
#### 1. prepare data ####
#### ------------------------------------- ####

# 1.a. read in training data
train <- read_fst(here::here('inputs', 'pm25', 'training_datasets', 'daily_yearly', 
                             'training_avcmjsme_2010.fst'))

ls(train)

# 1.b. create safe hex look up 
safe_hex_lookup <- read_rds(here::here('ancillary_data', 'raw', 'brokamp_objects', 
                                       'safe_hex_lookup.rds'))

# okay next drop training locations without a safe hex 
# 1.c.i. get list of safe hex 
cbHexList <- list.files(here::here('inputs', 'pm25', 'base_models', 'daily', 'pm25-brokamp'))
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
  read_fst(here::here('inputs', 'pm25', 'base_models', 'daily', 'pm25-brokamp',
                      train.oneHex$file_name[1])) %>% 
    mutate(date = as.character(date)) %>%
    filter(date %in% train.oneHex$date) %>% 
    rename(pred_cb = pm_pred) %>%
    inner_join(train.oneHex, by = c('date', 'h3')) %>% 
    dplyr::select(lat, lon, id, date, obs, starts_with('pred_'))
}

# 2.c. foreach paralelized loop
Sys.time()
training.full <- foreach(
  trainingHexes = train.list, 
  .combine = 'rbind'
) %dopar% {
  assign_cb(trainingHexes)
}
Sys.time()

# 2.d. save results
training.full %>% 
  write_fst(here::here('inputs', 'pm25', 'training_datasets', 'daily_yearly', 
                       'training_avcmjsmecb_2010.fst'))

