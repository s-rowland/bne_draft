# File: LGC_d_02a_make_training_data.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 08/21/21
#
# Contents:
#  N. notes 
#  0. import packages and set global objects
#  1. link H3 to prediction dataset locations


#### ---------- ####
####  N. notes  ####
#### ---------- ####

# this code is based on the getPM code from the geomarker repository, 
# and I (Sebastian) do not completely understand all of the h3 functions 

#### ------------------------------------------- ####
####  0. IMPORT PACKAGES AND SET GLOBAL OBJECTS  ####
#### ------------------------------------------- ####

# 0.a. import packages and global objects, if you haven't already done so
if(!exists("ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', 
                    "a_00_import_packages_set_global_objects.R"))
}

# 0.b. install Cole's package 
#p_load(remotes)
#remotes::install_github("geomarker-io/addPmData")
library(addPmData)

#### -------------------------------------------- ####
####  1. link H3 to prediction dataset locations  ####
#### -------------------------------------------- ####

# 1a establish refGrid 
refGridConus <- fst::read_fst(here::here('inputs', 'pm25', 'reference_grids',  
                                         paste0('refGrid_', 'conus', '.fst')))

# 1b. breka up refGrid in preparation for foreach 
refGridConus <- refGridConus %>% 
  mutate(group = row_number() %% 20)


d <- tibble::tribble(
  ~id,         ~lat,    ~lon, ~start_date,    ~end_date,
  '55000100280', 39.2, -84.6, '2008-09-09', '2008-09-11',
  '55000100281', 39.2, -84.6, '2007-08-05', '2007-08-08',
  '55000100282', 39.2, -84.6, '2015-08-31', '2015-09-02')

add_pm(d)


# 1.a. create safe hex look up object 
safe_hex_lookup <- read_rds(here::here('ancillary_data', 'raw', 'brokamp_objects', 
                                       'safe_hex_lookup.rds'))

# 1.b wrangle prediction locations
pred.loc <- read_fst(here::here('inputs', 'pm25', 'keys',
                                'refGridConus_js_key_nn_daily.fst')) %>% 
  dplyr::select(ref_id, ref_lat, ref_lon) %>% 
  rename(id = ref_id, lat = ref_lat, lon = ref_lon) 

# 1.c get the h3_3 locations
pred.loc.h3 <- pred.loc %>% 
  dplyr::select(lon, lat) %>%
  mutate(h3 = suppressMessages(h3jsr::point_to_h3(dplyr::select(., lon, lat), res = 8))) %>%
  mutate(h3_3 = h3jsr::get_parent(h3, res = 3)) %>%
  mutate(year = 2010) #%>% 
  
# 1.d some of the h3_3 id's need to be replaced 
pred.loc.h3 <- pred.loc.h3 %>%
  dplyr::left_join(safe_hex_lookup, by = 'h3_3') %>%
  dplyr::mutate(h3_3 = ifelse(!is.na(safe_hex), safe_hex, h3_3)) %>%
  dplyr::select(-safe_hex)
  
# 1.e create names of the files we want to read in 
pred.loc.h3_3 <- pred.loc.h3 %>% 
  dplyr::select(h3_3) %>% 
  distinct() %>% 
  mutate(id = row_number())
  
# test
#dta <- s3::s3_get_files(pred.loc.list[[1]]$file_name, 
 #                     confirm = FALSE, 
  #                    download_folder = here::here('inputs', 'pm25', 'base_models', 
   #                                                                 'raw'))

#### ------------------------------------------ ####
####  2. download CB by h3_3 - year combination ####
#### ------------------------------------------ ####

# 2.a. loop over the unique h3_# we need to get
for (i in 1:nrow(pred.loc.h3_3)){
  
  dta.pm <- downloadCB(pred.loc.h3_3$h3_3[i], 2010)
  
  if (i %% 10 == 0) {print(i)}
}

# check 
# a <- read_fst(paste0("~/Desktop/pm25-brokamp/832806fffffffff_2010_h3pm.fst"))
