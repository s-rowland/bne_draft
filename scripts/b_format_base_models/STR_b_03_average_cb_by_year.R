# File: LGC_d_02a_make_training_data.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 08/21/21
#
# Contents:
#  0. import packages and set global objects
#  1. average cb

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
# Robbie: again just a brief description of Cole's package would be useful here
library(addPmData)

#### --------------- ####
####  1. average cb  ####
#### --------------- ####

# 1.a. get list of available files
# Robbie: Empty here for me so not run
cbH3file.list <- list.files(here::here('inputs', 'pm25', 'base_models', 
                                       'raw', 'pm25-brokamp'))

# 1.b. loop over the H3
for (i in 1:length(cbH3file.list)) {
  
  # 1.b.i. bring in the year-and-H3-specific prediction file
  # h3_8 refers to the correct level of the hexagon
  cb.h3_8.daily <- read_fst(here::here('inputs', 'pm25', 'base_models', 
                           'raw', 'pm25-brokamp',cbH3file.list[i]))  %>% 
    rename(h3_8=h3)
  
  # 1.b.ii. compute annual mean within each child hexagon
  cb.h3_8.annual <- cb.h3_8.daily %>% 
    group_by(h3_8) %>% 
    summarize(pred_cb = mean(pm_pred))
  
  # 1.b.iii. save result
  cb.h3_8.annual %>% 
    write_fst(here::here('inputs', 'pm25', 'base_models', 'formatted', 'cb_annual_formatted', 
                       cbH3file.list[i]))
  
  # 1.b.iv. compute monthly means within each child hexagon
  cb.h3_8.monthly <- cb.h3_8.daily %>% 
    mutate(month = stringr::str_sub(date, 6, 7)) %>%
    group_by(h3_8, month) %>% 
    summarize(pred_cb = mean(pm_pred))
  
  # 1.b.v. save
  dta.m %>% 
    write_fst(here::here('inputs', 'pm25', 'base_models', 'formatted', 'cb_monthly_formatted', 
                       cbH3file.list[i]))
 
   # 1.b.vi. report progress
print(i)
}



