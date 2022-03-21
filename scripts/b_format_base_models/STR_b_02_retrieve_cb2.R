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

# 0.e set up parallelization
# 0.e.i get the number of cores
# we subtract one to reserve a core for non-lbic tasks
n.cores <- parallel::detectCores() - 1
# 0.e.ii create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "FORK"
)
#check cluster definition (optional)
print(my.cluster)

# 0.e.iii register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
#check if it is registered (optional)
foreach::getDoParRegistered()

#### -------------------------------------------- ####
####  1. link H3 to prediction dataset locations  ####
#### -------------------------------------------- ####

# 1a establish refGrid 
refGridConus <- fst::read_fst(here::here('inputs', 'pm25', 'reference_grids',  
                                         paste0('refGrid_', 'conus', '.fst')))

# 1b. breka up refGrid in preparation for foreach 
refGridConus <- refGridConus %>% 
  mutate(group = row_number() %% 1000, 
         id = row_number())

refGridConus.list <- split(refGridConus, refGridConus$group)

ee <- foreach(
  dta = refGridConus.list, 
  .combine = 'rbind'
) %dopar% {
  

  dta2 <- data.frame(id = dta$id, 
                     lat = dta$lat, 
                     lon = dta$lon, 
                     start_date = '2005-01-01', 
                     end_date = '2005-01-01')

dta2 <- add_pm(dta2) 


}



