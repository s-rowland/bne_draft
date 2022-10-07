

# Robbie: A brief description of the script would go well here
# Sebastian: Added

# File: STR_b_03_retrieve_js.R
# Author: Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 09/22/2022
#
# Contents:
#  N. Notes
#  0. Import Packages and Global Objects
#  1. DOWNLOAD JS

#### ------------------- ####
####       N. NOTES      ####
#### ------------------- ####

# N.1. goal 
# the goal of this function to download js data 

#### ------------------------------------------- ####
####  0. IMPORT PACKAGES AND SET GLOBAL OBJECTS  ####
#### ------------------------------------------- ####

# 0.a. import packages and global objects, if you haven't already done so
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

# 0.b minor function to add leading zeros to single digits
pad0 <- function(x){ stringr::str_pad(x, 2, 'left', '0')}

#### --------------- ####
#### 1. DOWNLOAD JS  ####
#### --------------- ####

# Robbie: Having seen this routine a couple of times in these scripts might it be worth having a source file for the below and elsewhere throughout?
# Sebastian: I agree it would be a nice feature, but not a priority for now.

# 1.a set up parallelization
# 1.a.i get the number of cores
# we subtract one to reserve a core for non-lbic tasks
n.cores <- parallel::detectCores() - 1
# 1.a.ii create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "FORK"
)
# 1.a check cluster definition (optional)
print(my.cluster)
# 1.a.iii register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
#check if it is registered (optional)
foreach::getDoParRegistered()

# 1.b set string objects used to download data
# Robbie: A brief description here and for all of the below would be useful
# Sebastian: Added 

# prefix is the main url where we download the data, 
# plus the unchanging start of the file name
prefix <- "https://beta.sedac.ciesin.columbia.edu/downloads/data/aqdh/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016-"
# suffix is the unchanging ending of the file name
suffix <- "-rds.zip"
# wd is the directory to which we will write the js data
wd <- paste0(here::here('inputs', 'pm25', 'base_models', 'daily', 'raw', 'js'), '/')

# 1.c create table of combinations of year and month for us to loop over
# infix is the changing component of the file name - the year and month of the data
time.tab <- tidyr::expand_grid(yyyy = c(2005, 2004), 
                        mm=  pad0(1:12)) %>%
 dplyr:: mutate(infix = paste0(yyyy, mm))

# 1.d foreach loop to download
Sys.time()
# 1.d.i establish foreach parameters
ee <- foreach(
  
  infix = time.tab$infix, 
  .combine = 'rbind'
  
) %do% {
 
  # 1.d.ii download js if not present in local directory
  if  ( !(infix %in% list.files(wd))) {
    
    # 1.d.iii name of monthly file
    url <- paste0(prefix, infix, suffix)
    zipfile <- paste0(wd, infix, ".zip")
    download <- paste0("curl -o ", zipfile, " -b ~/.urs_cookies -c ~/.urs_cookies -L -n ", url)
    # 1.d.iv do the download
    system(download)
    # 1.d.v unzip the file
    unzip(zipfile, exdir = paste0(wd, infix))
  }
  
  # 1.d.vi show tracker
  g = infix
  g
}

# 1.e. end parallelization
stopCluster(my.cluster)
