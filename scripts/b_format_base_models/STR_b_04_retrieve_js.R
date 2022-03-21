# File: LGC_d_01_make_training_predictions_JS.R
# Authors:
# Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 9/03/21
#
# Contents:
# N. Notes
# 0. Package Imports
# 1. General Setup
# 2. Process JS by month
# 3. Download ZIP
# 4. Unzip Files
# 5. Process JS day by day
# 6. Join w/EPA training data
# 7. Get Ref Grid for predictions data
# 8. Save that month's work
# 9. Delete ZIP and raw JS data

#### ------------------ ####
####       N. NOTES     ####
#### ------------------ ####
# Before running this script, you need to set up two files as described in this tutorial:
# https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget
# 
# (1) ~/.netrc, which should contain only the following on the first line: 
#     "machine urs.earthdata.nasa.gov login [uid_goes_here] password [password_goes_here]"
# (2) ~/.urs_cookies, which can be empty.
#
# Once that is done, this script can be run. The point of this script is to extract
# relevant PM2.5 predictions from the JS model to finish creating the daily training data,
# as well as to create a reference grid for the predictions dataset, based off a random 1% of the JS data...
#
# To find the JS data, visit:
# https://beta.sedac.ciesin.columbia.edu/data/set/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016/data-download#close


https://beta.sedac.ciesin.columbia.edu/downloads/data/aqdh/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016-200001-geotiff.zip

https://beta.sedac.ciesin.columbia.edu/downloads/data/aqdh/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016-200001-rds.zip
#### ------------------ ####
#### 0. PACKAGE IMPORTS ####
#### ------------------ ####
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

#### ------------------ ####
####  1. GENERAL SETUP  ####
#### ------------------ ####

# 1.a. specify the years and months we are interested in:
years <- c(2010:2016)
months <- stringr::str_pad(1:12, 2, "left", "0")
timeSteps <- expand.grid(list("year" = years, "month" = months)) %>%
  dplyr::arrange(year, month)

# 1.b. define the link to download the data & directory into which we will 
# download the data:
prefix <- "https://beta.sedac.ciesin.columbia.edu/downloads/data/aqdh/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016-"
prefix <- "https://beta.sedac.ciesin.columbia.edu/downloads/data/aqdh/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016-200001-rds.zip"
suffix <- "-rds.zip"
wd <- "~/Downloads/"
## test value
wd <- paste0(here::here('inputs', 'pm25', 'base_models', 'daily', 'js'), '/')

# 1.c. establish progress bar:
n <- as.numeric(as.Date("2017-01-01") - as.Date("2015-01-01"))
progressBar <- txtProgressBar(min=0, max=n, width=50, style=3)
counter <- 0
pb <- function(p) { setTxtProgressBar(progressBar, p); return(p+1) }
counter <- pb(counter)


i <- 1 
#### --------------- ####
#### 3. DOWNLOAD ZIP ####
#### --------------- ####
# daily data has been zipped into monthly zip files
infix <- paste0(timeSteps$year[i], timeSteps$month[i])
url <- paste0(prefix, infix, suffix)
zipfile <- paste0(wd, infix, ".zip")

# bash command as a string
download <- paste0("curl -o ", zipfile, " -b ~/.urs_cookies -c ~/.urs_cookies -L -n ", url)
# run the bash command
system(download)

# 4a. unzip the month's folder
unzip(zipfile, exdir = paste0(wd, infix))









# read in EPA data:
#epaPath <- "~/Documents/Research_Marianthi/BNE_project/EPA_data/latest_version_clean_daily_data/daily_data_2000-2016.csv"
#epa <- loadData(epaPath, "EPA")

aqs <- read_csv(here::here('inputs', 'pm25', 'ground_truth', 'formatted', 
                           'lgc_daily_data_2000-2016_conus.csv'))

# read in JS files:

# # str version 
key.AQS.JS <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                  'aqs_js_key_nn_daily.fst'))
key.refGridConus.JS <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                  'refGridConus_js_key_nn_daily.fst'))
refGridConus <- key.refGridConus.JS  %>% 
  dplyr::select(ref_lat, ref_lon)

key.refGridNYS.JS <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                           'refGridNYS_js_key_nn_daily.fst'))
refGridNYS <- key.refGridNYS.JS  %>% 
  dplyr::select(ref_lat, ref_lon)

key.refGridCities.JS <- read_fst(here::here('inputs', 'pm25', 'keys', 
                                           'refGridCities_js_key_nn_daily.fst'))
refGridCities <- key.refGridCities.JS  %>% 
  dplyr::select(ref_lat, ref_lon)


#key.AQS.JS <- loadData("~/Desktop/epa-js_nn_key.csv", "JSEPAKEY")
#key.refGrid.JS <- loadData("~/Desktop/pred_jss_ref_grid.csv", "JSREF")

# progress bar:
n <- as.numeric(as.Date("2017-01-01") - as.Date("2010-01-01"))
progressBar <- txtProgressBar(min=0, max=n, width=50, style=3)
counter <- 0
pb <- function(p) { setTxtProgressBar(progressBar, p); return(p+1) }
counter <- pb(counter)

# set up output directories:
# str version 
outDir.training <- 'inputs/pm25/training_daily_js_only/'
outDir.predictions <- "inputs/pm25/predictions_js_only/"

js.raw <- here::here('inputs', 'pm25', 'base_models', 'daily', 'js')
wd <- "~/Downloads/"

#### ---------------------------- ####
#### 2. PROCESS JS MONTH BY MONTH ####
#### ---------------------------- ####
#for (i in 1:nrow(timeSteps)) {
 i <- 1 
  #### --------------- ####
  #### 3. DOWNLOAD ZIP ####
  #### --------------- ####
  # daily data has been zipped into monthly zip files
  infix <- paste0(timeSteps$year[i], timeSteps$month[i])
  url <- paste0(prefix, infix, suffix)
  zipfile <- paste0(wd, infix, ".zip")
  
  # bash command as a string
  download <- paste0("curl -o ", zipfile, " -b ~/.urs_cookies -c ~/.urs_cookies -L -n ", url)
  # run the bash command
  system(download)
  
  #### --------------- ####
  ####  4. UNZIP FILES ####
  #### --------------- ####
  # each day's data is stored in its own .rds file
  
  # 4a. unzip the month's folder
  unzip(zipfile, exdir = paste0(wd, infix))
  
  # 4b. get the names of the daily rds within that folder
  files <- list.files(paste0(js.raw, infix), pattern = ".rds")
  
  # 4c. rename files
  # 4c.i create clean names of files 
  # extract date of each file, and append .rds
  nfiles <- files %>% 
    stringr::str_extract("\\d{8}") %>% 
    stringr::str_sub(-2, -1) %>%
    paste0(".rds")
  
  # 4c.ii rename files that you downloaded
  # each file is now named after the day they represent. format: "DD"
  # the directory they belong to is formatted "YYYYMM"
  # overall, we then have: "./YYYYMM/DD.rda"
  file.rename(paste0(wd, infix, "/", files), paste0(wd, infix, "/", nfiles))

  #### ------------------------- ####
  ####  5. PROCESS JS DAY BY DAY ####
  #### ------------------------- ####
  
  # 5a create vector of days of the month in this timeStep
  days <- nfiles %>%
    stringr::str_sub(1, 2)
  
  # 5b create empty tables to store the predictions of interest and reference grid 
  epa.js <- tibble::tibble()
  refGrid <- tibble::tibble()

  # 5c begin loop; one iteration per day in timeStep
  for (d in days) {
    # read in JS data:
    preds <- tibble::tibble(pred_js = as.vector(t(readRDS(paste0(wd, infix, "/", d, ".rds"))))) 
    
    #### ---------------------------- ####
    ####  6. JOIN W/EPA TRAINING DATA ####
    #### ---------------------------- ####
    
    # 6a restrict to timeStep
    aqs.timeStep <- aqs %>% 
      dplyr::filter(year == timeSteps$year[i], month == timeSteps$month[i], day == d)
    
    # 6b we have this condition just in case aqs data is missing for one day 
    if (nrow(aqs.timeStep) > 0) {
      
      preds.aqs <- preds %>%
        dplyr::slice(key.AQS.JS$baseModel_id)
      
      jsExtract <- cbind(key.AQS.JS, preds.aqs) %>%
        tibble::as_tibble() %>%
        dplyr::select(ref_id, pred_js)
      
      aqs.js.day <- dplyr::inner_join(aqs.timeStep, jsExtract, by = "ref_id")
      
    } else {
      
      aqs.js.day <- tibble::tibble()
      
    }
    
    # record:
    aqs.js <- rbind(aqs.js, aqs.js.day)
    
    #### ----------------------------------- ####
    ####  7. GET JS PREDICTIONS FOR REF GRID ####
    #### ----------------------------------- ####
    
    # 7.a.i. get predictions at conus grid
    preds.ref <- preds %>%
      dplyr::slice(key.refGridConus.JS$baseModel_id)
    # 7.a.ii assign locations
    refGrid.day <- cbind(key.refGridConus.JS, preds.ref) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(year = as.character(timeSteps$year[i]), month = as.character(timeSteps$month[i]), day = d) %>%
      dplyr::select(ref_id, lat, lon, year, month, day, pred_js)
    # 7.a.iii record:
    refGridConus <- rbind(refGridConus, refGrid.day)
    
    # 7.b.i. get predictions at conus grid
    preds.ref <- preds %>%
      dplyr::slice(key.refGridNYS.JS$baseModel_id)
    # 7.b.ii.
    refGrid.day <- cbind(key.refGridNYS.JS, preds.ref) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(year = as.character(timeSteps$year[i]), month = as.character(timeSteps$month[i]), day = d) %>%
      dplyr::select(ref_id, lat, lon, year, month, day, pred_js)
    # 7.b.iii. record:
    refGridNYS <- rbind(refGridNYS, refGrid.day)
    
    # 7.c.i. get predictions at conus grid
    preds.ref <- preds %>%
      dplyr::slice(key.refGridCities.JS$baseModel_id)
    # 7.c.ii.
    refGrid.day <- cbind(key.refGridCities.JS, preds.ref) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(year = as.character(timeSteps$year[i]), month = as.character(timeSteps$month[i]), day = d) %>%
      dplyr::select(ref_id, lat, lon, year, month, day, pred_js)
    # 7.c.iii. record:
    refGridCitiess <- rbind(refGridCities, refGrid.day)
    
    # progress bar:
    counter <- pb(counter)
  }
  
  #### -------------------------- ####
  ####  8. SAVE THAT MONTH'S WORK ####
  #### -------------------------- ####
  # once we finish a month, save that month's work:
  aqs.js %>% 
  fst::write_fst(epa.js, paste0(wd, outDir.training, "training_js_", infix, ".csv"))
  refGridConus %>%
  fst::write_fst(refGrid, paste0(wd, outDir.refGrid, "refGridConus_js_", infix, ".csv"))
  
  #### ------------------------------ ####
  ####  9. DELETE ZIP AND JS RAW DATA ####
  #### ------------------------------ ####
  unlink(paste0(wd, infix), recursive = TRUE)
  unlink(paste0(wd, infix, ".zip"))
}
