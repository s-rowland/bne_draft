# File: process_JS.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 8/19/21
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

#### ------------------ ####
#### 0. PACKAGE IMPORTS ####
#### ------------------ ####
library(magrittr)
source(here::here("scripts", "1_unstable_functions", "LGC_1_loadData.R"))

#### ------------------ ####
####  1. GENERAL SETUP  ####
#### ------------------ ####
# specify the years and months we are interested in:
years <- c(2010:2016)
months <- stringr::str_pad(1:12, 2, "left", "0")
timeSteps <- expand.grid(list("year" = years, "month" = months)) %>%
  dplyr::arrange(year, month)

# define the link to download the data & working directory:
prefix <- "https://beta.sedac.ciesin.columbia.edu/downloads/data/aqdh/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016-"
suffix <- "-rds.zip"
wd <- "~/Downloads/"

# read in EPA data:
epaPath <- "~/Documents/Research_Marianthi/BNE_project/EPA_data/latest_version_clean_daily_data/daily_data_2000-2016.csv"
epa <- loadData(epaPath, "EPA")

# read in JS files:
jsKey <- loadData("~/Desktop/epa-js_nn_key.csv", "JSEPAKEY")
jsRefGrid <- loadData("~/Desktop/js_preds_ref_grid.csv", "JSREF")

# for record keeping:
epa.js <- tibble::tibble()
refGrid <- tibble::tibble()

# progress bar:
n <- as.numeric(as.Date("2017-01-01") - as.Date("2010-01-01"))
progressBar <- txtProgressBar(min=0, max=n, width=50, style=3)
counter <- 0
pb <- function(p) { setTxtProgressBar(progressBar, p); return(p+1) }
counter <- pb(counter)

# set up output directories:
outDir.epa <- "EPA-JS_training_data/"
outDir.ref <- "JSrefGrid/"

dir.create(paste0(wd, outDir.epa))
dir.create(paste0(wd, outDir.ref))

#### ---------------------------- ####
#### 2. PROCESS JS MONTH BY MONTH ####
#### ---------------------------- ####
for (i in 1:nrow(timeSteps)) {
  
  #### --------------- ####
  #### 3. DOWNLOAD ZIP ####
  #### --------------- ####
  infix <- paste0(timeSteps$year[i], timeSteps$month[i])
  url <- paste0(prefix, infix, suffix)
  zipfile <- paste0(wd, infix, ".zip")
  
  download <- paste0("curl -o ", zipfile, " -b ~/.urs_cookies -c ~/.urs_cookies -L -n ", url)
  
  system(download)
  
  #### --------------- ####
  ####  4. UNZIP FILES ####
  #### --------------- ####
  unzip(zipfile, exdir = paste0(wd, infix))
  
  files <- list.files(paste0(wd, infix), pattern = ".rds")
  
  nfiles <- files %>% 
    stringr::str_extract("\\d{8}") %>% 
    stringr::str_sub(-2, -1) %>%
    paste0(".rds")
  
  # each file is now named after the day they represent. format: "DD"
  # the directory they belong to is formatted "YYYYMM"
  # overall, we then have: "./YYYYMM/DD.rda"
  file.rename(paste0(wd, infix, "/", files), paste0(wd, infix, "/", nfiles))
  
  days <- nfiles %>%
    stringr::str_sub(1, 2)
  
  #### ------------------------- ####
  ####  5. PROCESS JS DAY BY DAY ####
  #### ------------------------- ####
  for (d in days) {
    # read in JS data:
    preds <- tibble::tibble(js_pred = as.vector(t(readRDS(paste0(wd, infix, "/", d, ".rds"))))) 
    
    #### ---------------------------- ####
    ####  6. JOIN W/EPA TRAINING DATA ####
    #### ---------------------------- ####
    epa.timeStep <- epa %>% 
      dplyr::filter(year == timeSteps$year[i], month == timeSteps$month[i], day == d)
    
    if (nrow(epa.timeStep) > 0) {
      
      preds.epa <- preds %>%
        dplyr::slice(jsKey$js_index)
      
      jsExtract <- cbind(jsKey, preds.epa) %>%
        tibble::as_tibble() %>%
        dplyr::select(epa_id, js_pred)
      
      epa.js.day <- dplyr::inner_join(epa.timeStep, jsExtract, by = c("ref_id" = "epa_id"))
      
    } else {
      
      epa.js.day <- tibble::tibble()
      
    }
    
    # record:
    epa.js <- rbind(epa.js, epa.js.day)
    
    #### ------------------------------------- ####
    ####  7. GET REF GRID FOR PREDICTIONS DATA ####
    #### ------------------------------------- ####
    # 1% of Joel's data
    preds.ref <- preds %>%
      dplyr::slice(jsRefGrid$js_index)
    
    refGrid.day <- cbind(jsRefGrid, preds.ref) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(year = as.character(timeSteps$year[i]), month = as.character(timeSteps$month[i]), day = d) %>%
      dplyr::rename(lat = js_lat, lon = js_lon, ref_id = js_index) %>%
      dplyr::select(ref_id, lat, lon, year, month, day, js_pred)
    
    # record:
    refGrid <- rbind(refGrid, refGrid.day)
    
    # progress bar:
    counter <- pb(counter)
  }
  
  #### -------------------------- ####
  ####  8. SAVE THAT MONTH'S WORK ####
  #### -------------------------- ####
  # once we finish a month, save that month's work:
  readr::write_csv(epa.js, paste0(wd, outDir.epa,"epa-js_", infix, ".csv"))
  readr::write_csv(refGrid, paste0(wd, outDir.ref, "js_ref-grid_", infix, ".csv"))
  
  #### ------------------------------ ####
  ####  9. DELETE ZIP AND JS RAW DATA ####
  #### ------------------------------ ####
  unlink(paste0(wd, infix), recursive = TRUE)
  unlink(paste0(wd, infix, ".zip"))
}
