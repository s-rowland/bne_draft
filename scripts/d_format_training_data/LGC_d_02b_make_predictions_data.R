# File: LGC_d_02b_make_predictions_data.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 08/24/21
#
# Contents:
# N. Notes
# 0. Package Imports & Global Variables
# 1. Main Loop
#    a. Read in JS data
#    b. Set up CMAQ, GS, CACES loop
#    c. Run CMAQ, GS, CACES loop
#    d. AV data loop
#    e. Save outputs

#### -------- ####
#### N. NOTES ####
#### -------- ####
# This script takes about 1 hour 25 minutes to run
# assuming you do not include FIPS codes...

#### ------------------------------------- ####
#### 0. PACKAGE IMPORTS & GLOBAL VARIABLES ####
#### ------------------------------------- ####
library(magrittr)
library(foreach)
library(tictoc)

source(here::here("scripts", "1_unstable_functions", "LGC_1_spatioTemporalJoin.R"))
source(here::here("scripts", "1_unstable_functions", "LGC_1_loadData.R"))
source(here::here("scripts", "1_unstable_functions", "LGC_1_saveData.R"))

dataDir <- "~/Documents/Research_Marianthi/BNE_project/BNE_data_cleaning/"
outputDir <- "~/Desktop/dailyPredictionsDataset/"
ctf <- "~/Documents/Research_Marianthi/BNE_project/cb_2019_us_tract_500k"
years <- c(2010:2015)
n <- length(years)
dir.create(outputDir)

# because the JS reference grid is so large (~17GB)
# let's read & process the data one year at a time...
#### ------------- ####
####  1. MAIN LOOP ####
#### ------------- ####
jsPath <- "~/Downloads/JSrefGrid"

refGrid <- for(i in 1:n) {
  cat(paste0("\nProcessing year ", i, " / ", n, " (", years[i], ")...\n"))
  #### -------------------- ####
  ####  1a. READ IN JS DATA ####
  #### -------------------- ####
  refGrid <- list.files(jsPath, pattern = paste0("js_ref-grid_", years[i], "(.*).csv"), full.names = T) %>%
    purrr::map_dfr(~readr::read_csv(., col_types = "cddcccd")) %>%
    dplyr::rename(obs_pm2_5 = js_pred)
  
  cat(paste0("\n\tRead in JS for ", years[i]))
  
  #### --------------------------------- ####
  ####  1b. SET UP CMAQ, GS, CACES LOOP  ####
  #### --------------------------------- ####
  paths <- c(
    paste0(dataDir, "CMAQ/inputs/", years[i], "_cmaq_ins.csv"),
    paste0(dataDir, "CMAQ/outputs/", years[i], "_cmaq_outs.csv"),
    paste0(dataDir, "GS/GBD2016_PREDPOP_FINAL.RData"),
    paste0(dataDir, "CACES/downloaded/caces_tracts.csv")
  )
  
  pathCodes <- c("CMAQINS", "CMAQOUTS", "GS", "CACES")
  modelNames <- c("cmaq_ins", "cmaq_outs", "gs", "caces")
  overrides <- c(T, T, F, F) # used to be c(T, F, F, F)
  censusTrackFiles <- list(NULL, NULL, NULL, NULL) # used to be list(NULL, ctf, NULL, ctf)
  m <- length(paths)
  
  #### ------------------------- ####
  ####  1c. CMAQ, GS, CACES LOOP ####
  #### ------------------------- ####
  for (j in 1:m) {
    cat(paste("\n\tProcessing model:", j, "/", m + 1))
    cat(paste("\n\tModel:", pathCodes[j], "\n\t"))
    
    modelData <- loadData(path = paths[j], dataset = pathCodes[j]) %>%
      dplyr::filter(year == years[i])
    
    if ("fips" %in% colnames(modelData)) modelData <- modelData %>% dplyr::select(-fips)
    
    tic()
    refGrid <- spatioTemporalJoin(refData = refGrid,
                                  modelData = modelData,
                                  modelName = modelNames[j],
                                  override = overrides[j],
                                  censusTractFile = censusTrackFiles[[j]])
    cat(paste("\n\tTime to finish processing", pathCodes[j], "="))
    toc()
  }
  
  #### ----------------- ####
  ####  1d. AV DATA LOOP ####
  #### ----------------- ####
  cat(paste("\n\tProcessing model:", m + 1, "/", m + 1))
  cat("\n\tModel: AV\n\t")
  # faster to parse AV one month at a time in spatioTemporalJoin?
  avPaths <- list.files(path = paste0(dataDir, "AV/PM25"), pattern = paste0("V4NA03_PM25_NA_", years[i], "(.*).nc"), full.names = TRUE)
  modelData <- avPaths %>% purrr::map_dfr(~ loadData(.x, "AV"))
  
  tic()
  refGrid <- spatioTemporalJoin(refData = refGrid,
                                modelData = modelData,
                                modelName = "av",
                                override = TRUE)
  cat("\n\tTime to finish processing AV = ")
  toc()
  
  #### ----------------- ####
  ####  1e. SAVE OUTPUTS ####
  #### ----------------- ####
  readr::write_csv(refGrid, paste0(outputDir, "dailyPredictionsData_", years[i], ".csv"))
  saveData(refGrid, paste0(outputDir, "dailyPredictionsData_2010-2015/"))
}