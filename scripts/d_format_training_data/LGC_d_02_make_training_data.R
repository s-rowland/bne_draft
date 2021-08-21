# File: LGC_d_02_make_training_data.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 08/21/21
#
# Contents:
# 0. Package Imports & Global Variables
# 1. EPA & JS Daily Data
# 2. Setting Up CMAQ, GS, CACES, Loop
# 3. CMAQ, GS, CACES Loop
# 4. AV Data Loop
# 5. End of program

#### ------------------------------------- ####
#### 0. PACKAGE IMPORTS & GLOBAL VARIABLES ####
#### ------------------------------------- ####
library(magrittr)
library(foreach)
library(tictoc)

source(here::here("scripts", "1_unstable_functions", "LGC_1_spatioTemporalJoin.R"))
source(here::here("scripts", "1_unstable_functions", "LGC_1_loadData.R"))

dataDir <- "~/Documents/Research_Marianthi/BNE_project/BNE_data_cleaning/"
ctf <- "~/Documents/Research_Marianthi/BNE_project/cb_2019_us_tract_500k"

#### ------------------------- ####
####  1. READ IN EPA & JS DATA ####
#### ------------------------- ####
print("Reading JS Data...")
trainingData <- list.files("~/Downloads/EPA-JS_training_data", full.names = T) %>%
  purrr::map_dfr(~readr::read_csv(., col_types = "cddcccdd"))

#### -------------------------------- ####
####  2. SET UP CMAQ, GS, CACES LOOP  ####
#### -------------------------------- ####
paths <- c(
  "~/OneDrive - cumc.columbia.edu/CMAQ/cmaq_pm25_inputs_2010-2016.csv",
  paste0(dataDir, "CMAQ/outputs/cmaq_pm25_outputs_2010-2016.csv"),
  paste0(dataDir, "GS/GBD2016_PREDPOP_FINAL.RData"),
  paste0(dataDir, "CACES/downloaded/caces_tracts.csv")
)

pathCodes <- c("CMAQINS", "CMAQOUTS", "GS", "CACES")
modelNames <- c("cmaq_ins", "cmaq_outs", "gs", "caces")
overrides <- c(T, F, F, F)
censusTrackFiles <- list(NULL, ctf, NULL, ctf)
n <- length(paths)

#### ------------------------ ####
####  3. CMAQ, GS, CACES LOOP ####
#### ------------------------ ####
for (i in 1:n) {
  modelData <- loadData(path = paths[i], dataset = pathCodes[i])
  
  print(paste("Processing model:", i, "/", n))
  print(paste("Model:", pathCodes[i]))
  tic()
  trainingData <- spatioTemporalJoin(refData = trainingData,
                                     modelData = modelData,
                                     modelName = modelNames[i],
                                     override = overrides[i],
                                     censusTractFile = censusTrackFiles[[i]])
  print(paste("Time to finish processing", pathCodes[i], "="))
  toc()
}

readr::write_csv(trainingData, "~/Desktop/trainingData-5models.csv")

#### ---------------- ####
####  4. AV DATA LOOP ####
#### ---------------- ####
print("Processing AV model...")
# files 121 - 204 of fPaths span 2010 - 2016
# av is big so we should read 20 files at a time? 
# also faster to parse it one month at a time in spatioTemporalJoin
avPaths <- list.files(path = paste0(dataDir, "AV/PM25"), pattern = ".nc", full.names = TRUE)
startingPathIdxs <- c(121, 142, 163, 184)
endingPathIdxs <- c(141, 162, 183, 204)

tic()
trainingData6 <- foreach(i = iterators::icount(length(startingPathIdxs)), .combine = rbind) %do% {
  print(paste("Processing AV: part", i, "/", length(startingPathIdxs)))
  modelData <- startingPathIdxs[i]:endingPathIdxs[i] %>% purrr::map_dfr(~ loadData(avPaths[.x], "AV"))
  
  spatioTemporalJoin(refData = trainingData,
                     modelData = modelData,
                     modelName = "av",
                     override = TRUE)
  
}
print("Time to finish processing AV = ")
toc()

#### ------------------ ####
####  5. END OF PROGRAM ####
#### ------------------ ####
readr::write_csv(trainingData6, "~/Desktop/trainingData-6models.csv")

print("Finished making training data!")