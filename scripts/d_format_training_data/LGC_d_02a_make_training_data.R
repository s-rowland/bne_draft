# File: LGC_d_02a_make_training_data.R
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
source(here::here("scripts", "1_unstable_functions", "LGC_1_saveData.R"))

dataDir <- "~/Documents/Research_Marianthi/BNE_project/BNE_data_cleaning/"
ctf <- "~/Documents/Research_Marianthi/BNE_project/cb_2019_us_tract_500k"

#### ------------------------- ####
####  1. READ IN EPA & JS DATA ####
#### ------------------------- ####
cat("\nReading JS Data...\n")
trainingData <- list.files("~/Downloads/EPA-JS_training_data", full.names = T) %>%
  purrr::map_dfr(~readr::read_csv(., col_types = "cddcccdd"))

#### -------------------------------- ####
####  2. SET UP CMAQ, GS, CACES LOOP  ####
#### -------------------------------- ####
paths <- c(
  paste0(dataDir, "CMAQ/inputs/cmaq_pm25_inputs_2010-2016.csv"),        # 107.589 sec elapsed
  paste0(dataDir, "CMAQ/outputs/cmaq_pm25_outputs_2010-2016.csv"),      # 54.932 sec elapsed
  paste0(dataDir, "GS/GBD2016_PREDPOP_FINAL.RData"),                    # 10.176 sec elapsed
  paste0(dataDir, "CACES/downloaded/caces_tracts.csv")                  # 5.197 sec elapsed
)                                                                       # note: these times are always without fips data

pathCodes <- c("CMAQINS", "CMAQOUTS", "GS", "CACES")
modelNames <- c("cmaq_ins", "cmaq_outs", "gs", "caces")
overrides <- c(T, T, F, F) # used to be c(T, F, F, F)
censusTrackFiles <- list(NULL, NULL, NULL, NULL) # used to be list(NULL, ctf, NULL, ctf)
n <- length(paths)

#### ------------------------ ####
####  3. CMAQ, GS, CACES LOOP ####
#### ------------------------ ####
for (i in 1:n) {
  cat(paste("\nProcessing model:", i, "/", n))
  cat(paste("\nModel:", pathCodes[i]))
  
  modelData <- loadData(path = paths[i], dataset = pathCodes[i])
  
  if ("fips" %in% colnames(modelData)) modelData <- modelData %>% dplyr::select(-fips)

  tic()
  trainingData <- spatioTemporalJoin(refData = trainingData,
                                     modelData = modelData,
                                     modelName = modelNames[i],
                                     override = overrides[i],
                                     censusTractFile = censusTrackFiles[[i]])
  cat(paste("\nTime to finish processing", pathCodes[i], "="))
  toc()
}

#### ---------------- ####
####  4. AV DATA LOOP ####
#### ---------------- ####
cat("\nProcessing AV model...\n")
# files 121 - 204 of fPaths span 2010 - 2016
# av is big so we should read 20 files at a time? 
# also faster to parse it one month at a time in spatioTemporalJoin
avPaths <- list.files(path = paste0(dataDir, "AV/PM25"), pattern = ".nc", full.names = TRUE)
startingPathIdxs <- c(121, 142, 163, 184)
endingPathIdxs <- c(141, 162, 183, 204)

tic()
trainingData6 <- foreach(i = iterators::icount(length(startingPathIdxs)), .combine = rbind) %do% {
  cat(paste("\nProcessing AV: part", i, "/", length(startingPathIdxs), "\n"))
  modelData <- startingPathIdxs[i]:endingPathIdxs[i] %>% purrr::map_dfr(~ loadData(avPaths[.x], "AV"))
  
  spatioTemporalJoin(refData = trainingData,
                     modelData = modelData,
                     modelName = "av",
                     override = TRUE)
  
}
cat("Time to finish processing AV = ")
toc()
# 1939.502 sec elapsed

#### ------------------ ####
####   5. SAVE OUTPUTS  ####
#### ------------------ ####
outputDir <- "~/Desktop/dailyTrainingData/"

dir.create(outputDir)

readr::write_csv(trainingData6, paste0(outputDir, "dailyTrainingData_2010-2015.csv"))

output <- saveData(trainingData6, paste0(outputDir, "dailyTrainingData_2010-2015"))

cat("\nFinished making training data! Output saved to: \n\n")
cat(paste(output, collapse = "\n"))
