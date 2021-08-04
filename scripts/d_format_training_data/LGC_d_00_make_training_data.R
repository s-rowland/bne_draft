# File: LGC_d_00_make_training_data.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 07/26/21
# To Do: work on GS, Caces, and JS.

#### ------------------ ####
#### 0. PACKAGE IMPORTS ####
#### ------------------ ####
library(magrittr)
library(foreach)
library(tictoc)
source("~/Documents/Research_Marianthi/BNE_project/BNE_data_cleaning/spatioTemporalJoin.R")
source("~/Documents/Research_Marianthi/BNE_project/BNE_data_cleaning/loadData.R")

#### ----------------------------- ####
#### 1. READ IN EPA AQS DAILY DATA ####
#### ----------------------------- ####
epaPath <- "~/Documents/Research_Marianthi/BNE_project/EPA_data/latest_version_clean_daily_data/daily_data_2000-2016.csv"
epa <- loadData(epaPath, "EPA")

#### ----------- ####
#### 2. CMAQ INS ####
#### ----------- ####
# 2a. read in data:
cmaqInsPath <- "~/OneDrive - cumc.columbia.edu/CMAQ/cmaq_pm25_inputs_2010-2016.csv"
cmaqIns <- loadData(cmaqInsPath, "cmaq_ins")

# 2b. make training data:
tic()
epa.cmaqIns <- spatioTemporalJoin(refData = epa,
                                  modelData = cmaqIns,
                                  modelName = "cmaq_ins",
                                  override = TRUE)
toc()
# should be 685,588 x 8
# took 582 seconds going one month at a time...
# took 85.33 seconds going one year at a time...

#### ------------ ####
#### 3. CMAQ OUTS ####
#### ------------ ####
# 3a. read in data:
cmaqOutsPath <- "~/Documents/Research_Marianthi/BNE_project/BNE_data_cleaning/CMAQ/outputs/cmaq_pm25_outputs_2010-2016.csv"
cmaqOuts <- loadData(cmaqOutsPath, "cmaq_outs")

# 3b. make training data:
tic()
epa.cmaqOuts <- spatioTemporalJoin(refData = epa,
                                   modelData = cmaqOuts,
                                   modelName = "cmaq_outs",
                                   censusTractFile = "~/Documents/Research_Marianthi/BNE_project/cb_2019_us_tract_500k")
toc()

#### ------------------ ####
#### 4. AV MONTHLY DATA ####
#### ------------------ ####
# files 121 - 204 of fPaths span 2010 - 2016
# av is big so we should read 40 files at a time? 
# also faster to parse it one month at a time in spatioTemporalJoin

# 4a. read in data:
fPaths <- list.files(path = "~/Documents/Research_Marianthi/BNE_project/BNE_data_cleaning/AV/PM25", pattern = ".nc", full.names = TRUE)
av1 <- 121:141 %>% purrr::map_dfr(~ loadData(fPaths[.x], "AV"))
av2 <- 142:162 %>% purrr::map_dfr(~ loadData(fPaths[.x], "AV"))
av3 <- 163:183 %>% purrr::map_dfr(~ loadData(fPaths[.x], "AV"))
av4 <- 164:204 %>% purrr::map_dfr(~ loadData(fPaths[.x], "AV"))

# 4b. make training data:
tic()
epa.av1 <- spatioTemporalJoin(refData = epa,
                              modelData = av1,
                              modelName = "av",
                              override = TRUE)
toc()

tic()
epa.av2 <- spatioTemporalJoin(refData = epa,
                              modelData = av2,
                              modelName = "av",
                              override = TRUE)
toc()

tic()
epa.av3 <- spatioTemporalJoin(refData = epa,
                              modelData = av3,
                              modelName = "av",
                              override = TRUE)
toc()

tic()
epa.av4 <- spatioTemporalJoin(refData = epa,
                              modelData = av4,
                              modelName = "av",
                              override = TRUE)
toc()

epa.av <- rbind(epa.av1, epa.av2, epa.av3, epa.av4)

#### ----------------- ####
#### 5. GS ANNUAL DATA ####
#### ----------------- ####

#### ------------- ####
#### 6. CACES DATA ####
#### ------------- ####

#### ---------- ####
#### 7. JS DATA ####
#### ---------- ####
