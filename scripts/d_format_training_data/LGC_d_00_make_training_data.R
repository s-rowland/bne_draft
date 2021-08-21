# File: LGC_d_00_make_training_data.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 07/26/21
#
# Contents:
# 0. Package Imports
# 1. EPA & JS Daily Data
# 2. CMAQ ins
# 3. CMAQ outs
# 4. AV
# 5. GS
# 6. CACES

#### ------------------ ####
#### 0. PACKAGE IMPORTS ####
#### ------------------ ####
library(magrittr)
library(foreach)
library(tictoc)

source(here::here("scripts", "1_unstable_functions", "LGC_1_spatioTemporalJoin.R"))
source(here::here("scripts", "1_unstable_functions", "LGC_1_loadData.R"))
dataDir <- "~/Documents/Research_Marianthi/BNE_project/BNE_data_cleaning/"
#### ------------------------- ####
####  1. READ IN EPA & JS DATA ####
#### ------------------------- ####
print("Reading JS Data in (model 1/6)...")
epa.js <- list.files("~/Downloads/EPA-JS_training_data", full.names = T) %>%
  purrr::map_dfr(~readr::read_csv(., col_types = "cddcccdd"))

#### ----------- ####
#### 2. CMAQ INS ####
#### ----------- ####
print("Processing CMAQ INS (model 2/6)...")

# 2a. read in data:
cmaqInsPath <- "~/OneDrive - cumc.columbia.edu/CMAQ/cmaq_pm25_inputs_2010-2016.csv"
cmaqIns <- loadData(cmaqInsPath, "CMAQINS")

# 2b. make training data:
tic()
epa.js.cmaqIns <- spatioTemporalJoin(refData = epa.js,
                                     modelData = cmaqIns,
                                     modelName = "cmaq_ins",
                                     override = TRUE)
toc()
# should be 685,588 x 9
# took 582 seconds going one month at a time...
# took 85.33 seconds going one year at a time...

# 2c. clean environment:
rm(cmaqIns, cmaqInsPath, epa.js)

#### ------------ ####
#### 3. CMAQ OUTS ####
#### ------------ ####
print("Processing CMAQ Outs (model 3/6)...")

# 3a. read in data:
cmaqOutsPath <- paste0(dataDir, "CMAQ/outputs/cmaq_pm25_outputs_2010-2016.csv")
cmaqOuts <- loadData(cmaqOutsPath, "CMAQOUTS")

# 3b. make training data:
tic()
epa.js.cmaqIns.cmaqOuts <- spatioTemporalJoin(refData = epa.js.cmaqIns,
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
fPaths <- list.files(path = paste0(dataDir, "AV/PM25"), pattern = ".nc", full.names = TRUE)
av1 <- 121:141 %>% purrr::map_dfr(~ loadData(fPaths[.x], "AV"))
av2 <- 142:162 %>% purrr::map_dfr(~ loadData(fPaths[.x], "AV"))
av3 <- 163:183 %>% purrr::map_dfr(~ loadData(fPaths[.x], "AV"))
av4 <- 184:204 %>% purrr::map_dfr(~ loadData(fPaths[.x], "AV"))

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
# 5a. read in data:
gsPath <- paste0(dataDir, "GS/GBD2016_PREDPOP_FINAL.RData")
gs <- loadData(gsPath, "GS")

# 5b. make training data:
tic()
epa.gs <- spatioTemporalJoin(refData = epa,
                             modelData = gs,
                             modelName = "gs")
toc()
# took 14.312 seconds...
# output a tibble 918,636 x 8

#### ------------- ####
#### 6. CACES DATA ####
#### ------------- ####
# 6a. read in data:
cacesPath <- paste0(dataDir, "CACES/downloaded/caces_tracts.csv")
caces <- loadData(cacesPath, "CACES")

# 6b. make training data:
tic()
epa.caces <- spatioTemporalJoin(refData = epa,
                                modelData = caces,
                                modelName = "caces",
                                censusTractFile = "~/Documents/Research_Marianthi/BNE_project/cb_2019_us_tract_500k")
toc()
