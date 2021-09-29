# File: LGC_c_01_clean_epa_aqs_data.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 12/23/20 (reformatted 9/27/21)

# Contents:
# N. Notes
# 0. Package Imports
# 1. Load daily data
# 2. Load annual data
# 3. Hash tables for annual data
# 4. Hash tables for seasonal data
# 5. Generate datasets
# 6. Save & log all data

#----------------#
#### N. NOTES ####
#----------------#
# The point of this file is to clean the EPA AQS data in a few different ways.
# This file will generate many (very similar) datasets as a result.
# Other files (currently named lost_monitors.Rmd and maps.Rmd) will then look at the
# advantages and disadvantages of the generated datasets, helping us come to 
# a decision as to which datasets to use as the EPA AQS daily and annual datasets.

# In the end, the objects 'ddCompact75' and 'adCompact75' were chosen from this file
# to serve as the daily and annual datasets respectively. This decision was made after
# consulting the .html output from lost_monitors.Rmd with Marianthi and 
# subsequently discussing with the whole BNE team. 
# (Note: I believe lost_monitors.Rmd relies on plots made from maps.Rmd).

# This file was originally written well before the BNE coding conventions 
# document was made. It was updated in format / style only in an attempt to follow
# the conventions doc. It still needs to be debugged to confirm it still 
# runs as expected. Beyond that, file paths should be changed to reflect the BNE 
# directory structure (here::here conventions utilized). Now that we have a server
# (yay!) this will be done soon... Additional comments are probably needed, especially
# to explain what the hash tables are doing in sections 3 and 4.
# This file used to be named 'clean.Rmd' on Lawrence's local copy.

#--------------------------#
#### 0. PACKAGE IMPORTS ####
#--------------------------#
library(dplyr)
library(stringr)

#--------------------------#
#### 1. LOAD DAILY DATA ####
#--------------------------#
dailyDataFiles <- list.files("~/Documents/Research_Marianthi/BNE_project/EPA_data/daily_data", pattern = ".csv", full.names = TRUE)

# data is 3772249 x 29
# create monitor IDs
dd <- purrr::map_dfr(dailyDataFiles, ~read.csv(., header=TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)) %>% 
  filter(Event.Type != "Excluded", Sample.Duration != "1 HOUR") %>% 
  mutate(State.Code = str_pad(State.Code, 2, pad="0"),
    County.Code = str_pad(County.Code, 3, pad="0"),
    Site.Num = str_pad(Site.Num, 4, pad="0"),
    Parameter.Code = str_pad(Parameter.Code, 5, pad="0"),
    POC = str_pad(POC, 1, pad="0"),
    Monitor.ID = str_c(State.Code, County.Code, Site.Num, Parameter.Code, POC, sep = "-"), 
    Date.Local.Formatted = as.POSIXct(Date.Local, format = "%Y-%m-%d"), 
    Year = format(Date.Local.Formatted, format = "%Y"), 
    Month = format(Date.Local.Formatted, format = "%m"), 
    Monitor.Days = str_c(Monitor.ID, Date.Local, sep = "-"),
    Monitor.Years = str_c(Monitor.ID, Year, sep = "-"))

# length(unique(dd$Monitor.ID)) = 2814, so there are 2814 distinct monitors before any cleaning.
# verify that each monitor-day pairing only has one measurement on record. 
#head(dd %>% count(Monitor.Days, sort = T))
# next we want to map those addresses to schedulers... 
# we do this by looking at the 'Required Day Count` variable in the Annual summary files...

#---------------------------#
#### 2. LOAD ANNUAL DATA ####
#---------------------------#
annualDataFiles <- list.files("~/Documents/Research_Marianthi/BNE_project/EPA_data/annual_data", pattern = ".csv", full.names = TRUE)

ad <- purrr::map_dfr(annualDataFiles, ~read.csv(., header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)) %>% 
  filter(Parameter.Code == 88101, 
    Event.Type != "Events Excluded", 
    Event.Type != "Concurred Events Excluded", 
    Sample.Duration != "1 HOUR", 
    Metric.Used == "Daily Mean", 
    Pollutant.Standard == "PM25 24-hour 2012") %>%
  mutate(State.Code = str_pad(State.Code, 2, pad="0"),
    County.Code = str_pad(County.Code, 3, pad="0"),
    Site.Num = str_pad(Site.Num, 4, pad="0"),
    Parameter.Code = str_pad(Parameter.Code, 5, pad="0"),
    POC = str_pad(POC, 1, pad="0"),
    Monitor.ID = str_c(State.Code, County.Code, Site.Num, Parameter.Code, POC, sep = "-"), 
    Monitor.Years = str_c(Monitor.ID, Year, sep = "-"))

# verify that each monitor-year pairing only has one measurement on file
#head(annualData %>% count(Monitor.Years, sort = T))

ad75 <- ad %>% filter(Observation.Percent >= 75)

#--------------------------------------#
#### 3. HASH TABLES FOR ANNUAL DATA ####
#--------------------------------------#
# 3a. initialize hash table
monitors <- unique(ad$Monitor.ID)
years <- as.character(unique(ad$Year))
requiredDayCount.HT <- new.env(hash=TRUE)
obsPerc.HT <- new.env(hash=TRUE)
for (monitor in monitors) {
  requiredDayCount.HT[[monitor]] <- new.env(hash=TRUE)
  obsPerc.HT[[monitor]] <- new.env(hash=TRUE)
  for (year in years) {
    requiredDayCount.HT[[monitor]][[year]] <- c()
    obsPerc.HT[[monitor]][[year]] <- c()
  }
}

# 3b. fill it
for (i in 1:nrow(ad)) {
  rdCenv <- requiredDayCount.HT[[ad$Monitor.ID[i]]][[as.character(ad$Year[i])]]
  opCenv <- obsPerc.HT[[ad$Monitor.ID[i]]][[as.character(ad$Year[i])]]
  requiredDayCount.HT[[ad$Monitor.ID[i]]][[as.character(ad$Year[i])]] <- c(rdCenv, ad$Required.Day.Count[i])
  obsPerc.HT[[ad$Monitor.ID[i]]][[as.character(ad$Year[i])]] <- c(opCenv, ad$Observation.Percent[i])
}

# 3c. verify hash table is as it should be
rdCount <- 0
opCount <- 0
for (monitor in monitors) {
  for (year in years) {
    if (length(requiredDayCount.HT[[monitor]][[year]]) > 1) {
      rdCount <- rdCount + 1
    } 
    if (length(obsPerc.HT[[monitor]][[year]]) > 1) {
      opCount <- opCount + 1
    } 
  }
}

#----------------------------------------#
#### 4. HASH TABLES FOR SEASONAL DATA ####
#----------------------------------------#
# 4a. initialize hash table
seasons.HT <- new.env(hash=TRUE)
for (monitor in monitors) {
  seasons.HT[[monitor]] <- new.env(hash=TRUE)
  for (year in years) {
    seasons.HT[[monitor]][[year]] <- c("Spring" = 0, "Summer" = 0, "Fall" = 0, "Winter" = 0)
  }
}

# 4b fill it
for (i in 1:nrow(dd)) {
  monitor <- dd$Monitor.ID[i]
  year <- dd$Year[i]
  month <- dd$Month[i]
  
  if (month %in% c("12", "01", "02")) {
    seasons.HT[[monitor]][[year]]["Winter"] <- seasons.HT[[monitor]][[year]]["Winter"] + 1
  } else if (month %in% c("03", "04", "05")) {
    seasons.HT[[monitor]][[year]]["Spring"] <- seasons.HT[[monitor]][[year]]["Spring"] + 1
  } else if (month %in% c("06", "07", "08")) {
    seasons.HT[[monitor]][[year]]["Summer"] <- seasons.HT[[monitor]][[year]]["Summer"] + 1
  } else {
    seasons.HT[[monitor]][[year]]["Fall"] <- seasons.HT[[monitor]][[year]]["Fall"] + 1
  }
}

#----------------------------#
#### 5. GENERATE DATASETS ####
#----------------------------#
dd75 <- dd %>% filter(Monitor.Years %in% ad75$Monitor.Years)

season75 <- numeric(length = nrow(ad75))
for (i in 1:nrow(ad75)) {
  monitor <- ad75$Monitor.ID[i]
  year <- as.character(ad75$Year[i])
  oc <- ad75$Observation.Count[i] # could also compare this to the required day count...
  seasonTable <- seasons.HT[[monitor]][[year]] / (oc / 4) >= .75
  season75[i] <- ifelse(FALSE %in% seasonTable, 0, 1)
}

adSeason75 <- cbind(ad75, season75) %>% filter(season75 == 1) %>% select(!season75)
ddSeason75 <- dd75 %>% filter(Monitor.Years %in% adSeason75$Monitor.Years)
adCompact75 <- ad75 %>% mutate(Arithmetic.Mean.Seasonal = ifelse(season75 == 1, Arithmetic.Mean, NA))
dailySeason75 <- (dd75 %>% mutate(season75_b = Monitor.Years %in% adSeason75$Monitor.Years) %>% select(season75_b)) * 1
ddCompact75 <- dd75 %>% mutate(Arithmetic.Mean.Seasonal = ifelse(dailySeason75 == 1, Arithmetic.Mean, NA))

append_day_count <- function(df, ht) {
  newCol <- numeric(length = nrow(df))
  for (i in 1:nrow(df)) {
    monitor <- df$Monitor.ID[i]
    year <- as.character(df$Year[i])
    if (is.null(ht[[monitor]][[year]])) {
      newCol[i] <- NA
    } else {
      newCol[i] <- ht[[monitor]][[year]]
    }
  }
  df$Required.Day.Count <- newCol
  df
}

dd <- append_day_count(dd, requiredDayCount.HT)
dd75 <- append_day_count(dd75, requiredDayCount.HT)
ddSeason75 <- append_day_count(ddSeason75, requiredDayCount.HT)
ddCompact75 <- append_day_count(ddCompact75, requiredDayCount.HT)

#------------------------------#
#### 6. SAVE & LOG ALL DATA ####
#------------------------------#
# 6a. logging function so we have readmes for each dataset generated
logDF <- function(df, fname, outDir="~/Documents/Research_Marianthi/BNE_project/EPA_data/out/"){
  fileConn <- file(str_c(outDir, fname, ".txt"))
  isDaily <- "Date.Local.Formatted" %in% colnames(df)
  if(isDaily) {
    dfToSave <- df %>% select(Monitor.ID, Latitude, Longitude, Datum, State.Name, Date.Local.Formatted, Year, Arithmetic.Mean, Arithmetic.Mean.Seasonal, Required.Day.Count)
    extraLines <- c()
  } else {
    dfToSave <- ifelse("Arithmetic.Mean.Seasonal" %in% colnames(df), 
      df %>% select(Monitor.ID, Latitude, Longitude, Datum, State.Name, Year, Arithmetic.Mean, Arithmetic.Mean.Seasonal), 
      df %>% select(Monitor.ID, Latitude, Longitude, Datum, State.Name, Year, Arithmetic.Mean))
    extraLines <- c(
      #str_c("Observation Percent: {", str_c(as.character(sort(as.numeric(unique(df$Observation.Percent)))), collapse = ", "), "}")#,
      #str_c("Observation Count: {", str_c(as.character(sort(as.numeric(unique(df$Observation.Count)))), collapse = ", "), "}"),
      #str_c("Required Day Count: {", str_c(as.character(sort(as.numeric(unique(df$Required.Day.Count)))), collapse = ", "), "}")
    )
  }
  
  lines <- c(
    str_pad("", 80, pad = "#"),
    "",
    str_c("Metadata for ", fname, ".csv"),
    "",
    str_c("Dimensions of data: ", str_c(as.character(dim(dfToSave)), collapse = " x ")),
    str_c("Column names: ", str_c(colnames(dfToSave), collapse = ", ")),
    "",
    str_c("Num. of unique monitors: ", as.character(length(unique(dfToSave$Monitor.ID)))),
    str_c("Num. of unique locations: ", as.character(length(unique(str_c(dfToSave$Latitude, dfToSave$Longitude, sep=", "))))),
    ifelse(isDaily, str_c("Num. of unique dates: ", as.character(length(unique(dfToSave$Date.Local.Formatted)))), "Num. of unique dates: NA (annual data)"),
    str_c("Years covered: ", str_c(as.character(unique(dfToSave$Year)), collapse = ", ")),
    "",
    str_pad("", 80, pad = "#"),
    "",
    "Discarded metadata: \n",
    str_c("Parameter Code: {", str_c(as.character(sort(as.numeric(unique(df$Parameter.Code)))), collapse = ", "), "}"),
    str_c("POC: {", str_c(as.character(sort(as.numeric(unique(df$POC)))), collapse = ", "), "}"),
    #str_c("Datum: {", str_c(as.character(unique(df$Datum)), collapse = ", "), "}"),
    str_c("Parameter Name: {", str_c(as.character(unique(df$Parameter.Name)), collapse = ", "), "}"),
    str_c("Sample Duration: {", str_c(as.character(unique(df$Sample.Duration)), collapse = ", "), "}"),
    str_c("Pollutant Standard: {", str_c(as.character(unique(df$Pollutant.Standard)), collapse = ", "), "}"),
    str_c("Units of Measure: {", str_c(as.character(unique(df$Units.of.Measure)), collapse = ", "), "}"),
    str_c("Event Type: {", str_c(as.character(unique(df$Event.Type)), collapse = ", "), "}")
  )
  
  writeLines(c(lines, extraLines), con = fileConn)
  close(fileConn)
  
  write.csv(dfToSave, str_c(outDir, fname, ".csv"), row.names = FALSE)
}

# 6b. daily datasets
logDF(dd, fname = "daily_full")
logDF(dd %>% filter(POC == 1), fname = "daily_full_poc1")

logDF(dd75, fname = "daily_75")
logDF(dd75 %>% filter(POC == 1), fname = "daily_75_poc1")

logDF(ddSeason75, fname = "daily_season75")
logDF(ddSeason75 %>% filter(POC == 1), fname = "daily_season75_poc1")

logDF(ddCompact75 %>% filter(POC == 1), fname = "dailyData_2000-2016")

# 6c. annual datasets
logDF(ad, fname = "annual_full")
logDF(ad %>% filter(POC == 1), fname = "annual_full_poc1")

logDF(ad75, fname = "annual_75")
logDF(ad75 %>% filter(POC == 1), fname = "annual_75_poc1")

logDF(adSeason75, fname = "annual_season75")
logDF(adSeason75 %>% filter(POC == 1), fname = "annual_season75_poc1")

logDF(adCompact75 %>% filter(POC == 1), fname = "annualData_2000-2016")