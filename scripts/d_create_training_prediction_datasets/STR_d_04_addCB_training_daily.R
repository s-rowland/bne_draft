# File: LGC_d_02a_make_training_data.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 08/21/21
#
# Contents:
# 0. Package Imports & Global Variables
# 1. EPA & JS Daily Data
# 2. Setting Up CMAQ, GS, CACES loop
# 3. CMAQ, GS, CACES loop
# 4. AV data loop
# 5. Save outputs

##** ------------------------------------- **##
#### 0. PACKAGE IMPORTS & GLOBAL VARIABLES ####
#### ------------------------------------- ####

# 0a. load packages and functions required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_config_env.R"))
}

# 0.b. install Cole's package 
#p_load(remotes)
#remotes::install_github("geomarker-io/addPmData")
library(addPmData)

##** ------------------------------------- **##
#### 1. Modified PM-downloading function ####
#### ------------------------------------- ####
#' add PM2.5 concentrations to geocoded data based on h3 geohash
#'
#' @param d dataframe with columns called 'lat', 'lon', 'start_date' and 'end_date'
#' @param type either 'coords' (if d contains lat/lon) or 'h3' (if d contains
#'             . resolution 8 h3 ids)
#' @param verbose if TRUE a statement is printed to the console telling the user
#'                which chunk file is currently being processed. Defaults to FALSE.
#' @param ... arguments passed to \code{\link[s3]{s3_get_files}}
#'
#' @return the input dataframe, expanded to include one row per day between the given 'start_date'
#'         and 'end_date', with appended columns for h3_3 (resolution 3), h3 (resolution 8),
#'         year, pm_pred, and pm_se.
#'
#' @examples
#' if (FALSE) {
#' d <- tibble::tribble(
#'      ~id,         ~lat,    ~lon, ~start_date,    ~end_date,
#'      '55000100280', 39.2, -84.6, '2008-09-09', '2008-09-11',
#'      '55000100281', 39.2, -84.6, '2007-08-05', '2007-08-08',
#'      '55000100282', 39.2, -84.6, '2015-08-31', '2015-09-02') %>%
#'    dplyr::mutate(dplyr::across(c(start_date, end_date), as.Date))
#'
#'    add_pm(d)
#' }
#' @export
add_pm_dlOnly <- function(d, type = 'coords', verbose = FALSE, ...) {
  d <- prep_data(d, type)
  
  # dates - expand, extract year, filter out of range
  d <- dht::expand_dates(d, by = 'day')
  d$year <- lubridate::year(d$date)
  out_of_range_year <- sum(d$year < 2000 | d$year > 2020)
  if (out_of_range_year > 0) {
    cli::cli_alert_warning("Data is currently available from 2000 through 2020.")
    cli::cli_alert_info(glue::glue("PM estimates for {out_of_range_year} rows will be NA due to unavailable data.\n"))
    d_missing_date <- dplyr::filter(d, !year %in% 2000:2020)
    d <- dplyr::filter(d, year %in% 2000:2020)
  }
  
  # coords - filter out missing, match to h3 ids
  if(type == 'coords'){
    n_missing_coords <- nrow(d %>% dplyr::filter(is.na(lat) | is.na(lon)))
    if (n_missing_coords > 0) {
      cli::cli_alert_warning(glue::glue("PM estimates for {n_missing_coords} rows will be NA due to missing coordinates in input data.\n"))
      d_missing_coords <- dplyr::filter(d, is.na(lat) | is.na(lon))
      d <- dplyr::filter(d, !is.na(lat), !is.na(lon))
    }
    
    message('matching lat/lon to h3 cells...')
    d$h3 <- suppressMessages(h3jsr::point_to_h3(dplyr::select(d, lon, lat), res = 8))
  } else n_missing_coords <- 0
  
  # h3 res 3 - and match to safe harbor
  d$h3_3 <- h3jsr::get_parent(d$h3, res = 3)
  
  d <- d %>%
    dplyr::left_join(safe_hex_lookup, by = 'h3_3') %>%
    dplyr::mutate(h3_3 = ifelse(!is.na(safe_hex), safe_hex, h3_3)) %>%
    dplyr::select(-safe_hex)
  
  # h3 availability  - check and filter
  ## h3 ids that do not exist within the scope of this model
  invalid_h3 <- sum(!d$h3_3 %in% safe_harbor_h3)
  if (invalid_h3 > 0) {
    cli::cli_alert_info(glue::glue("PM estimates for {invalid_h3} rows will be NA due to invalid h3 ids.\n"))
    d_invald_h3 <- dplyr::filter(d, !d$h3_3 %in% safe_harbor_h3)
    d <- dplyr::filter(d, d$h3_3 %in% safe_harbor_h3)
  }
  
  ## h3 ids belonging to large aggregate chunks that are currently unavailable
  d <- d %>%
    dplyr::mutate(n_aggregated = stringr::str_count(d$h3_3, "-"))
  
  unavailble_h3 <- d %>%
    dplyr::filter(n_aggregated > 2) %>%
    nrow(.)
  
  if (unavailble_h3 > 0) {
    cli::cli_alert_warning("This package is under development. Available data is currently limited, but will be available nationwide soon.\n")
    cli::cli_alert_info(glue::glue("PM estimates for {unavailble_h3} rows will be NA due to unavailable data.\n"))
    d_unavailable_h3 <- dplyr::filter(d, n_aggregated > 2) %>%  dplyr::select(-n_aggregated)
    d <- dplyr::filter(d, n_aggregated < 3)
  }
  d <- dplyr::select(d, -n_aggregated)
  
  message('downloading PM chunk files...')
  pm_chunks <-
    glue::glue("s3://pm25-brokamp/{d$h3_3}_{d$year}_h3pm.fst") %>%
    unique() %>%
    s3::s3_get_files(...) %>%
    get_unique_h3_3_year()
  
  d_split <- split(d, f = list(d$h3_3, d$year), drop=TRUE)
  
  a <- 1
  return(a)
}

#### ------------------------- ####
####  1. READ IN training data  ####
#### ------------------------- ####

# 1.a. get training data 
trainingData <-
  readr::read_csv(here::here('BNE_inputs', 'training_datasets', 'yearly_daily', 
                              paste0('training_avcmjsme_', 2010, '_all.csv')))
# 1.b. add variables to TrainingData that we will need for add_pm function
trainingData <- trainingData %>% 
  mutate(start_date = paste0("2010-", month, '-', day)) %>% 
  mutate(end_date = start_date) %>% 
  mutate(id = paste0(ref_id, '_', start_date)) %>% 
  mutate(chunk = row_number() %% 2000) 
  
# 1.c. break up training data into list to prepare for looping
trainingData.list <- split(trainingData, trainingData$chunk)

# 1.d. function to add cd to chunk of training data
add_cb <- function(dta) {
  dtaPlusCB <- dta %>%
    dplyr::select(id, lat, lon, start_date, end_date) %>%
    add_pm(.) %>% 
    dplyr::select(pm_pred, id) %>% 
    rename(pred_cd = pm_pred) %>% 
    inner_join(dta, by = 'id')
  return(dtaPlusCB)
}

# 1.e. loop over list to add data 
# 1.e.i start the dataframe we will fill out
trainingDataPlusCB <- add_cb(trainingData.list[[1]])
# 1.f. loop! 
for (i in 2:4) {
  trainingDataPlusCB <- trainingDataPlusCB %>% 
    bind_rows(add_cb(trainingData.list[[i]]))
}

dgk <- trainingData.list[[3]]
dgk <- dgk %>% 
  mutate(id2 = row_number() %% 5) %>% 
  filter(id2 ==4) %>% 
  mutate(id3 = row_number())



a <- split(dgk, dgk$id3)

for (i in 1:length(a)) {
  print(i)
  bbq <- add_cb(a[[i]])
}

test <- dgk[1,]
test$lon <-  test$lon + 0.01
a <- add_cb(test)

for (i in 2: length(trainingData.list)) {
  trainingDataPlusCB <- trainingDataPlusCB %>% 
    bind_rows(add_cb(trainingData.list[[i]]))
}


a <- dgk %>% filter(id2 == 4)


for (i in 1: length(trainingData.lis))
dta <- trainingData.list[[1]] %>%
  dplyr::select(id, lat, lon, start_date, end_date) %>%
  add_pm(.) %>% 
  dplyr::select(pm_pred, id) %>% 
  rename(pred_cd = pm_pred) %>% 
  inner_join(trainingData.list[[1]], by = 'id')




# 1.b. keep only the variables of interest
trainingData.loc <- trainingData %>% 
  mutate(start_date = paste0("2010-", month, '-', day)) %>% 
  mutate(end_date = start_date) %>% 
  mutate(id = paste0(ref_id, '_', start_date)) %>% 
  mutate(group_id = row_number() %% 20000) %>%
  dplyr::select(id, lat, lon, start_date, end_date, group_id) 

# 1.c. convert to list to speed up processing 
trainingData.loc.list <- split(trainingData.loc, trainingData.loc$group_id)

# 1.d. assign values
a <- list()
#a <- add_pm(dplyr::select(trainingData.loc.list [[i]], -group_id))

for ( i in 1:2){
  a[i] <- list(add_pm(dplyr::select(trainingData.loc.list [[i]], -group_id)))
}


