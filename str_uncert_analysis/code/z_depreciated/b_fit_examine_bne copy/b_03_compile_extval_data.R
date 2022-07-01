# Evaluate Grid Search
# Fit Annual-CONUS BNE
# BNE Uncertainty Analysis 
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 
# Last updated Oct 24, 2021

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Calculate CV Metrics for Grid Search Models

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

if(!exists('ran_a_00_uncert')){
  here::i_am('README.md')
  source(here::here('str_uncert_analysis', 'code', 
                    '0_00_config_env_uncert_analysis.R'))
}

####****************************************************
#### 1: DASH####
####****************************************************

# 1a dash 
ev1 <- readr::read_csv(here::here('external_validation_data', 'pm25', 'daily', 'raw', 'DASH', 
                           'Sheet1-Table 1.csv'),)
ev1 <- ev1 %>% 
  janitor::clean_names()
ev1 <- ev1 %>% 
  slice(2:nrow(ev1))

ev1 <- ev1 %>% 
  rename( ddate = sample_date, obs = pmf_mass_conc) %>%
  filter(!is.na(obs)) %>%
  dplyr::select(ddate, obs) %>%
  mutate(ddate = parse_date_time(ddate, 'mdy'), obs = as.numeric(obs))

ev1$lat <- 39.7319728
ev1$lon <- -104.9273678
ev1$project <- 'DASH'
ev1$name <- 'Denver'

ev1 <- ev1 %>%
  dplyr::select(lat, lon, ddate, obs, project, name)

####****************************************************
#### 2: CCRUSH     ####
####****************************************************

# ALS school
ev.als <- read_csv(here::here('external_validation_data', 'pm25', 'daily', 'raw', 'CCRUSH', 
                              'ALS_compiled_filtered_DAILY_CSV.csv'))
ev.als <- ev.als %>% 
  janitor::clean_names()

ev.als <- ev.als %>% 
  rename(ddate = date, obs = a_calc) %>% 
  filter(!is.na(obs)) %>% 
  filter(obs != -999) %>% 
  dplyr::select(ddate, obs) %>%
  mutate(ddate = parse_date_time(ddate, 'mdy'), obs = as.numeric(obs))

ev.als$lat <- 39.8206863
ev.als$lon <- -104.9385061
ev.als$project <- 'CCRUSH'
ev.als$name <- 'ALS'

ev.als <- ev.als %>%
  dplyr::select(lat, lon, ddate, obs, project, name)

# EDI school
ev.edi <- read_csv(here::here('external_validation_data', 'pm25', 'daily', 'raw', 'CCRUSH', 
                              'EDI_compiled_filtered_DAILY_CSV.csv'))
ev.edi <- ev.edi %>% 
  janitor::clean_names()

ev.edi <- ev.edi %>% 
  rename(obs = a_calc, ddate = date) %>% 
  filter(!is.na(obs)) %>% 
  filter(obs != -999) %>% 
  dplyr::select(ddate, obs) %>%
  mutate(ddate = parse_date_time(ddate, 'mdy'), obs = as.numeric(obs))

ev.edi$lat <- 39.764869
ev.edi$lon <- -105.0420957
ev.edi$project <- 'CCRUSH'
ev.edi$name <- 'EDI' 

ev.edi <- ev.edi %>%
  dplyr::select(lat, lon, ddate, obs, project, name)

# MCA school
ev.mca <- read_csv(here::here('external_validation_data', 'pm25', 'daily', 'raw', 'CCRUSH', 
                              'MCA_compiled_filtered_DAILY_CSV.csv'))
ev.mca <- ev.mca %>% 
  janitor::clean_names()

ev.mca <- ev.mca %>% 
  rename(obs = a_calc, ddate = date) %>% 
  filter(!is.na(obs)) %>% 
  filter(obs != -999) %>% 
  dplyr::select(ddate, obs) %>%
  mutate(ddate = parse_date_time(ddate, 'mdy'), obs = as.numeric(obs))

ev.mca$lat <- 40.4282548
ev.mca$lon <- -104.7676221
ev.mca$project <- 'CCRUSH'
ev.mca$name <- 'MCA'  

ev.mca <- ev.mca %>%
  dplyr::select(lat, lon, ddate, obs, project, name)

# MAP school
ev.map <- read_csv(here::here('external_validation_data', 'pm25', 'daily', 'raw', 'CCRUSH', 
                              'MAP_compiled_filtered_DAILY_CSV.csv'))
ev.map <- ev.map %>% 
  janitor::clean_names()

ev.map <- ev.map %>% 
  rename(obs = a_calc, ddate = date) %>% 
  filter(!is.na(obs)) %>% 
  filter(obs != -999) %>% 
  dplyr::select(ddate, obs) %>%
  mutate(ddate = parse_date_time(ddate, 'mdy'), obs = as.numeric(obs))

ev.map$lat <- 40.4198079
ev.map$lon <- -104.7143578
ev.map$project <- 'CCRUSH'
ev.map$name <- 'MAP'

ev.map <- ev.map %>%
  dplyr::select(lat, lon, ddate, obs, project, name)

####****************************************************
#### 3: NYCCAS    ####
####****************************************************


ev.nyccas <- read_csv(here::here('external_validation_data', 'pm25', 'daily', 'raw', 'NYCCAS', 
                              'PM_raw_data-Table 1.csv'))
ev.nyccas <- ev.nyccas %>% 
  janitor::clean_names()


head(ev.nyccas)
ev.nyccas.day <-  foreach(
  i = 1:nrow(ev.nyccas), 
  .combine = 'rbind'
  
) %do% {

  date_start <- parse_date_time(ev.nyccas$pm_start_date[i], 'mdy HM')
  date_end <- parse_date_time(ev.nyccas$pm_end_date[i], 'mdy HM')
  date_seq <- seq(date_start, date_end, by = 'day')
  
  dta <- data.frame(ddate = date_seq, 
                    lat = ev.nyccas$latitude[i], 
                    lon = ev.nyccas$longitude[i], 
                    obs = ev.nyccas$blk_corr_pm_ugm3[i])
}

ev.nyccas.day$lat <- as.numeric(ev.nyccas.day$lat)
ev.nyccas.day$lon <- as.numeric(ev.nyccas.day$lon)
ev.nyccas.day$project <- 'nyccas'
ev.nyccas.day$name <- 'nyc'


####****************************************************
#### 4: SEARCH   ####
####****************************************************

# 4a set up grid of all values of the SEARCH data 
searchGrid <- expand_grid(yyyy = 2005:2015, 
                          siteName = c('BHM', 'CTR', 'JST', 'OLF', 'YRK'))

ev.search <- foreach(
    i = 1:nrow(searchGrid), 
    .combine = 'rbind'
  ) %do% {
#for (i in 1:55){
    yyyy <- searchGrid$yyyy[i]
    siteName <- searchGrid$siteName[i]
    
    if (yyyy < 2008) {
      dta <- readxl::read_excel(here::here('external_validation_data', 'pm25', 'daily', 'raw', 'SEARCH', 
                                           yyyy, 
                                           paste0(siteName, 'PMData', yyyy, 'Public.xls')), 
                                na = '-999', 
                                #skip = 2,
                                range = cellranger::cell_cols('A:B'),
                                col_types = c('date', 'text'))
      dta <- dta[3:nrow(dta),]
      names(dta) <- c('sample_date', 'frm_mass')
      
    } else if (yyyy == 2008) {
      dta <- readxl::read_excel(here::here('external_validation_data', 'pm25', 'daily', 'raw', 'SEARCH', 
                                           yyyy, 
                                           paste0(siteName, '-PM2.5 Mass and Composition-', yyyy, ' Part 1.xlsx')), 
                                na = '-999') %>% 
        bind_rows(readxl::read_excel(here::here('external_validation_data', 'pm25', 'daily', 'raw', 'SEARCH', 
                                                yyyy, 
                                                paste0(siteName, '-PM2.5 Mass and Composition-', yyyy, ' Part 2.xlsx')), 
                                     na = '-999') )
    } else if (yyyy > 2008 & yyyy < 2012) {
      dta <- readxl::read_excel(here::here('external_validation_data', 'pm25', 'daily', 'raw', 'SEARCH', 
                                           yyyy, 
                                           paste0(siteName, '-PM2.5 Mass and Composition-0101', 
                                                  str_sub(yyyy, 3, 4), '-1231', str_sub(yyyy, 3, 4), '.xlsx')), 
                                na = '-999') %>% 
        janitor::clean_names() 
      if (siteName %in% c('JST', 'YRK') | 
          (siteName == 'OLF' & yyyy != 2011) | 
          (siteName == 'CTR' & yyyy == 2011)) {
        dta <- dta %>% rename(frm_pm2_5_mass_ug_m3 = pm2_5_mass_ug_m3) 
        }
        dta <- dta %>% rename(frm_mass = frm_pm2_5_mass_ug_m3)
    } else if (yyyy %in% c( 2012)) {
      dta <- readxl::read_excel(here::here('external_validation_data', 'pm25', 'daily', 'raw', 'SEARCH', 
                                           yyyy, 
                                           paste0(siteName, '-PM25 Mass and Composition-0101', 
                                                  str_sub(yyyy, 3, 4), '-1231', str_sub(yyyy, 3, 4), '.xlsx')), 
                                na = '-999')  %>% 
        janitor::clean_names() %>% 
        rename(frm_mass = frm_pm2_5_mass_ug_m3)
    } else if ( yyyy >= 2013) {
      dta <- readxl::read_excel(here::here('external_validation_data', 'pm25', 'daily', 'raw', 'SEARCH', 
                                           yyyy, 
                                           paste0(siteName, '-PM25 Mass and Composition-TwentyFourHour-1_1_', 
                                                  yyyy, '-12_31_', yyyy, '.xlsx')), 
                                na = '-999')  %>% 
        janitor::clean_names() %>% 
        rename(frm_mass = frm_pm2_5_mass_ug_m3)

    }
    
    dta <- dta %>% 
      janitor::clean_names() %>% 
      rename(obs = frm_mass, ddate = sample_date) %>%
      filter(!is.na(obs)) %>% 
      filter(!is.null(obs)) %>%
      dplyr::select(ddate, obs) %>%
      mutate(ddate = parse_date_time(ddate, 'ymd'))

  
  if (siteName == 'BHM') {
    dta$lat <- 33.55300; dta$lon <- -86.81500
  } 
  if (siteName == 'CTR') {
    dta$lat <- 32.90200; dta$lon <- -87.25000
  } 
  if (siteName == 'JST') {
    dta$lat <- 33.77600; dta$lon <- -84.41300
  } 
  if (siteName == 'OLF') {
    dta$lat <- 30.55100; dta$lon <- -87.37600
  } 
  if (siteName == 'YRK') {
    dta$lat <- 33.93100; dta$lon <- -85.04600
  } 
    
    dta$project <- 'SEARCH'
    dta$name <- siteName
dta <- dta %>% 
  dplyr::select(lat, lon, ddate, obs, project, name)
  

}

ev.search <- ev.search %>% 
  mutate(obs = as.numeric(obs)) %>% 
  filter(!is.na(obs))

####**********************
#### 4: Saint Regis   ####
####**********************

ev.stmr <- read.csv(here::here('external_validation_data', 'pm25', 'daily', 'raw', 
                    'saint_regis_mohawk', '24-hour-Table 1.csv'))

ev.stmr <- ev.stmr %>% 
  clean_names()

ev.stmr <- ev.stmr %>% 
  rename(obs = sample_value, ddate = date) %>% 
  filter(!is.na(obs)) %>% 
  dplyr::select(obs, ddate) %>%
  mutate(ddate = parse_date_time(ddate, 'ymd'))

ev.stmr$lat <- 44.98
ev.stmr$lon <- -74.69
  
ev.stmr$project <- 'Saint Regis Mohawk'
ev.stmr$name <- 'Saint Lawrence County'



####**********************
#### 5: Merge all of them ####
####**********************

EVdata <- bind_rows(ev.als, ev.edi, ev.map, ev.mca, ev.stmr, ev1 , ev.search, 
                    ev.nyccas.day)

EVdata %>%
  mutate(yyyy = year(ddate)) %>% 
  filter(yyyy >=2005 & yyyy <= 2016) %>% 
  dplyr::select(-yyyy) %>%
  filter(!is.na(lat)) %>% 
  write_csv(here::here('str_uncert_analysis', 
                                'data', 'external_validation', 'inputs', 
                                'ev_data_unassigned.csv'))



