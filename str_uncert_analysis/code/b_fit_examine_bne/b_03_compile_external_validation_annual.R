# File: b_03_compile_external_validation_data.R
# Authors:
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 03/17/22
#
# Contents:
#  0. preparation
#  1: process DASH 
#  2: process CCRUSH 
#  3: process NYCCAS
#  4: process SEARCH 
#  5: process Saint Regis
#  6: merge ev datasets

#### ---------------- ####
####  0. preparation  ####
#### ---------------- ####

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

#### ----------------- ####
####  1: process DASH  ####
#### ----------------- ####

# 1.a bring in DASH data
ev1 <- readr::read_csv(here::here('external_validation_data', 'pm25', 'daily', 'raw', 'DASH', 
                           'Sheet1-Table 1.csv'))

# 1.b make proper column names
ev1 <- ev1 %>% 
  janitor::clean_names()

# 1.c remove uninformative row
ev1 <- ev1 %>% 
  slice(2:nrow(ev1))

# 1.d more processing
ev1 <- ev1 %>% 
  rename( ddate = sample_date, obs = pmf_mass_conc) %>%
  filter(!is.na(obs)) %>%
  dplyr::select(ddate, obs) %>%
  mutate(ddate = parse_date_time(ddate, 'mdy'), obs = as.numeric(obs))

# 1.e manually add the location and project name
ev1$lat <- 39.7319728
ev1$lon <- -104.9273678
ev1$project <- 'DASH'
ev1$name <- 'Denver'

# 1.f curate variables
ev1 <- ev1 %>%
  dplyr::select(lat, lon, ddate, obs, project, name)

#### ------------------- ####
####  2: process CCRUSH  ####
#### ------------------- ####

# 2.a ALS school
# 2.a.i bring in ALS school data
ev.als <- read_csv(here::here('external_validation_data', 'pm25', 'daily', 'raw', 'CCRUSH', 
                              'ALS_compiled_filtered_DAILY_CSV.csv'))

# 2.a.ii make proper column names
ev.als <- ev.als %>% 
  janitor::clean_names()

# 2.a.iii more processing
ev.als <- ev.als %>% 
  rename(ddate = date, obs = a_calc) %>% 
  filter(!is.na(obs)) %>% 
  filter(obs != -999) %>% 
  dplyr::select(ddate, obs) %>%
  mutate(ddate = parse_date_time(ddate, 'mdy'), obs = as.numeric(obs))

# 2.a.iv manually add the location and project name
ev.als$lat <- 39.8206863
ev.als$lon <- -104.9385061
ev.als$project <- 'CCRUSH'
ev.als$name <- 'ALS'

# 2.a.v curate variables
ev.als <- ev.als %>%
  dplyr::select(lat, lon, ddate, obs, project, name)

# 2.b EDI school
# 2.b.i bring in EDI school data
ev.edi <- read_csv(here::here('external_validation_data', 'pm25', 'daily', 'raw', 'CCRUSH', 
                              'EDI_compiled_filtered_DAILY_CSV.csv'))

# 2.b.ii make proper column names
ev.edi <- ev.edi %>% 
  janitor::clean_names()

# 2.b.iii more processing
ev.edi <- ev.edi %>% 
  rename(obs = a_calc, ddate = date) %>% 
  filter(!is.na(obs)) %>% 
  filter(obs != -999) %>% 
  dplyr::select(ddate, obs) %>%
  mutate(ddate = parse_date_time(ddate, 'mdy'), obs = as.numeric(obs))

# 2.b.iv manually add the location and project name
ev.edi$lat <- 39.764869
ev.edi$lon <- -105.0420957
ev.edi$project <- 'CCRUSH'
ev.edi$name <- 'EDI' 

# 2.b.v curate variables
ev.edi <- ev.edi %>%
  dplyr::select(lat, lon, ddate, obs, project, name)

# 2.c MCA school
# 2.c.i bring in MCA school data
ev.mca <- read_csv(here::here('external_validation_data', 'pm25', 'daily', 'raw', 'CCRUSH', 
                              'MCA_compiled_filtered_DAILY_CSV.csv'))
# 2.c.ii make proper column names
ev.mca <- ev.mca %>% 
  janitor::clean_names()

# 2.c.iii more processing
ev.mca <- ev.mca %>% 
  rename(obs = a_calc, ddate = date) %>% 
  filter(!is.na(obs)) %>% 
  filter(obs != -999) %>% 
  dplyr::select(ddate, obs) %>%
  mutate(ddate = parse_date_time(ddate, 'mdy'), obs = as.numeric(obs))

# 2.c.iv manually add the location and project name
ev.mca$lat <- 40.4282548
ev.mca$lon <- -104.7676221
ev.mca$project <- 'CCRUSH'
ev.mca$name <- 'MCA'  

# 2.c.v curate variables
ev.mca <- ev.mca %>%
  dplyr::select(lat, lon, ddate, obs, project, name)

# 2.d MAP school
# 2.d.i bring in MAP school data
ev.map <- read_csv(here::here('external_validation_data', 'pm25', 'daily', 'raw', 'CCRUSH', 
                              'MAP_compiled_filtered_DAILY_CSV.csv'))
# 2.d.ii make proper column names
ev.map <- ev.map %>% 
  janitor::clean_names()

# 2.d.iii more processing
ev.map <- ev.map %>% 
  rename(obs = a_calc, ddate = date) %>% 
  filter(!is.na(obs)) %>% 
  filter(obs != -999) %>% 
  dplyr::select(ddate, obs) %>%
  mutate(ddate = parse_date_time(ddate, 'mdy'), obs = as.numeric(obs))

# 2.d.iv manually add the location and project name
ev.map$lat <- 40.4198079
ev.map$lon <- -104.7143578
ev.map$project <- 'CCRUSH'
ev.map$name <- 'MAP'

# 2.d.v curate variables
ev.map <- ev.map %>%
  dplyr::select(lat, lon, ddate, obs, project, name)

#### ------------------- ####
####  3: process NYCCAS  ####
#### ------------------- ####

# 3.a read in data
ev.nyccas <- read_csv(here::here('external_validation_data', 'pm25', 'daily', 'raw', 'NYCCAS', 
                              'PM_raw_data-Table 1.csv'))

# 3.b make proper column names
ev.nyccas <- ev.nyccas %>% 
  janitor::clean_names()


head(ev.nyccas)
# 3.c reformat data to correctly extract dates
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

# 3.d convert to numeric
ev.nyccas.day$lat <- as.numeric(ev.nyccas.day$lat)
ev.nyccas.day$lon <- as.numeric(ev.nyccas.day$lon)

# 3.e assign project name 
ev.nyccas.day$project <- 'nyccas'
ev.nyccas.day$name <- 'nyc'

#### ------------------- ####
####  4: process SEARCH  ####
#### ------------------- ####

# 4.a set up grid of all values of the SEARCH data 
searchGrid <- expand_grid(yyyy = 2005:2015, 
                          siteName = c('BHM', 'CTR', 'JST', 'OLF', 'YRK'))

# 4.b bring in all of the SEARCH data, accounting for inconsistencies
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

#### ------------------------ ####
####  5: process Saint Regis  ####
#### ------------------------ ####

# 5.a bring in data
ev.stmr <- read.csv(here::here('external_validation_data', 'pm25', 'daily', 'raw', 
                    'saint_regis_mohawk', '24-hour-Table 1.csv'))

# 5.b create proper column names
ev.stmr <- ev.stmr %>% 
  clean_names()

# 5.c some more data wrangling
ev.stmr <- ev.stmr %>% 
  rename(obs = sample_value, ddate = date) %>% 
  filter(!is.na(obs)) %>% 
  dplyr::select(obs, ddate) %>%
  mutate(ddate = parse_date_time(ddate, 'ymd'))

# 5.d manually add location and project name
ev.stmr$lat <- 44.98
ev.stmr$lon <- -74.69
ev.stmr$project <- 'Saint Regis Mohawk'
ev.stmr$name <- 'Saint Lawrence County'

#### ---------------------- ####
####  6: merge ev datasets  ####
#### ---------------------- ####

# 6.a bring together the dataets
EVdata <- bind_rows(ev.als, ev.edi, ev.map, ev.mca, ev.stmr, ev1 , ev.search, 
                    ev.nyccas.day)

# 6.b curate and then save
EVdata %>%
  mutate(yyyy = year(ddate)) %>% 
  filter(yyyy >=2005 & yyyy <= 2016) %>% 
  dplyr::select(-yyyy) %>%
  filter(!is.na(lat)) %>% 
  write_csv(here::here('str_uncert_analysis', 
                                'data', 'external_validation', 'inputs', 
                                'ev_data_unassigned.csv'))
