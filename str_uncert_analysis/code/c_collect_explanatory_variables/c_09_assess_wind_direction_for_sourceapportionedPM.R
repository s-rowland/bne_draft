# Task: Assess consistency of wind direction to determine if we can use the 
# source-apportioned PM2.5 data from 2017. 
# File: c_05_assess_wind_direction_for_sourceapportionedPM.R
# SubProject: Analysis of BNE PM2.5 Predictive Uncertainty
# Project: Bayesian Nonparametric Ensemble 
# Author: Sebastian T. Rowland <sr3463@cumc.columbia.edu>

#### ------------------- ####
####  Table of Contents  ####
#### ------------------- ####

# N: Notes
# 0: Preparation 
# 1: Bring in Wind Data 
# 2: Assign EPA Region 
# 3: Assess Correlation

#### ---------------- ####
####  0: Preparation  ####
#### ---------------- ####

# 0.a. import relevant packages, etc, based on whole project
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up',
                    'a_00_import_packages_set_global_objects.R'))
}

# 0.b. import packages and set objects specific to this subproject
if(!exists('ran_a_00_uncert')){
  here::i_am('README.md')
  source(here::here('str_uncert_analysis', 'code', 
                    '0_00_config_env_uncert_analysis.R'))
}

#### ----------------------- ####
####  1: Bring in Wind Data  ####
#### ----------------------- ####

# 1.a. bring in 2015 and 2017 wind data 
wind.2015 <- readr::read_csv(here::here(dir.proj, 'data', 'ancillary_data', 
                                 'nldas_wind_direction_2015.csv')) %>% 
  dplyr::mutate(index = row_number()) %>%
  dplyr::filter(!is.na(wind_dir_from))
wind.2017 <- readr::read_csv(here::here(dir.proj, 'data', 'ancillary_data', 
                                 'nldas_wind_direction_2017.csv')) %>% 
  dplyr::mutate(index = row_number()) %>%
  dplyr::filter(!is.na(wind_dir_from))

# 1.c. make sure they only have matching observations 
wind.2015 <- wind.2015 %>% 
  filter(index %in% wind.2017$index)
wind.2017 <- wind.2017 %>% 
  filter(index %in% wind.2015$index)

# 1.d. combine 
# 1.d.i. rename variables in preparation for join
wind.2015 <- wind.2015 %>% 
  rename(lon = long, mm = nldas_month, 
         zonal_2015 = zonal_wind_speed, merid_2015 = merid_wind_speed,
         speed_2015 = abs_wind_speed, angle_2015 = wind_dir_from) %>% 
  dplyr::select(lon, lat, mm, zonal_2015, merid_2015,  speed_2015, angle_2015)
wind.2017 <- wind.2017 %>% 
  rename(lon = long, mm = nldas_month, 
         zonal_2017 = zonal_wind_speed, merid_2017 = merid_wind_speed,
         speed_2017 = abs_wind_speed, angle_2017 = wind_dir_from) %>% 
  dplyr::select(lon, lat, mm, zonal_2017, merid_2017,  speed_2017, angle_2017)

# 1.d.ii combine 
wind <- inner_join(wind.2015, wind.2017, by = c('lat', 'lon', 'mm'))

wind <- wind %>% 
  mutate(angle_cos_2015 = cos(2*pi*angle_2015/360), 
         angle_cos_2017 = cos(2*pi*angle_2017/360))

# another check - this time we can also consider the magnitude and location 
wind <- wind %>% 
  mutate(dist = sqrt((zonal_2015 - zonal_2017)^2 + (merid_2015 - merid_2017)^2))

cor(wind$angle_cos_2015, wind$angle_cos_2017)

cor(wind$speed_2015, wind$speed_2017)
wind <- wind %>% 
  mutate(angle_diff = 360*cospi(angle_cos_2015 - angle_cos_2017) / (2*pi))

mean(abs(wind$angle_diff))
sd(wind$angle_diff)


# still pretty bad. 
# okay, now I'm willing to give up. 
# okay, this doesn't make sense. Ought to just ask Johnathan for a datset. 

wind.high <- wind %>% 
  filter(speed_2015 > 4 | speed_2017 > 4)

cor(wind.high$angle_cos_2015, wind.high$angle_cos_2017)
check_corr <- function(dta) {
  cor(dta$angle_cos_2015, dta$angle_cos_2017)
  
}

map(split(wind, wind$mm), check_corr)

#### ---------------------- ####
####  2: Assign EPA Region  ####
#### ---------------------- ####

# 2.a. make epa region simple feature 
# 2.a.i. bring in state shapefile 
states <- st_read(here::here('ancillary_data', 'raw', 'census', 
                             'cb_2015_us_state_500k', 'cb_2015_us_state_500k.shp')) %>% 
  janitor::clean_names()
# 2.a.ii bring in epa region - state key 
key.states.region <- read_csv(here::here('ancillary_data', 'generated', 
                                         'epa_regions.csv')) %>% 
  filter(!state %in% c('AK', 'HI', 'PR')) %>% 
  filter(!is.na(state))
# 2.a.iii combine 
regions <- states %>% 
  inner_join(key.states.region, by = c('stusps' = 'state'))

# 2.b. a

#### ----------------------- ####
####  3: Assess Correlation  ####
#### ----------------------- ####


