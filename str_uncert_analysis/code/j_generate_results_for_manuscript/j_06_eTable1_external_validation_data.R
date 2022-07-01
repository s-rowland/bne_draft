# File: j_07_eTable1.R
# BNE Annual Uncertainty Analysis
# Authors:
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 10/04/2022
#
# Contents:
#  N. notes
#  0. preparation
#  1. bring in data
#  2. create table

#### ------------------ ####
####       N. notes     ####
#### ------------------ ####


#### ---------------- ####
####  0. preparation  ####
#### ---------------- ####

# 0.a. load packages, etc
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

# 0.b. load objects and packages specific for this work
if(!exists('ran_a_00_uncert')){
  here::i_am('README.md')
  source(here::here('str_uncert_analysis', 'code', 
                    '0_00_config_env_uncert_analysis.R'))
}

# 0.c. loac objects for generating plots
if(!exists('ran_j_00')){
  here::i_am('README.md')
  source(here::here('str_uncert_analysis', 'code', 'j_generate_results_for_manuscript',
                    'j_00_set_plotting_features.R'))
}

#### ------------------ ####
####  1. bring in data  ####
#### ------------------ ####

# 1.a brin in unassigned ev data
ev <- read_csv(here::here('str_uncert_analysis', 
                          'data', 'external_validation', 'inputs', 
                          'ev_data_unassigned_annual.csv'))

# 1.b calculate location summary stats
ev.name <- ev %>% 
  filter(yyyy >= 2010 & yyyy <=2015) %>%
  group_by(name) %>% 
    summarize(pm_mean = round(mean(obs), 2), 
              pm_sd = round(sd(obs), 2))

ev.loc <- ev %>% 
  filter(yyyy >= 2010 & yyyy <=2015) %>%
  group_by(name, lat, lon) %>% 
  summarize(pm_mean = round(mean(obs), 2), 
            pm_sd = round(sd(obs), 2))

#### ---------------------------------------------- ####
####  2. calculate distance to nearest aqs monitor  ####
#### ---------------------------------------------- ####

# 2.a get unique ev location as sf 
ev.sf <- ev.loc %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = plotCRS) %>% 
  st_transform(crs = projCRS)

# 2.b bring in trainign data 
aqs.sf <- read_csv(here::here('inputs', 'pm25', 'training_datasets','annual_combined', 
                           'training_cvfolds.csv')) %>% 
  dplyr::select(lat, lon) %>% 
  distinct() %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = plotCRS)%>% 
  st_transform(crs = projCRS)

plot(aqs.sf)
  
ev.loc$dist_mon <- nngeo::st_nn(ev.sf, aqs.sf, k = 1, returnDist  = TRUE)$dist

ev.name.dist <- ev.loc %>% 
  group_by(name) %>% 
  mutate(dist_mon= mean(as.numeric(dist_mon))) %>% 
  mutate(dist_mon_km = round(dist_mon/1000, 2))

a <- bind_rows(
  aqs.sf %>%mutate(type = 'aqs'), 
  ev.sf %>% mutate(type='ev')
)


ggplot(a) + 
  geom_sf(aes(color = type))
# 2.d save 
ev.name %>% 
  inner_join(ev.name.dist, by = 'name') %>%
  write_csv(here::here(dir.proj, 'manuscript', 'etable1_external_validation_data.csv'))
