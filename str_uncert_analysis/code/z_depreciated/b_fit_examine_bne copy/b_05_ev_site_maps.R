# File: STR_d_02_make_training_predictions_JS.R
# Authors:
# Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 03/17/23
#
# Contents:
#  N. notes
#  0. Package Imports

#### ------------------ ####
####       N. notes     ####
#### ------------------ ####


#yyyy <- 2010


#### -------------- ####
#### 0. preparation ####
#### -------------- ####

# 0.a. load packages, etc
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

# 0.b. bring in conus shapefile 
conus <- sf::st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                                'conus.shp')) %>% 
  sf::st_transform(., crs=st_crs('epsg:4326'))

#### --------------- ####
#### 1. prep ev data ####
#### --------------- ####

EVdata <-   readr::read_csv(here::here('str_uncert_analysis', 
                              'data', 'external_validation', 'inputs', 
                              'ev_data_assigned_all.csv')) 

EVdata.loc <- EVdata %>% 
  dplyr::select(lat, lon) %>% 
  distinct() %>% 
  mutate(g = 1)

EVdata.grp <- EVdata %>% 
  mutate(yyyy = year(ddate)) %>% 
  group_by(name, yyyy) %>% 
  summarize(count = n(), 
            meanPM = mean(obs))
#EVdata.loc.list <- split(EVdata.loc, EVdata.loc$name)
  
#### ------------------------- ####
#### 2. loop to generate plots ####
#### ------------------------- ####

png(here::here(dir.proj, 'outputs', 'd_bne_results', 'external_validation', 
               paste0('evLoc_plot_', 'all', '.png')))
plotOneParameterSpatial(dta = EVdata.loc, 
                        parameterName = 'g', 
                        legYN = 'legN', 
                        mainTitle = "External Validation Sites", 
                        pointSize = 2, 
                        borderObj = conus)
dev.off()


for (i in 1:nrow(EVdata.loc)) {
  locName <- EVdata.loc$name[i]
  
  png(here::here(dir.proj, 'outputs', 'd_bne_results', 'external_validation', 
                 paste0('evLoc_plot_', locName, '.png')))
  plotOneParameterSpatial(dta = EVdata.loc[i,], 
                          parameterName = 'g', 
                          legYN = 'legN', 
                          mainTitle = locName, 
                          pointSize = 2, 
                          borderObj = conus)
  dev.off()
}
