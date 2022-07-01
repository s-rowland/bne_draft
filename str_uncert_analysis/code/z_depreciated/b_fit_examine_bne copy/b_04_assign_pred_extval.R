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

#fileNames <- list.files(here::here('inputs', 'pm25', 'training_datasets', 'daily_yearly'))[[7]]

EVdata <- read_csv(here::here('str_uncert_analysis', 
                                'data', 'external_validation', 'inputs', 
                                'ev_data_unassigned.csv')) %>% 
  mutate(yyyy = year(ddate), doy = yday(ddate)) %>% 
  mutate(yyyy_doy = paste0(yyyy, '_', doy))

EVdata1 <- EVdata %>% 
  filter(yyyy <= 2010)
EVdata1.ls <- split(EVdata1, EVdata1$yyyy_doy)

n.cores <- 7
# 0.e.ii create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "FORK"
)
#check cluster definition (optional)
print(my.cluster)

# 0.e.iii register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
#check if it is registered (optional)
foreach::getDoParRegistered()


EVdata.assigned1 <-  foreach(
  ev = EVdata1.ls,
  .combine = 'rbind'
  
) %dopar% {
   # bring in annual training data
  preds <- read_csv(here::here('inputs', 'pm25', 'prediction_datasets', 'daily_individual', 
                      paste0('preds_', ev$yyyy[1], '_', str_pad(ev$doy[1], 3, 'left', 0), '.csv')))
  
  # make preds spatial
  preds <- preds %>% st_as_sf(coords = c('lon', 'lat'))
  
  # make ev spatial
  ev.sf <- ev %>% st_as_sf(coords = c('lon', 'lat'))
  
  # find the nearest neighbors
  a <- unlist(nngeo::st_nn(ev.sf, preds))
  
  # assign the exposures
  ev$day_of_year <- preds$day_of_year[a]
  ev$julian_day <- preds$julian_day[a]
  ev$pred_av <- preds$pred_av[a]
  ev$pred_cm <- preds$pred_cm[a]
  ev$pred_js <- preds$pred_js[a]
  ev$pred_me <- preds$pred_me[a]
  ev$pred_rk <- preds$pred_rk[a]
  
 # return assigned ev
  ev
}

EVdata.assigned1 %>% 
  readr::write_csv(here::here('str_uncert_analysis', 
                              'data', 'external_validation', 'inputs', 
                              'ev_data_assigned1.csv'))

EVdata2 <- EVdata %>% 
  filter(yyyy > 2010)
EVdata2.ls <- split(EVdata2, EVdata2$yyyy_doy)

EVdata.assigned2 <-  foreach(
  ev = EVdata2.ls,
  .combine = 'rbind'
  
) %dopar% {
  # bring in annual training data
  preds <- read_csv(here::here('inputs', 'pm25', 'prediction_datasets', 'daily_individual', 
                               paste0('preds_', ev$yyyy[1], '_', str_pad(ev$doy[1], 3, 'left', 0), '.csv')))
  
  # make preds spatial
  preds <- preds %>% st_as_sf(coords = c('lon', 'lat'))
  
  # make ev spatial
  ev.sf <- ev %>% st_as_sf(coords = c('lon', 'lat'))
  
  # find the nearest neighbors
  a <- unlist(nngeo::st_nn(ev.sf, preds))
  
  # assign the exposures
  ev$day_of_year <- preds$day_of_year[a]
  ev$julian_day <- preds$julian_day[a]
  ev$pred_av <- preds$pred_av[a]
  ev$pred_cm <- preds$pred_cm[a]
  ev$pred_js <- preds$pred_js[a]
  ev$pred_me <- preds$pred_me[a]
  ev$pred_rk <- preds$pred_rk[a]
  
  # return assigned ev
  ev
}

EVdata.assigned2 %>% 
  readr::write_csv(here::here('str_uncert_analysis', 
                              'data', 'external_validation', 'inputs', 
                              'ev_data_assigned2.csv'))


EVdata <-  readr::read_csv(here::here('str_uncert_analysis', 
                                       'data', 'external_validation', 'inputs', 
                                       'ev_data_assigned1.csv')) %>% 
  bind_rows(readr::read_csv(here::here('str_uncert_analysis', 
                                       'data', 'external_validation', 'inputs', 
                                       'ev_data_assigned2.csv')) )

EVdata %>% 
  dplyr::select(lat, lon, julian_day, day_of_year, 
                pred_av, pred_cm, pred_js, pred_me, pred_rk, obs) %>%
  write_csv(here::here('str_uncert_analysis', 
                       'data', 'external_validation', 'inputs', 
                       'ev_data_assigned_all.csv'))
