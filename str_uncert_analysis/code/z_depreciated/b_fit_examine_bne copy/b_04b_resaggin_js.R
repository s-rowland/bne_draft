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


my.cluster <- parallel::makeCluster(
  6, 
  type = "FORK"
)
#check cluster definition (optional)
print(my.cluster)

# 0.e.iii register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
#check if it is registered (optional)
foreach::getDoParRegistered()

# bring in EVdata
EVdata <-   readr::read_csv(here::here('str_uncert_analysis', 
                                       'data', 'external_validation', 'inputs', 
                                       'ev_data_assigned_all.csv')) 

EVdata <- EVdata %>% 
  mutate(ddate = parse_date_time('2005-01-01', 'ymd') + julian_day* 24*60*60) %>% 
  mutate(yyyy = year(ddate)) %>% 
  filter(yyyy == 2016)

EVdata <- EVdata %>% 
  dplyr::select(-ddate, -yyyy)
# split the EV data by day 
EVdata.list <- split(EVdata, EVdata$julian_day)

# identify relevant JS indicies
js.indicies <- read_csv(here::here('external_validation_data', 
                     'pm25', 'daily',
                     'js_index.csv'))


ev.reassigned <-  foreach(
  ev = EVdata.list,
  .combine = 'rbind'
  
) %dopar% {
  
 # ev.list <- list()
 for (i in 1: length(EVdata.list)){
    print(i)
ev <- EVdata.list[[i]]
  ddate <- parse_date_time('2005-01-01', 'ymd') + ev$julian_day[1]* 24*60*60
  yyyy <- year(ddate)
  mm <- pad0(month(ddate))
  dd <- pad0(day(ddate))
  doy = yday(ddate)
  
  infix <- paste0(yyyy, mm)
  
  # 3E.b bring in js 

  js <- readRDS(here::here('inputs', 'pm25', 'base_models', 'daily', 'raw', 'js', 
                           'USGridSite.rds')) %>% 
    rename(lat = Lat, lon = Lon)
  
  
  js$pred_js <- as.vector(readRDS(here::here('inputs', 'pm25', 'base_models', 'daily', 'raw', 'js',  infix, 
                                         paste0('PredictionStep2_PM25_USGrid_', 
                                         yyyy, mm, dd, '_', yyyy, mm, dd, '.rds'))))
  


  js <- js %>% slice(js.indicies$js_index)  
      
      # make both spatial 
      js <- js %>% st_as_sf(coords = c('lon', 'lat'))
      
      ev.sf <- ev %>% st_as_sf(coords = c('lon', 'lat'))
      
      a <- unlist(nngeo::st_nn(ev.sf, js))
      
      ev$pred_js <- js$pred_js[a]
    
  # return re-assigned dataframe
 ev.list[[i]] <- ev
 gc()
}


ev.reassigned <- bind_rows(ev.list )
ev.reassigned %>% 
  rename(percent_of_year = day_of_year) %>%
  readr::write_csv(here::here('str_uncert_analysis', 
                              'data', 'external_validation', 'inputs', 
                              'ev_data_assigned_all.csv'))

