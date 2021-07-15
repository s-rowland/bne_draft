# Join Daily AQS and MERRA
# Assess Daily Input Models 
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Wrangle AQS Observations
# 2: Wrangle MERRA Estimates
# 3: Keep Only MERRA at AQS Monitors
# 4: Join AQS and MERRA

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_0_00")){
  # change this to something .md when I can 
  here::i_am("README.rtf")
  source(here::here('Scripts', '0_set_up', "0_00_setUp_env.R"))
}

# 0b Readin Conus
conus <- st_read(here::here('Data', 'general', 'spatial_outlines', 'conus.shp'), 
                 crs = projString)
conus.sp <- as_Spatial(conus)

####*********************************
#### 1: Wrangle AQS Observations ####
####*********************************

# 1a Read AQS observations
aqs <- read_csv(here::here('BNE_Inputs', 'b_02_AQS_processed', 'daily', 
                       'daily_aqs_cleaned.csv'), col_types = 'dddcc')

# 1b Make a simple features dataframe of just the monitor locations
monitors <- aqs %>% 
  dplyr::select(lat,lon, latlon) %>%
  distinct() %>%
  st_as_sf(., coords = c("lon", "lat"),
           crs=st_crs("epsg:4326"))

# 1c Change projections
monitors <- st_transform(monitors, crs=st_crs(projString))

# 1d Restrict to conus
monitors <- monitors %>% 
  st_join(conus, st_intersects) %>% 
  filter(!is.na(g)) %>% 
  dplyr::select(-g, -m)

# 1e Make a WGS84 version of monitors to extract with MERRA, 
# which is WGS84
mon.sf.wgs <- st_transform(monitors, crs=st_crs('epsg:4326'))
monitors.sp <- as_Spatial(mon.sf.wgs)

####********************************
#### 2: Wrangle MERRA Estimates ####
####********************************

# 2a Read MERRA estimates
m14 <- brick("BNE_Inputs/a_01_inputModels_original/MERRA/daily_2010adjPM25sum_OM2OC14.nc")
#m20 <- brick("BNE_Inputs/a_01_inputModels_original/MERRA/daily_2010adjPM25sum_OM2OC20.nc")

# 2b Reproject raster
#crs(m14) <- crs(projStringRas)
#crs(m20) <- crs(projStringRas)

####****************************************
#### 3: Keep Only MERRA at AQS Monitors ####
####****************************************

# 3a Extract MERRA estimates at monitor locations
m14.mon <- raster::extract(m14, monitors.sp, df=TRUE)

# 3b Wrangle dataframe
m14.mon <- m14.mon %>% 
  dplyr::select(-ID)
names(m14.mon) <- paste0('M14','_',  c(0:(ncol(m14.mon)-1)))

# 3c Combine 
m14.mon$latlon <- monitors$latlon
m14.mon <- m14.mon %>% 
  separate(latlon,c('lat', 'lon'), '_') %>% 
  mutate(lat = as.numeric(lat), lon = as.numeric(lon))

# 3d Put in long format 
m14.mon.long <- m14.mon%>% 
  gather('dIndex', 'M14', -lat, -lon)

# 3e Convert estimates to ug/m^3
m14.mon.long <- m14.mon.long %>% 
  mutate(M14 = M14 * 10^9)

# 3f ConvertdIndex to date 
m14.mon.long <- m14.mon.long %>% 
  mutate(dIndex = as.numeric(str_sub(dIndex, 5))) %>%
  mutate(date = parse_date_time('1/1/2010', 'mdy', tz = 'America/Los_Angeles') + 
           dIndex*24*60*60 + 60*60) %>% 
  mutate(date = str_sub(as.character(date), 0, 10)) %>% 
  dplyr::select(-dIndex)

# 3g Clean up environment 
rm(m14, m14.mon, monitors.sp, mon.sf.wgs, monitors)

####***************************
#### 4: Join AQS and MERRA ####
####***************************

# 4a Join 
dta <- aqs %>% 
  inner_join(m14.mon.long, by = c('lat', 'lon', 'date'))

# 4b Save dataset
dta %>% 
  write_csv(here::here('BNE_Inputs', 'b_02_AQS_processed', 'daily', 
                       'daily_aqs_M14.csv'))