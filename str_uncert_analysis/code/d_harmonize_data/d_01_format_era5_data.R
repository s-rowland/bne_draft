# File: c_01_format_era5_data.R
# Authors:
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 03/17/22
#
# Contents:
#  N. notes
#  0. preparation
#  1. create useful objects
#  2. determine which files need to be processed
#  3. format era5 data 

#### -------------- ####
#### 0. preparation ####
#### -------------- ####

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

# 0.c load rgdal
library(rgdal)

# 0.d define an interior function
getDailyMean <- function(dta, v, d) {
  
  # get only the relevant columns
  dta.day <- dta %>% 
    dplyr::select(contains(paste0(eraVar[v], '_', pad0(d))))
  
  # compute average
  dta.mean <- data.frame(value = apply(dta.day, 1, mean))
  
  # rename 
  names(dta.mean) <- paste0(eraVar[v], '_', pad0(d))
  
  # return result
  return(dta.mean)
}

#### -------------------------- ####
####  1. create useful objects  ####
#### -------------------------- ####

# 1.a define the variable included in each era5 dataset
# for land 
#'10m_u_component_of_wind', '10m_v_component_of_wind', '2m_dewpoint_temperature',
#'2m_temperature', 'forecast_albedo', 'surface_pressure',
# 'total_precipitation',
eraVarLand <- c('windU', 'windV', 'tDew', 'tMean', 'albedo', 'pres', 'precip')
# for singleLayer 
# '2m_dewpoint_temperature', '2m_temperature', 'surface_pressure',
# 'total_cloud_cover', 'uv_visible_albedo_for_diffuse_radiation',
# 'uv_visible_albedo_for_direct_radiation',
eraVarSingleLayer <- c('tDew2', 'tMean2', 'pres2', 'cloud', 'uvAlbedoDif', 'uvAlbedoDir')
# boundaryH
eraVarBoundaryH <- c('boundary_layer_height')

#### ----------------------------------------------- ####
####  2. determine which files need to be processed  ####
#### ----------------------------------------------- ####

# 2.a create dataframe of all of the possible year-month-dataset combinations
comboDF <- expand_grid(mm = pad0(1:12), 
                       yyyy = 2010:2015, 
                       eraSet = c('boundaryH', 'land', 'singleLayer'))

# 2.b  fix so that our file names use lower case boundaryH
comboDF <- comboDF %>% 
  mutate(eraSet2 = if_else(eraSet == 'boundaryH', 'BoundaryH', eraSet)) %>%
  mutate(file_name_raw = paste0(yyyy, '_', mm, '_', eraSet, '.grib'), 
         file_name_intermed = paste0(yyyy, '_', mm, '_', eraSet2, '.fst')) 
  

# 2.c identify era datasets that have not yet been downloaded
downloadedERA <- list.files(here::here(dir.proj, 'data', 'explanatory_variables', 
                                   'raw', 'era5'))
missing <- comboDF %>% filter(!file_name %in% downloadedERA)

# 2.d identify files that have not been completed 
completed.files <- list.files(here::here(dir.proj, 'data', 'explanatory_variables', 
                                         'intermediate', 'era5'))
comboDF <- comboDF %>% 
  filter(!file_name_intermed %in% completed.files)

# 2.e arrange by year
comboDF <- comboDF %>% 
  arrange(yyyy)

#### --------------------- ####
####  3. format era5 data  ####
#### --------------------- ####

for (i in 1:nrow(comboDF)) {
  
  # 3.a extract date info
  yyyy <- comboDF$yyyy[i]
  mm <- comboDF$mm[i]
  eraSet <- comboDF$eraSet[i]
  
  # 3.b identify relevant variables
  if (eraSet == 'land') {eraVar <- eraVarLand }
  if (eraSet == 'singleLayer') {eraVar <- eraVarSingleLayer }
  if (eraSet == 'boundaryH') {eraVar <- eraVarBoundaryH }
  
  # 3.c read dataset
  dta0 <- rgdal::readGDAL(here::here(dir.proj, 'data', 'explanatory_variables', 
                                    'raw', 'era5', 
                                    paste0(yyyy, '_', pad0(mm), '_', eraSet, '.grib'))) %>% 
    data.frame(.)  
  
  # 3.d temporary - shrink for speed 
  dta <- dta0 
   # slice_sample(prop = 0.1)
  
  # 3.e determine number of days in the month
  numDays <- (ncol(dta) -2) / (4*length(eraVar))
  
  # 3.f create a dataframe of day-variable combinations
  colNames.df.dd <- expand.grid(dd = pad0(1:numDays), era_var = eraVar)
  
  # 3.g add hours to the dataframe
  # we only collect four hourly samples
  colNames.df <- colNames.df.dd %>% 
    mutate(hh = 0) %>% 
    bind_rows(mutate(colNames.df.dd, hh = 6)) %>% 
      bind_rows(mutate(colNames.df.dd, hh = 12)) %>% 
      bind_rows(mutate(colNames.df.dd, hh = 18)) %>% 
      arrange(dd)
  colNames <- paste0(colNames.df$era_var, '_', colNames.df$dd, '_', colNames.df$hh)
  
  # 3.h assign column names 
  names(dta) <- c(colNames, 'lon', 'lat')
    
  # 3.i get daily means for each day of the month, for each variable
  dta.month <- purrr::pmap(list(list(dta), colNames.df.dd$era_var, colNames.df.dd$dd), 
                  getDailyMean ) %>% 
    bind_cols() %>% 
    bind_cols(dplyr::select(dta, lat, lon))
  
  # 3.j save
  dta.month %>% 
    fst::write_fst(here::here(dir.proj, 'data', 'explanatory_variables', 
                              'intermediate', 'era5', 
                              paste0(yyyy, '_', pad0(mm), '_', eraSet, '.fst')))

}

