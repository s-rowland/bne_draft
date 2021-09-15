# Compare Aditya and Lawrence Location Data
# BNE Crude Error Assessment 
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Wrangle Data
# 2: Identify Linear Relationships 
# 3: Check for Autocorrelation of the Residuals
# 4: Fit Spatial Model 
# 5: Assess Explanatory Power
# 6: Visualize Associations

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_set_up_env.R"))
}

if(!exists("Ran_a_00_ISEE")){
  here::i_am("README.md")
  source(here::here('uncertainty_factor_analysis_ISEE', 'scripts', 
                    "a_00_set_up_env_ISEE.R"))
}

# 0b Read CONUS shape file 
conus <-   st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                              'conus.shp'))

####******************
#### 1: Read Data ####
####******************

# 1a Read Data
readCombinedData <- function(YYYY) {
  read_fst(here::here('uncertainty_factor_analysis_ISEE', 'data', 'processed', 
                      paste0('AVGSCMJSCC_3.5_explanatory_', YYYY, '_015deg_final.fst'))) %>% 
    mutate(YYYY = YYYY)
}
dta <- map(2010:2015, readCombinedData) %>% 
  bind_rows()
rm(readCombinedData)

# 1b Convert units of population density to thousand ppl per km 
dta <- dta %>% 
  mutate(pop_density_1kper1000km2 = pop_density*1000000 / 1000) %>% 
  mutate(lat = as.numeric(as.character(lat)), 
         lon = as.numeric(as.character(lon)))

# 1d Average across years 
dta <- dta %>% 
  group_by(lat, lon) %>% 
  summarize_all(mean) %>% 
  ungroup() %>% 
  dplyr::select(-state, -region, -YYYY) %>% 
  mutate(cell_id = row_number()) 

dta.sf <- dta %>% 
  st_as_sf(coords = c('lon', 'lat'), crs=st_crs('epsg:4326')) %>% 
  st_transform(crs=st_crs(projString))
# 1e Scale all of the variables... yes! 
# Let's not scale, it makes it a bit harder to interpret the plots
#dta <- dta %>% 
# mutate_at(vars(-lat, -lon,  -stateNum), scale) 


# 2c.ii Readin state spatial data
states <- read_sf(here::here('ancillary_data', 'raw', 'Census','cb_2015_us_state_500k', 
                             'cb_2015_us_state_500k.shp'))
# 2c.iii Combine 
states <- states %>% 
  rename(state = STUSPS) %>%
  mutate(state_num = as.numeric(STATEFP)) %>% 
  dplyr::select(-AFFGEOID, -ALAND, -AWATER, -GEOID, -LSAD, -NAME, 
                -STATEFP, -STATENS)
states <- states %>% st_transform(crs=st_crs(projString))

states.ref <- computeIntersectionMax(dta.sf, states, 'state_num') 
states.ref <- states.ref %>% 
  mutate(state_um = round(state_num, 0)) 

states.ref <- states.ref %>% 
  inner_join(states, by ='state_num')

dta <- states.ref %>%
  as.data.frame() %>% 
  dplyr::select(-geometry) %>% 
  right_join(dta, by = 'cell_id')

dta.state.uncert <- dta %>% 
  group_by(state) %>%
  summarize(mean_uncert = mean(pred_sd)) %>% 
  arrange(desc(mean_uncert)) %>% 
  slice(1:5)
