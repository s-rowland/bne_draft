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
                      paste0('AVGSCMJSCC_3.5_explanatory_', YYYY, '_015deg.fst'))) %>% 
    mutate(YYYY = YYYY)
}
dta <- map(2010:2015, readCombinedData) %>% 
  bind_rows()
dta2010 <- readCombinedData(2010)
rm(readCombinedData)

# 1b Convert units of population density to thousand ppl per km 
dta <- dta %>% 
  mutate(popD1kper1km = popD*1000000 / 1000) %>% 
  mutate(lat = as.numeric(as.character(lat)), 
         lon = as.numeric(as.character(lon)))

# 1c Remove obs with missing data 
dta <- dta %>% 
  dplyr::select(-cell_id, -cellID) %>% 
  na.omit()

# 1d Average across years 
dta <- dta %>% 
  group_by(lat, lon) %>% 
  summarize_all(mean) %>% 
  ungroup() %>% 
  dplyr::select(-state, -region, -YYYY)

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
  mutate(stateNum = as.numeric(STATEFP)) %>% 
  dplyr::select(-AFFGEOID, -ALAND, -AWATER, -GEOID, -LSAD, -NAME, 
                -STATEFP, -STATENS)
dta <- states %>%
  as.data.frame() %>% 
  dplyr::select(-geometry) %>% 
  right_join(dta, by = 'stateNum')

####**************************************
#### 2: Identify Linear Relationships ####
####**************************************

# 2a Fit GAMM with all psp terms 
tic()
mod.allPsp <- gam(pred_sd ~ s(pred_mean) + s(monDist) + s(winterTemp) +
                     s(summerTemp) + s(popD1kper1km),
                   data = dta)
toc()
# 2b Review edf for potential linear terms 
summary(mod.allPsp$gam)

# No evidence for linear relationships, so we will stick with psp terms for now

####***************************************************
#### 3: Check for Autocorrelation of the Residuals ####
####***************************************************

# 3a Put the residuals in the dataframe 
dta$resid <- mod.allPsp$gam$residuals

# 3b Calculate Moran's I of the residuals 
# 3b.i Create distance matrix
dta.dists <- as.matrix(dist(cbind(dta$lon, dta$lat)))
# 3b.ii Compute the inverse of the distance
dta.dists.inv <- 1/dta.dists
# 3b.iii Make all of the diagonals (self-distance) zero
diag(dta.dists.inv) <- 0
# 3b.iv Calculate Moran's I 
Moran.I(dta$resid, dta.dists.inv)

# The spatial autocorrelation is significant, though not very high, so a spatial
# model is more appropriate
# option for spatial:  te(lat, lon, k = 5)
# even at k = 10 (which uses ~ 95 df), it still doesn't get rid of autocorrelation 

####*********************************
#### 5: Assess Explanatory Power ####
####*********************************
# this is the approach recommended by Wood.



mod.full <- gam(pred_sd ~ s(pred_mean) + s(monDist) + s(winterTemp) +
                     s(summerTemp) + s(popD1kper1km) + te(lat, lon, k = 5),
                   data = dta)

# 5a Fit each of the models
mod.noPredMean <- gam(pred_sd ~ s(monDist) + s(winterTemp) +
                  s(summerTemp) + s(popD1kper1km) + te(lat, lon, k = 5), 
                  sp = mod.full$gam$sp[2:7], 
                data = dta)

mod.noMonDist <- gam(pred_sd ~ s(pred_mean) + s(winterTemp) +
                       s(summerTemp) + s(popD1kper1km) + te(lat, lon, k = 5), 
                     sp = c(mod.full$gam$sp[1], mod.full$gam$sp[3:7]), 
                data = dta)
mod.noWinTemp <- gam(pred_sd ~ s(pred_mean) + s(monDist) +
                       s(summerTemp) + s(popD1kper1km) + te(lat, lon, k = 5),
                     sp = c(mod.full$gam$sp[1:2], mod.full$gam$sp[4:7]), 
                data = dta)
mod.noSumTemp <- gam(pred_sd ~ s(pred_mean) + s(monDist) + s(winterTemp) +
                       s(summerTemp) + s(popD1kper1km) + te(lat, lon, k = 5),
                     sp = c(mod.full$gam$sp[1:3], mod.full$gam$sp[5:7]), 
                data = dta)
mod.noPopD <- gam(pred_sd ~ s(pred_mean) + s(monDist) + s(winterTemp) +
                    s(summerTemp) + s(popD1kper1km) + te(lat, lon, k = 5),
                  sp = c(mod.full$gam$sp[1:4], mod.full$gam$sp[6:7]), 
                data = dta)
mod0 <- gam(pred_sd ~ 1+ te(lat, lon, k = 5), data = dta)

# Define function to calculate deviance 
calculatePropDevianceExplained <- function(activeModel){
  (deviance(activeModel)-deviance(mod.full))/deviance(mod0)
}

# Calculate proportion of deviance explained by each term 
propDev <- data.frame(
  var = c('pred_mean', 'monDist', 'winterTemp', 'summerTemp', 'popD'), 
  prop_dev = c(
    calculatePropDevianceExplained(mod.noPredMean), 
    calculatePropDevianceExplained(mod.noMonDist), 
    calculatePropDevianceExplained(mod.noWinTemp), 
    calculatePropDevianceExplained(mod.noSumTemp), 
    calculatePropDevianceExplained(mod.noPopD)))

# okay, this looks great! 
# I'm happy with this results
propDev %>% 
  write_csv(here::here('uncertainty_factor_analysis_ISEE', 'outputs',
                       'presentation', 'proportion_deviance_explained.png'))


####*******************************
#### 6: Visualize Associations ####
####*******************************

png(here::here('uncertainty_factor_analysis_ISEE', 'outputs','presentation', 
               'var_response_plot_mon_dist.png'))
plot(mod.full, rug = TRUE, select = 2, 
     xlab = 'Distance to Nearest Monitor (km)', 
     ylab = expression('Predictive Uncertainty ('*mu*g/m^3*')'))
dev.off()    

png(here::here('uncertainty_factor_analysis_ISEE', 'outputs','presentation', 
               'var_response_plot_pred_mean.png'))
plot(mod.full, rug = TRUE, select = 1, 
     xlab = expression('Predicted'[PM2.5]~'('*mu*g/m^3*')'), 
     ylab = expression('Predictive Uncertainty ('*mu*g/m^3*')'))
dev.off() 

png(here::here('uncertainty_factor_analysis_ISEE', 'outputs','presentation', 
               'var_response_plot_winter_temp.png'))
plot(mod.full, rug = TRUE, select = 3, 
     xlab = 'Wintertime Mean Temperature (\u00B0C)', 
     ylab = expression('Predictive Uncertainty ('*mu*g/m^3*')'))
dev.off() 

