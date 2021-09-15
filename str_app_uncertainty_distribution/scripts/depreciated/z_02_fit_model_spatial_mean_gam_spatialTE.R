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
readCombinedData <- function(YYYY){
  read_fst(here::here('uncertainty_factor_analysis_ISEE', 'data', 'processed', 
                      paste0('AVGSCMJSCC_3.5_explanatory_', YYYY, '_025deg.fst'))) %>% 
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

# 1c Remove obs with missing data 
#dta <- dta %>% 
  #dplyr::select(-cell_id) %>% 
 # na.omit()

# 1d Average across years 
dta <- dta %>% 
  group_by(lat, lon) %>% 
  summarize_all(mean) %>% 
  ungroup() %>% 
  dplyr::select(-state, -region, -YYYY)

# 1e Scale all of the variables... yes! 
# Let's not scale, it makes it a bit harder to interpret the plots
dta <- dta %>% 
 mutate_at(vars(-lat, -lon,  -stateNum), scale) 


# 2c.ii Readin state spatial data
states <- read_sf(here::here('ancillary_data', 'raw', 'Census','cb_2015_us_state_500k', 
                             'cb_2015_us_state_500k.shp'))
# 2c.iii Combine 
states <- states %>% 
  rename(state = STUSPS) %>%
  mutate(state_num = as.numeric(STATEFP)) %>% 
  dplyr::select(-AFFGEOID, -ALAND, -AWATER, -GEOID, -LSAD, -NAME, 
                -STATEFP, -STATENS)
dta <- states %>%
  as.data.frame() %>% 
  dplyr::select(-geometry) %>% 
  right_join(dta, by = 'state_num')


dta <- dta %>% 
  dplyr::select(-zcta) 
dta <- dta %>% 
  filter(complete.cases(dta))


dta <- dta %>% 
  mutate(monDist1 = monDist1.mean, 
         monDist2 = 2*monDist2.mean-monDist1.mean, 
         monDist3 = 3*monDist3.mean-2*monDist2.mean, 
         monDist4 = 4*monDist4.mean-3*monDist3.mean, 
         monDist5 = 5*monDist5.mean-4*monDist4.mean)
cor(dta$monDist1, dta$monDist2)
####**************************************
#### 2: Identify Linear Relationships ####
####**************************************

# 2a Fit GAMM with all psp terms 
tic()
mod.allPsp <- gam(pred_sd ~ s(pred_mean) + #s(monDist5.mean) + 
                    s(monDist4) +  s(monDist1) +# s(monDist3) + s(monDist4) +
                    
                    
                    s(winter_temp) +
                    s(summer_temp) + s(pop_density_1kper1000km2) + 
                    s(cloud_cover) + s(elev) + te(lat, lon, k = 15),
                   data = dta)
toc()
# 2b Review edf for potential linear terms 
summary(mod.allPsp)

plot(mod.allPsp)
# there we go, that's actually the answer. 
# there was a hidden residual prcoess - the other missing monitors. 



# No evidence for linear relationships, except for pop density 

####***************************************************
#### 3: Check for Autocorrelation of the Residuals ####
####***************************************************

# 3a Put the residuals in the dataframe 
dta$resid <- mod.allPsp$residuals

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
# option for spatial:  te(lat, lon, k=5)
# even at k = 10 (which uses ~ 95 df), it still doesn't get rid of autocorrelation 

####*********************************
#### 5: Assess Explanatory Power ####
####*********************************
# this is the approach suggested by Wood. 
# Not my favorite because I worry the tensor product for space is explaining a lot of the distribution
# This sort of model is also called a 'geoadditive model' 

# Like, increaing the the k eats all of the coefficinets. 
# like, if we use  k= 15, then monitor distance gets really big, but nothing else does. 
# note that k= 5 as a default. 

mod.full<- gam(pred_sd ~ s(pred_mean) + s(monDist1.mean) + s(winter_temp) +
                     s(summer_temp) + s(pop_density_1kper1000km2) + 
                  s(cloud_cover) + s(elev) +
                  te(lat, lon, k=15),
                   data = dta)


# note

# 5a Fit each of the models
mod.noPredMean <- gam(pred_sd ~ s(monDist1.mean) + s(winter_temp) +
                  s(summer_temp) + s(pop_density_1kper1000km2) +
                    s(cloud_cover) + s(elev) + te(lat, lon, k=15),
                  sp = mod.full$sp[2:9], 
                data = dta)

mod.noMonDist <- gam(pred_sd ~ s(pred_mean) + s(winter_temp) +
                       s(summer_temp) + s(pop_density_1kper1000km2) +
                       s(cloud_cover) + s(elev) + te(lat, lon, k=15),
                     sp = c(mod.full$sp[1], mod.full$sp[3:9]), 
                data = dta)
mod.noWinTemp <- gam(pred_sd ~ s(pred_mean) + s(monDist1.mean) +
                       s(summer_temp) + s(pop_density_1kper1000km2) + 
                       s(cloud_cover) + s(elev) + te(lat, lon, k=15),
                     sp = c(mod.full$sp[1:2], mod.full$sp[4:9]), 
                data = dta)
mod.noSumTemp <- gam(pred_sd ~ s(pred_mean) + s(monDist1.mean) + s(winter_temp) +
                       s(pop_density_1kper1000km2) + 
                      s(cloud_cover) + s(elev) + te(lat, lon, k=15),
                     sp = c(mod.full$sp[1:3], mod.full$sp[5:9]), 
                data = dta)
mod.noPopD <- gam(pred_sd ~ s(pred_mean) + s(monDist1.mean) + s(winter_temp) +
                    s(summer_temp) +
                     s(cloud_cover) + s(elev) + te(lat, lon, k=15),
                  sp = c(mod.full$sp[1:4], mod.full$sp[6:9]), 
                data = dta)

mod.noCloud <- gam(pred_sd ~ s(pred_mean) + s(monDist1.mean) + s(winter_temp) +
                  s(summer_temp) + s(pop_density_1kper1000km2) + 
                    s(elev) + te(lat, lon, k=15),
                  sp = c(mod.full$sp[1:5], mod.full$sp[7:9]), 
                data = dta)

mod.noElev <- gam(pred_sd ~ s(pred_mean) + s(monDist1.mean) + s(winter_temp) +
                     s(summer_temp) + s(pop_density_1kper1000km2) + 
                    s(cloud_cover) + te(lat, lon, k=15),
                   sp = c(mod.full$sp[1:6], mod.full$sp[8:9]), 
                   data = dta)

mod0 <- gam(pred_sd ~  te(lat, lon), 
            sp = c(mod.full$sp[8:9]), 
            data = dta)

# Define function to calculate deviance 
# I think this is wrong...
calculatePropDevianceExplained <- function(activeModel){
  (deviance(activeModel)-deviance(mod.full))/deviance(activeModel)
}

# Calculate proportion of deviance explained by each term 
propDev <- data.frame(
  var = c('pred_mean', 'mon_dist', 'winter_temp', 'summer_temp', 'pop_density', 
           'cloud', 'elev'), 
  prop_dev = c(
    calculatePropDevianceExplained(mod.noPredMean), 
    calculatePropDevianceExplained(mod.noMonDist), 
    calculatePropDevianceExplained(mod.noWinTemp), 
    calculatePropDevianceExplained(mod.noSumTemp), 
    calculatePropDevianceExplained(mod.noPopD), 
    calculatePropDevianceExplained(mod.noCloud), 
    calculatePropDevianceExplained(mod.noElev))) %>% 
  mutate(prop_dev = round(prop_dev, 3))


mod.full <- gam(pred_sd ~ s(pred_mean) + s(monDist5.mean) + s(winter_temp) +
                  s(summer_temp) + s(pop_density_1kper1000km2) + 
                  s(cloud_cover) + s(elev) +
                  te(lat, lon, k=6),
                data = dta)
mod.noMonDist <- gam(pred_sd ~ s(pred_mean) + s(winter_temp) +
                       s(summer_temp) + s(pop_density_1kper1000km2) +
                       s(cloud_cover) + s(elev) + te(lat, lon, k=6),
                     sp = c(mod.full$sp[1], mod.full$sp[2:8]), 
                     data = dta)

calculatePropDevianceExplained(mod.noMonDist)


mod.full <- gam(pred_sd ~ s(pred_mean) + s(monDist5.mean) + s(winter_temp) +
                  s(summer_temp) + s(pop_density_1kper1000km2) + 
                  s(cloud_cover) + s(elev)+ te(lat, lon, np = TRUE),
                data = dta)
mod.noSpace <- gam(pred_sd ~ s(pred_mean) +s(monDist5.mean)+ s(winter_temp) +
                       s(summer_temp) + s(pop_density_1kper1000km2) +
                       s(cloud_cover) + s(elev),
                     sp = c(mod.full$sp[1], mod.full$sp[2:7]), 
                     data = dta)

calculatePropDevianceExplained(mod.noSpace)
calculatePropDevianceExplained(mod.noMonDist)


# okay, now the linear and non--linear yield very simialr propDevianceExplaiend. GREAT 

mod.full$sp

# okay, this looks great! 
# I'm happy with this results
propDev %>% 
  write_csv(here::here('uncertainty_factor_analysis_ISEE', 'outputs',
                       'presentation', 'proportion_deviance_explained.csv'))


mod <- lm(pred_sd~monDist1.mean, data = dta)
summary(mod)
mod
p_load(rsq)
rsq.partial(mod)
# let's just like retry this with linear terms? 
# 
cor(dta$monDist1.mean, dta$pred_sd)

####*******************************
#### 6: Visualize Associations ####
####*******************************


mod.active <- gam(pred_sd ~ s(monDist5.mean) + te(lat, lon, k=5),
                  data = dta)
png(here::here('uncertainty_factor_analysis_ISEE', 'outputs','presentation', 
               'var_response_plot_mon_dist.png'))
par(mar = c(5, 5, 5,5))
plot(mod.active, rug = TRUE, select = 1, 
     xlab = 'Distance to Nearest Monitor (km)', 
     ylab = expression('Predictive Uncertainty ('*mu*g/m^3*')'), 
     main = 'Distance to Nearest Monitor and Uncertainty', 
     cex.lab = 1.4, cex.axis = 1.4, cex.main = 1.4)
dev.off()    

mod.active <- gam(pred_sd ~ s(pred_mean)  + te(lat, lon, k=5),
                  data = dta)
png(here::here('uncertainty_factor_analysis_ISEE', 'outputs','presentation', 
               'var_response_plot_pred_mean.png'))
par(mar = c(5, 5, 5,5))
plot(mod.active, rug = TRUE, select = 1, 
     xlab = expression('Estimated PM'[2.5]~'('*mu*g/m^3*')'), 
     ylab = expression('Predictive Uncertainty ('*mu*g/m^3*')'), 
     main = expression('Estimated PM'[2.5]~' and Uncertainty'), 
     cex.lab = 1.4, cex.axis = 1.4, cex.main = 1.4)
dev.off() 

mod.active <- gam(pred_sd ~ s(winter_temp) + te(lat, lon, k=5),
                  data = dta)
png(here::here('uncertainty_factor_analysis_ISEE', 'outputs','presentation', 
               'var_response_plot_winter_temp.png'))
par(mar = c(5, 5, 5,5))
plot(mod.active, rug = TRUE, select = 1, 
     xlab = 'Winter-Time Mean Temperature (\u00B0C)', 
     ylab = expression('Predictive Uncertainty ('*mu*g/m^3*')'), 
     main = 'Winter-Time Mean Temperature and Uncertainty', 
     cex.lab = 1.4, cex.axis = 1.4, cex.main = 1.4)
dev.off() 

mod.active <- gam(pred_sd ~ s(summer_temp) + te(lat, lon, k=5),
                  data = dta)
png(here::here('uncertainty_factor_analysis_ISEE', 'outputs','presentation', 
               'var_response_plot_summer_temp.png'))
par(mar = c(5, 5, 5,5)) 
plot(mod.active, rug = TRUE, select = 1, 
     xlab = 'Summer-Time Mean Temperature (\u00B0C)', 
     ylab = expression('Predictive Uncertainty ('*mu*g/m^3*')'), 
     main = 'Summer-time Mean Temperature and Uncertainty', 
     cex.lab = 1.4, cex.axis = 1.4, cex.main = 1.4)
dev.off() 
