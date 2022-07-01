# spBayes Tutorial
# Nationwide Application
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 
# Sept 12, 2021

####-----------------------
#### Table of Contents ####
####-----------------------

#  0: preparation 
#  1: bring in assigned bne ppd
#  2: set ingredients of model
#  3: fit model 
#  4: examine model

####--------------------
#### 0: preparation ####
####--------------------

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

# 0.c. loac objects for generating plots
if(!exists('ran_j_00')){
  here::i_am('README.md')
  source(here::here('str_uncert_analysis', 'code', 'j_generate_results_for_manuscript',
                    'j_00_set_plotting_features.R'))
}

# 0.b. load spatial package 
p_load(splm)

# 0.d. set seed 
# from tutorial
set.seed(1)

#### ------------------------------ ####
####  1: bring in assigned bne ppd  ####
#### ------------------------------ ####

# 1.a. function to read in the data
readAssignedPPD <- function(yyyy) {
  bne.ppd <- fst::read_fst(here::here(dir.proj, 'data', 'ppd_assigned', ppdPath, 
                                      paste0('bnePPD_expVar_', yyyy, '.fst'))) %>% 
    mutate(time = yyyy, cell_id = row_number(), 
           y_sd_scaled = y_sd/y_mean)
}

# 1.b. bring in all the years of assigned ppd
bne.ppd <- map_dfr(2010:2015, readAssignedPPD)

# 1.c. assign popd to be minimal value if it is na, since it is na for very low 
# densities
bne.ppd.nozero <- bne.ppd %>% 
  filter(pop_d != 0 )
pop_d.min <- min(bne.ppd.nozero$pop_d, na.rm = TRUE)

bne.ppd <- bne.ppd %>% 
    mutate(pop_d = if_else(is.na(pop_d), pop_d.min, pop_d)) %>% 
  mutate(pop_d = if_else(pop_d ==0, pop_d.min, pop_d))

  bne.ppd <- bne.ppd %>% 
    filter(complete.cases(.))
  
  
  bne.ppd <- bne.ppd %>% 
    mutate(pop_d = log(pop_d))
  
#### ----------------------------- ####
####  2: set ingredients of model  ####
#### ----------------------------- ####
  

# 3.c. create the objects we need to do our loop
covarDF <- data.frame(covarName = c('y_mean', 'pop_d', 'mon_dist', 'elev', 'temp_winter', 
                                    'temp_summer', 'wind_speed', 'precip', 'albedo', 
                                    'cloud_cover', 'boundary_h'), 
                      prop_dev = 999) %>% 
  mutate(covarTerm = paste0('s(', covarName, ')+'), 
         var_order = row_number())

  #### -------------------------------------- ####
  ####  3: Get Proportion Deviance Explained  ####
  #### -------------------------------------- ####  
  
# 3.a. create the empty model
  mod.empty <- gam(y_sd_scaled ~ te(lat, lon, time, k = c(15, 15, 6), bs = 'cr') + 
                     region*time, 
                  data = bne.ppd)
  
# 3.b. create the full model  
  mod.full <- gam(y_sd_scaled ~ s(y_mean) + s(mon_dist) + s(pop_d) + s(elev) +
                    s(temp_winter)+ s(temp_summer) + 
                    s(wind_speed)+ s(precip)+ s(albedo) + 
                    s(cloud_cover)+ s(boundary_h) + 
                    te(lat, lon, bs = 'ts') +
                    te(lat, lon, time, k = c(15, 15, 6), bs = 'cr') + 
                    region*time, 
                  data = bne.ppd)

  
  bne.ppd$residuals <- mod.full$residuals
  
  conus <- sf::st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                                  'conus.shp')) %>% 
    sf::st_transform(., crs=st_crs('epsg:4326'))
 TP <-  plotOneParameterSpatial(dta = filter(bne.ppd,time ==2011), parameterName = 'residuals', 
                          borderObj = conus,
                          mainTitle = '2011 Residuals for Absolute Uncertainty GAM\n with 5 df lat, 5 df long, 3 time')
  
  png(here::here(dir.proj, outPath, 'f_uncert_analysis', 'sdYScaled_gam_residuals_10lat_15lon.png'))
  print(TP)
  dev.off()
  
# 3.d. loop to calcualte proportion of deviance explained
for (i in 1:nrow(covarDF)) {
  
  covarName1 <- covarDF$covarName[i]
  covarDF.sm <- covarDF %>% 
    filter(covarName != covarName1)
  
  covariateStatement.partial <- paste0('y_sd_scaled ~ ',
                                       paste0(covarDF.sm$covarTerm, collapse = ''), 
                                       'te(lat, lon, time, k = c(15, 15, 6), bs =',  ' \'cr\'', ')+ region*time')
  
  mod.partial <- gam(as.formula(covariateStatement.partial), 
                     data = bne.ppd, sp = c(mod.full$sp[covarDF.sm$var_order], 
                                            mod.full$sp[12:14]))
  
  covarDF$prop_dev[i] <- (deviance(mod.partial) - deviance(mod.full) ) / deviance(mod.empty)
  
}

covarDF %>% write_csv(here::here(dir.proj, outPath, 'f_uncert_analysis', 
                                 'prop_dev_explained_ySDScaled_psp_sensitivity_5knots.csv'))


