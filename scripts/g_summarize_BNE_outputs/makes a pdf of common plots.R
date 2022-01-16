# Join Daily AQS and MERRA
# Assess Daily Input Models 
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: 

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_config_env.R"))
}

# 0b Readin Conus
conus <- st_read(here::here('data_ancillary', 'formatted', 'spatial_outlines', 
                            'conus.shp'))

####*****************************
#### 1: Set model parameterNames ####
####****************************


YYYY <- 2010
lenScaleSpace <- 1.5
lenScaleTime <- 'spatialBNE'
baseModelSet <- c('av', 'gs', 'cm', 'js', 'cc')
fold <- 'all'
residualProcess = 'resid'

if(residualProcess == 'resid'){resid <- ''}
if(residualProcess == 'noResid'){resid <- '_noResid'}


if (fold %in% c('NYC', 'Boston', 'Chicago', 'LA')){
  predFold <- 'cities'
} else if (str_detect(fold, 'feat') | fold == 'NYS') {
  predFold <- fold
} else {predFold <- 'all'}

####*****************************
#### 1: Wrangle BNE Outputs ####
####****************************

# 1c get model for 2011
bne.out <-  readBNEoutput(YYYY = YYYY, 
                          baseModelSet = baseModelSet,
                          lenScaleSpace = lenScaleSpace, 
                          lenScaleTime = lenScaleTime,
                          fold = predFold, 
                          residualProcess = residualProcess)

# 1d TEMPORARY 
# drop 50% of observations to speed up plotting 
bne.out <- bne.out %>% 
 slice_sample(prop = 0.8)

bne.out <- bne.out %>% 
  mutate(city = case_when(
    lon < -100 ~ 'LA', 
    lon > -100 & lon < -85 ~ 'Chicago', 
    lat > 41.4 & lon > -73 ~ 'Boston',
    lon > -80 ~ 'NYC')) 

if (fold %in% bne.out$city){bne.out <- bne.out %>% filter(city == !!fold)}



predMean.vec <- c(bne.out$w_sd_av, bne.out$w_sd_gs, bne.out$w_sd_cm,
                  bne.out$w_sd_js, bne.out$w_sd_cc)
p.min <- min(predMean.vec); p.max <- max(predMean.vec)
p.1 <- round(p.max*0.25, 2); p.2 = round(p.max*0.5, 2); p.3 = round(p.max*0.75, 2)
valueVec.wSD <- c(round(p.min,2), round(p.1,2),  round(p.2,2), round(p.3,2), round(p.max,2))

# readin AQS monitors 
aqs <- read_csv(here::here('BNE_inputs', 'training_datasets', 'combined_annual', 
                           'training_avgscmjscc_all.csv')) %>% 
  filter(year == YYYY) %>% 
  filter(lat > min(bne.out$lat) & lat < max(bne.out$lat) & 
           lon > min(bne.out$lon) & lon < max(bne.out$lon))

bne.out <- bne.out %>% 
  mutate(w_dist_av = abs(w_mean_av - 0.2), 
         w_dist_gs = abs(w_mean_gs - 0.2),
         w_dist_cm = abs(w_mean_cm - 0.2),
         w_dist_js = abs(w_mean_js - 0.2),
         w_dist_cc = abs(w_mean_cc - 0.2))

####********************************
#### 2: Create BNE Output Plots ####
####********************************

dir.proj <- 'str_testing'


# 2a Begin PDf
pdf(here::here(dir.proj, 'outputs', 
               paste0('selected_bne_',lenScaleSpace, '_', lenScaleTime, '_', 
                      YYYY, '_', fold, resid, '.pdf')))

# 2b weights
cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne.out, parameterName = 'w_mean_av',
                          mainTitle = 'Weight of Aaron Von Donkelaar', 
                          extraPointObj = aqs), 
  plotOneParameterSpatial(dta = bne.out, parameterName = 'w_mean_gs', 
                          mainTitle = 'Weight of Global Burden of Disease', 
                          extraPointObj = aqs),
  plotOneParameterSpatial(dta = bne.out, parameterName = 'w_mean_cm',
                          mainTitle = 'Weight of CMAQ-Fusion',
                          extraPointObj = aqs),
  plotOneParameterSpatial(dta = bne.out, parameterName = 'w_mean_js', 
                          mainTitle = 'Weight of Joel Schwartz 2019', 
                          extraPointObj = aqs),
  plotOneParameterSpatial(dta = bne.out, parameterName = 'w_mean_cc', 
                          mainTitle = 'Weight of CACES', 
                          extraPointObj = aqs),
  ncol = 2, nrow = 3)

#dev.off()






cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne.out, parameterName = 'w_dist_av',
                          mainTitle = 'Weight of Aaron Von Donkelaar Relative to Prior', pointSize = 2.0,
                          extraPointObj = aqs), 
  plotOneParameterSpatial(dta = bne.out, parameterName = 'w_dist_gs', 
                          mainTitle = 'Weight of Global Burden of Disease Relative to Prior', pointSize = 2.0,
                          extraPointObj = aqs),
  plotOneParameterSpatial(dta = bne.out, parameterName = 'w_dist_cm',
                          mainTitle = 'Weight of CMAQ-Fusion Relative to Prior', pointSize = 2.0,
                          extraPointObj = aqs),
  plotOneParameterSpatial(dta = bne.out, parameterName = 'w_dist_js', 
                          mainTitle = 'Weight of Joel Schwartz 2019 Relative to Prior', pointSize = 2.0,
                          extraPointObj = aqs),
  plotOneParameterSpatial(dta = bne.out, parameterName = 'w_dist_cc', 
                          mainTitle = 'Weight of CACES Relative to Prior', pointSize = 2.0,
                          extraPointObj = aqs),
  ncol = 2, nrow = 3)

vec.wSD <- c(bne.out$w_sd_av, bne.out$w_sd_gs, bne.out$w_sd_cm, bne.out$w_sd_js, bne.out$w_sd.cc)
p.min <- round(min(dta$p), 2); p.max <- round(max(dta$p), 2)
valueVec.wSD  <- c(p.min, 
                   round(p.min + 0.25*(p.max - p.min), 2), 
                   round(p.min + 0.50*(p.max - p.min), 2), 
                   round(p.min + 0.75*(p.max - p.min), 2), 
                   p.max)

cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne.out, parameterName = 'w_sd_av',
                          mainTitle = 'Uncertainty of Weight of Aaron Von Donkelaar', 
                          valueScale = valueVec.wSD, pointSize = 2.0,
                          extraPointObj = aqs), 
  plotOneParameterSpatial(dta = bne.out, parameterName = 'w_sd_gs', 
                          mainTitle = 'Uncertainty of Weight of Global Burden of Disease', 
                          valueScale = valueVec.wSD, pointSize = 2.0,
                          extraPointObj = aqs),
  plotOneParameterSpatial(dta = bne.out, parameterName = 'w_sd_cm',
                          mainTitle = 'Uncertainty of Weight of CMAQ-Fusion', 
                          valueScale = valueVec.wSD, pointSize = 2.0,
                          extraPointObj = aqs),
  plotOneParameterSpatial(dta = bne.out, parameterName = 'w_sd_js', 
                          mainTitle = 'Uncertainty of Weight of Joel Schwartz 2019', 
                          valueScale = valueVec.wSD, pointSize = 2.0,
                          extraPointObj = aqs),
  plotOneParameterSpatial(dta = bne.out, parameterName = 'w_sd_cc', 
                          mainTitle = 'Uncertainty of Weight of CACES', 
                          valueScale = valueVec.wSD, pointSize = 2.0,
                          extraPointObj = aqs),
  ncol = 2, nrow = 3)

cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne.out, parameterName = 'ens_mean', 
                          mainTitle = 'Model Combination', pointSize = 2.0,
                          extraPointObj = aqs),
  plotOneParameterSpatial(dta = bne.out, parameterName = 'ens_sd',
                          mainTitle = 'Uncertainty of Model Combination', pointSize = 2.0,
                          extraPointObj = aqs), 
  ncol = 1, nrow = 2)

cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne.out, parameterName = 'res_mean', 
                          mainTitle = 'Estimated Residual Process', 
                          extraPointObj = aqs),
  plotOneParameterSpatial(dta = bne.out, parameterName = 'res_sd',
                          mainTitle = 'Uncertainty of Residual Process', 
                          extraPointObj = aqs), 
  ncol = 1, nrow = 2)

cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne.out, parameterName = 'pred_mean', pointSize = 2.0),
  plotOneParameterSpatial(dta = bne.out, parameterName = 'pred_sd', pointSize = 2.0, extraPointObj = aqs), 
  ncol = 1, nrow = 2)

dev.off()
              



