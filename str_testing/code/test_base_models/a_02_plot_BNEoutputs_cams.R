# Plot BNE Outputs
# Examine BNE Outputs
# BNE Uncertainty Analysis 
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 
# Last updated Oct 24, 2021

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Plot Weights
# 2: Plot Highest-Weighted Model 
# 3: Plot Uncertainty

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_set_up_env.R"))
}

if(!exists("Ran_a_00_uncert")){
  here::i_am("README.md")
  source(here::here('str_uncert_analysis', 'code', 
                    "0_00_set_up_env_uncert_analysis.R"))
}

####*********************
#### 1: Plot Weights ####
####*********************

# 1a get bne outputs
bne.conus.2010 <-  readBNEoutput(YYYY = 2010, 
                          inputSet = c('av', 'gs', 'cm', 'js', 'cc', 'ca'),
                          lenScaleSpace = 1.5, 
                          lenScaleTime = 'spatialOnly',
                          fold = 'all')
bne.conus.2011 <-  readBNEoutput(YYYY = 2011, 
                                 inputSet = c('av', 'gs', 'cm', 'js', 'cc', 'ca'),
                                 lenScaleSpace = 1.5, 
                                 lenScaleTime = 'spatialOnly',
                                 fold = 'all')
bne.conus.2012 <-  readBNEoutput(YYYY = 2012, 
                                 inputSet = c('av', 'gs', 'cm', 'js', 'cc', 'ca'),
                                 lenScaleSpace = 1.5, 
                                 lenScaleTime = 'spatialOnly',
                                 fold = 'all')
bne.conus.2013 <-  readBNEoutput(YYYY = 2013, 
                                 inputSet = c('av', 'gs', 'cm', 'js', 'cc', 'ca'),
                                 lenScaleSpace = 1.5, 
                                 lenScaleTime = 'spatialOnly',
                                 fold = 'all')
bne.conus.2014<-  readBNEoutput(YYYY = 2014, 
                                 inputSet = c('av', 'gs', 'cm', 'js', 'cc', 'ca'),
                                 lenScaleSpace = 1.5, 
                                 lenScaleTime = 'spatialOnly',
                                 fold = 'all')
bne.conus.2015 <-  readBNEoutput(YYYY = 2010, 
                                 inputSet = c('av', 'gs', 'cm', 'js', 'cc', 'ca'),
                                 lenScaleSpace = 1.5, 
                                 lenScaleTime = 'spatialOnly',
                                 fold = 'all')
bne.nys.2010 <-  readBNEoutput(YYYY = 2010, 
                                 inputSet = c('av', 'gs', 'cm', 'js', 'cc', 'ca'),
                                 lenScaleSpace = 1.5, 
                                 lenScaleTime = 'spatialOnly',
                                 fold = 'NYS')
bne.nys.2015 <-  readBNEoutput(YYYY = 2015, 
                               inputSet = c('av', 'gs', 'cm', 'js', 'cc', 'ca'),
                               lenScaleSpace = 1.5, 
                               lenScaleTime = 'spatialOnly',
                               fold = 'NYS')
# 1b make plots 
png(here::here(dir.proj, 'outputs', 'b1_weights_conus_2010_2015.png'), 
    height = 600, width = 1000)
cowplot::plot_grid(
  plotSpatialOneBNEParameter(dta = bne.conus.2010, parameter = 'w_mean_ca', 
                             mainTitle = '2010 CAMS Weights',  
                             valueScale = c(0,0.1, 0.16,  0.2, 0.3, 0.4), legYN = 'legY'), 
  plotSpatialOneBNEParameter(dta = bne.conus.2015, parameter = 'w_mean_ca', 
                             mainTitle = '2015 CAMS Weights',  
                             valueScale = c(0,0.1, 0.16,  0.2, 0.3, 0.4), legYN = 'legY'), 
  makeNiceHistogram(bne.conus.2010, 'w_mean_ca', mainTitle = '2010 CAMS ',
                    xRange = c(0, 1), binCount = 60),
  makeNiceHistogram(bne.conus.2015, 'w_mean_ca', mainTitle = '2015 CAMS ',
                    xRange = c(0, 1), binCount = 60),
  nrow = 2, ncol = 2, rel_heights = 4:2)
dev.off()

png(here::here(dir.proj, 'outputs', 'b2_weights_conus_allYears.png'), 
    height = 600, width = 1000)
cowplot::plot_grid(
  plotSpatialOneBNEParameter(dta = bne.conus.2010, parameter = 'w_mean_ca', 
                             mainTitle = '2010 CAMS Weights',  
                             valueScale = c(0,0.16,  0.2, 0.3,0.4), legYN = 'legY'), 
  plotSpatialOneBNEParameter(dta = bne.conus.2011, parameter = 'w_mean_ca', 
                             mainTitle = '2011 CAMS Weights',  
                             valueScale = c(0, 0.16,  0.2, 0.3, 0.4), legYN = 'legY'),
  plotSpatialOneBNEParameter(dta = bne.conus.2012, parameter = 'w_mean_ca', 
                             mainTitle = '2012 CAMS Weights',  
                             valueScale = c(0, 0.16,  0.2, 0.3, 0.4), legYN = 'legY'),
  plotSpatialOneBNEParameter(dta = bne.conus.2013, parameter = 'w_mean_ca', 
                             mainTitle = '2013 CAMS Weights',  
                             valueScale = c(0, 0.16,  0.2, 0.3, 0.4), legYN = 'legY'),
  plotSpatialOneBNEParameter(dta = bne.conus.2014, parameter = 'w_mean_ca', 
                             mainTitle = '2014 CAMS Weights',  
                             valueScale = c(0, 0.16,  0.2, 0.3, 0.4), legYN = 'legY'),
  plotSpatialOneBNEParameter(dta = bne.conus.2015, parameter = 'w_mean_ca', 
                             mainTitle = '2015 CAMS Weights',  
                             valueScale = c(0, 0.16,  0.2, 0.3, 0.4), legYN = 'legY'), 

  nrow = 2, ncol = 3)
dev.off()


png(here::here(dir.proj, 'outputs', 'b3_weights_nys_2010_2015.png'), 
    height = 600, width = 1000)
cowplot::plot_grid(
  plotSpatialOneBNEParameter(dta = bne.nys.2010, parameter = 'w_mean_ca', 
                             mainTitle = '2010 CAMS Weights',  
                             valueScale = c(0,0.1, 0.16,  0.2, 0.3, 0.4), legYN = 'legY'), 
  plotSpatialOneBNEParameter(dta = bne.nys.2015, parameter = 'w_mean_ca', 
                             mainTitle = '2015 CAMS Weights',  
                             valueScale = c(0,0.1, 0.16,  0.2, 0.3, 0.4), legYN = 'legY'), 
  makeNiceHistogram(bne.nys.2010, 'w_mean_ca', mainTitle = '2010 CAMS ',
                    xRange = c(0, 1), binCount = 60),
  makeNiceHistogram(bne.nys.2015, 'w_mean_ca', mainTitle = '2015 CAMS ',
                    xRange = c(0, 1), binCount = 60),
  nrow = 2, ncol = 2, rel_heights = 4:2)
dev.off()

####************************************
#### 2: Plot Highest-Weighted Model ####
####************************************

bne.w_mean <- bne.conus.2010 %>% 
  dplyr::select(contains('w_mean'))
bne.conus.2010$maxW <-  apply(bne.w_mean, 1, max)

bne.conus.2010 <- bne.conus.2010 %>% 
  mutate(base_maxW = case_when(
    maxW == w_mean_av ~ 'von Donkelaar Model', 
    maxW == w_mean_gs ~ 'BGD', 
    maxW == w_mean_cm ~ 'CMAQ-fusion', 
    maxW == w_mean_js ~ 'Schwartz Model', 
    maxW == w_mean_cc ~ 'CACES', 
    maxW == w_mean_ca ~ 'CAMS'))

png(here::here(dir.proj, 'outputs', 'b4_highestWeight.png'), 
    height = 300, width = 400)
plotSpatialOneBNEParameter(dta = bne.conus.2010, parameter = 'base_maxW', 
                           valueScale = rep(1,5), legYN = 'legY')

dev.off()

####*************************
#### 3: Plot Uncertainty ####
####*************************

bne.conus.2010.noCA <-  readBNEoutput(YYYY = 2010, 
                                 inputSet = c('av', 'gs', 'cm', 'js', 'cc'),
                                 lenScaleSpace = 1.5, 
                                 lenScaleTime = 'spatialOnly',
                                 fold = 'all')
bne.conus.2015.noCA <-  readBNEoutput(YYYY = 2015, 
                                      inputSet = c('av', 'gs', 'cm', 'js', 'cc'),
                                      lenScaleSpace = 1.5, 
                                      lenScaleTime = 'spatialOnly',
                                      fold = 'all')

bne.nys.2010.noCA <-  readBNEoutput(YYYY = 2010, 
                                      inputSet = c('av', 'gs', 'cm', 'js', 'cc'),
                                      lenScaleSpace = 1.5, 
                                      lenScaleTime = 'spatialOnly',
                                      fold = 'NYS')
bne.nys.2015.noCA <-  readBNEoutput(YYYY = 2015, 
                                      inputSet = c('av', 'gs', 'cm', 'js', 'cc'),
                                      lenScaleSpace = 1.5, 
                                      lenScaleTime = 'spatialOnly',
                                      fold = 'NYS')


bne.conus.2010$pred_sd_diff <- bne.conus.2010$pred_sd - bne.conus.2010.noCA$pred_sd

values.SD <- c(bne.conus.2010$pred_sd, bne.conus.2010.noCA$pred_sd)
valueVec.SD <- c(min(values.SD), quantile(values.SD, 0.25), 
                 quantile(values.SD, 0.5), quantile(values.SD, 0.75), max(values.SD))
  
  
png(here::here(dir.proj, 'outputs', 'b5_uncertainty_conus_2010.png'), 
    height = 600, width = 1000)
cowplot::plot_grid(
  plotSpatialOneBNEParameter(dta = bne.conus.2010, parameter = 'pred_sd_diff', 
                             mainTitle = 'Uncertainty Added by CAMS',  legYN = 'legY'), 
  makeNiceHistogram(bne.conus.2010, 'pred_sd_diff', mainTitle = 'Uncertainty Added by CAMS',
                    binCount = 60),
  nrow = 2, ncol = 1, rel_heights = 4:2)
dev.off()

bne.nys.2010$pred_sd_diff <- bne.nys.2010$pred_sd - bne.nys.2010.noCA$pred_sd


png(here::here(dir.proj, 'outputs', 'b6_uncertainty_nys_2010.png'), 
    height = 600, width = 1000)
cowplot::plot_grid(
  plotSpatialOneBNEParameter(dta = bne.nys.2010, parameter = 'pred_sd_diff', 
                             mainTitle = 'Uncertainty Added by CAMS',  legYN = 'legY'), 
  makeNiceHistogram(bne.nys.2010, 'pred_sd_diff', mainTitle = 'Uncertainty Added by CAMS',
                    binCount = 60),
  nrow = 2, ncol = 1, rel_heights = 4:2)
dev.off()

png(here::here(dir.proj, 'outputs', 'b5_uncertainty_nys_2010.png'), 
    height = 600, width = 1000)
cowplot::plot_grid(
  plotSpatialOneBNEParameter(dta = bne.nys.2010, parameter = 'pred_sd', 
                             mainTitle = 'Uncertainty with CAMS',  
                             valueScale = valueVec.SD, legYN = 'legY'), 
  plotSpatialOneBNEParameter(dta = bne.nys.2010.noCA, parameter = 'pred_sd', 
                             mainTitle = 'Uncertainty without CAMS',  
                             valueScale = valueVec.SD, legYN = 'legY'), 
  makeNiceHistogram(bne.nys.2010, 'pred_sd', mainTitle = 'Uncertainty with CAMS',
                    xRange = c(0, max(values.SD)), binCount = 60),
  makeNiceHistogram(bne.nys.2010.noCA, 'pred_sd', mainTitle = 'Uncertainty without CAMS',
                    xRange = c(0, max(values.SD)), binCount = 60),
  nrow = 2, ncol = 2, rel_heights = 4:2)
dev.off()
