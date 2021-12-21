# Plot BNE outputs_merra
# Examine BNE outputs_merra
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
dir.proj <- 'str_examine_cams'
####*********************
#### 1: Plot Weights ####
####*********************

# 1a get bne outputs_merra
bne.conus.2010 <-  readBNEoutput(YYYY = 2010, 
                          inputSet = c('av', 'gs', 'cm', 'js', 'cc', 'me'),
                          lenScaleSpace = 1.5, 
                          lenScaleTime = 'spatialOnly',
                          fold = 'all')


bne.nys.2010 <-  readBNEoutput(YYYY = 2010, 
                                 inputSet = c('av', 'gs', 'cm', 'js', 'cc', 'me'),
                                 lenScaleSpace = 1.5, 
                                 lenScaleTime = 'spatialOnly',
                                 fold = 'NYS')

# 1b make plots 
png(here::here(dir.proj, 'outputs_merra', 'b1_weights_conus_2010.png'), 
    height = 600, width = 1000)
cowplot::plot_grid(
  plotSpatialOneBNEParameter(dta = bne.conus.2010, parameter = 'w_mean_me', 
                             mainTitle = '2010 MERRA Weights',  
                             valueScale = c(0,0.1, 0.16,  0.2, 0.3, 0.4), legYN = 'legY'), 
  makeNiceHistogram(bne.conus.2010, 'w_mean_me', mainTitle = '2010 MERRA ',
                    xRange = c(0, 1), binCount = 60),
  nrow = 2, ncol = 1, rel_heights = 4:2)
dev.off()


png(here::here(dir.proj, 'outputs_merra', 'b3_weights_nys_2010.png'), 
    height = 600, width = 1000)
cowplot::plot_grid(
  plotSpatialOneBNEParameter(dta = bne.nys.2010, parameter = 'w_mean_me', 
                             mainTitle = '2010 MERRA Weights',  
                             valueScale = c(0,0.1, 0.16,  0.2, 0.3, 0.4), legYN = 'legY'), 
  makeNiceHistogram(bne.nys.2010, 'w_mean_me', mainTitle = '2010 MERRA ',
                    xRange = c(0, 1), binCount = 60),
  nrow = 2, ncol = 1, rel_heights = 4:2)
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
    maxW == w_mean_me ~ 'MERRA'))

png(here::here(dir.proj, 'outputs_merra', 'b4_highestWeight.png'), 
    height = 300, width = 400)
plotSpatialOneBNEParameter(dta = bne.conus.2010, parameter = 'base_maxW', 
                           valueScale = rep(1,5), legYN = 'legY')

dev.off()

####*************************
#### 3: Plot Uncertainty ####
####*************************

bne.conus.2010.noME <-  readBNEoutput(YYYY = 2010, 
                                 inputSet = c('av', 'gs', 'cm', 'js', 'cc'),
                                 lenScaleSpace = 1.5, 
                                 lenScaleTime = 'spatialOnly',
                                 fold = 'all')

bne.nys.2010.noME <-  readBNEoutput(YYYY = 2010, 
                                      inputSet = c('av', 'gs', 'cm', 'js', 'cc'),
                                      lenScaleSpace = 1.5, 
                                      lenScaleTime = 'spatialOnly',
                                      fold = 'NYS')



values.SD <- c(bne.conus.2010$pred_sd, bne.conus.2010.noME$pred_sd)
valueVec.SD <- c(min(values.SD), quantile(values.SD, 0.25), 
                 quantile(values.SD, 0.5), quantile(values.SD, 0.75), max(values.SD))
  
bne.conus.2010$pred_sd_diff <- bne.conus.2010$pred_sd - bne.conus.2010.noME$pred_sd

png(here::here(dir.proj, 'outputs_merra', 'b5_uncertainty_conus_2010.png'), 
    height = 600, width = 600)
cowplot::plot_grid(
  plotSpatialOneBNEParameter(dta = bne.conus.2010, parameter = 'pred_sd_diff', 
                             mainTitle = 'Uncertainty Added by MERRA',  legYN = 'legY', 
                             plotOutline = conus), 
  makeNiceHistogram(bne.conus.2010, 'pred_sd_diff', mainTitle = 'Uncertainty Added by MERRA',
                    binCount = 60),
  nrow = 2, ncol = 1, rel_heights = 4:2)
dev.off()



#### Plot uncertainty decomposition
bne.conus.2010$ens_sd_diff <- bne.conus.2010$ens_sd - bne.conus.2010.noME$ens_sd
bne.conus.2010$bias_sd_diff <- bne.conus.2010$bias_sd - bne.conus.2010.noME$bias_sd
values.SD <- c(bne.conus.2010$ens_sd_diff , bne.conus.2010$bias_sd_diff)

valueVec.SD <- c(round(min(values.SD), 2), round(0.25*max(values.SD), 2), 
                 round(0.5*max(values.SD), 2), round(0.75*max(values.SD), 2),
                 round(max(values.SD), 2))


png(here::here(dir.proj, 'outputs_merra', 'b5_uncertainty_ens_conus_2010.png'), 
    height = 600, width = 600)
cowplot::plot_grid(
  plotSpatialOneBNEParameter(dta = bne.conus.2010, parameter = 'ens_sd_diff', 
                             mainTitle = 'Uncertainty of Model Combination Added by MERRA', 
                             valueScale = valueVec.SD, legYN = 'legY', 
                             plotOutline = conus), 
  makeNiceHistogram(bne.conus.2010, 'ens_sd_diff', mainTitle = 'Uncertainty Added by MERRA',
                    binCount = 60),
  nrow = 2, ncol = 1, rel_heights = 4:2)
dev.off()



png(here::here(dir.proj, 'outputs_merra', 'b5_uncertainty_bias_conus_2010.png'), 
    height = 600, width = 600)
cowplot::plot_grid(
  plotSpatialOneBNEParameter(dta = bne.conus.2010, parameter = 'bias_sd_diff', 
                             mainTitle = 'Uncertainty of Residual Process Added by MERRA',  
                             valueScale = valueVec.SD, legYN = 'legY', 
                             plotOutline = conus), 
  makeNiceHistogram(bne.conus.2010, 'bias_sd_diff', mainTitle = 'Uncertainty Added by MERRA',
                    binCount = 60),
  nrow = 2, ncol = 1, rel_heights = 4:2)
dev.off()




bne.nys.2010$pred_sd_diff <- bne.nys.2010$pred_sd - bne.nys.2010.noME$pred_sd


png(here::here(dir.proj, 'outputs_merra', 'b6_uncertainty_nys_2010.png'), 
    height = 600, width = 600)
cowplot::plot_grid(
  plotSpatialOneBNEParameter(dta = bne.nys.2010, parameter = 'pred_sd_diff', 
                             mainTitle = 'Uncertainty Added by MERRA',  legYN = 'legY', 
                             plotOutline = nys), 
  makeNiceHistogram(bne.nys.2010, 'pred_sd_diff', mainTitle = 'Uncertainty Added by MERRA',
                    binCount = 60),
  nrow = 2, ncol = 1, rel_heights = 4:2)
dev.off()

png(here::here(dir.proj, 'outputs_merra', 'b5_uncertainty_nys_2010.png'), 
    height = 600, width = 1000)
cowplot::plot_grid(
  plotSpatialOneBNEParameter(dta = bne.nys.2010, parameter = 'pred_sd', 
                             mainTitle = 'Uncertainty with MERRA',  
                             valueScale = valueVec.SD, legYN = 'legY'), 
  plotSpatialOneBNEParameter(dta = bne.nys.2010.noME, parameter = 'pred_sd', 
                             mainTitle = 'Uncertainty without MERRA',  
                             valueScale = valueVec.SD, legYN = 'legY'), 
  makeNiceHistogram(bne.nys.2010, 'pred_sd', mainTitle = 'Uncertainty with MERRA',
                    xRange = c(0, max(values.SD)), binCount = 60),
  makeNiceHistogram(bne.nys.2010.noME, 'pred_sd', mainTitle = 'Uncertainty without MERRA',
                    xRange = c(0, max(values.SD)), binCount = 60),
  nrow = 2, ncol = 2, rel_heights = 4:2)
dev.off()
