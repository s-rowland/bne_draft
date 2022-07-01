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

if(!exists('ran_a_00_uncert')){
  here::i_am('README.md')
  source(here::here('str_uncert_analysis', 'code', 
                    '0_00_config_env_uncert_analysis.R'))
}

#### -------------------------------- ####
#### 1. plot all weights for one day  ####
#### -------------------------------- ####


bneFiles <- data.frame(bne_file=
                         list.files(here::here(dir.proj, 'BNE_ppd', 
                                                      'penalty_exam')))
bneFiles <- bneFiles %>% 
  mutate(param_set = str_sub(bne_file, 16, -5))




paramSets <- unique(bneFiles$param_set)


for ( i in 23:length(paramSets)){
bneSet <- bneFiles %>% 
  filter(param_set == paramSets [i])

bne1 <- read_csv(here::here(dir.proj, 'BNE_ppd', 'penalty_exam', bneSet$bne_file[1]))
bne2 <- read_csv(here::here(dir.proj, 'BNE_ppd', 'penalty_exam', bneSet$bne_file[2]))
bne3 <- read_csv(here::here(dir.proj, 'BNE_ppd', 'penalty_exam', bneSet$bne_file[3]))


pdf(here::here(dir.proj, 'outputs', 'b_description_bne_grid_search', 'penalty_exam', 
               paste0(paramSets[i], '.pdf')))

#png(here::here(dir.proj, 'outputs', 'plots4m', 'allwegihts_jan1_05.png'))
cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne1, parameterName = 'w_mean_av',
                          mainTitle = 'Weight of Aaron Von Donkelaar'), 
  plotOneParameterSpatial(dta = bne1, parameterName = 'w_mean_cm',
                          mainTitle = 'Weight of CMAQ-Fusion'),
  plotOneParameterSpatial(dta = bne1, parameterName = 'w_mean_js', 
                          mainTitle = 'Weight of Joel Schwartz 2019'),
  plotOneParameterSpatial(dta = bne1, parameterName = 'w_mean_me', 
                          mainTitle = 'Weight of MERRA'),
  plotOneParameterSpatial(dta = bne1, parameterName = 'w_mean_rk', 
                          mainTitle = 'Weight of Rajesh Model'),
  ncol = 2, nrow = 3)

#dev.off()

  

#### -------------------------------- ####
#### 2. lot js weights for five days  ####
#### -------------------------------- ####

#png(here::here(dir.proj, 'outputs', 'plots4m', 'b_w_js_fivedays.png'))
cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne1, parameterName = 'w_mean_js', 
                          mainTitle = 'Weight of JS Jan 1, 2005'),
  plotOneParameterSpatial(dta = bne2, parameterName = 'w_mean_js', 
                          mainTitle = 'Weight of JS May 31, 2005'),
  plotOneParameterSpatial(dta = bne3, parameterName = 'w_mean_js', 
                         mainTitle = 'Weight of JS Sept 8, 2005'),
  ncol = 2, nrow = 3)

# dev.off()

#### -------------------------------- ####
#### 3. plot w_js_uncert for one day   ####
#### -------------------------------- ####

#png(here::here(dir.proj, 'outputs', 'plots4m', 'c_allwegihtsSD_jan1_05.png'))
cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne1, parameterName = 'w_sd_av',
                          mainTitle = 'SD of Weight of Aaron Von Donkelaar'), 
                          #valueScale = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06)), 
  plotOneParameterSpatial(dta = bne1, parameterName = 'w_sd_cm',
                          mainTitle = 'SD of Weight of CMAQ-Fusion'), 
                          #valueScale = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06)),
  plotOneParameterSpatial(dta = bne1, parameterName = 'w_sd_js', 
                          mainTitle = 'SD of Weight of Joel Schwartz 2019'), 
                         # valueScale = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06)),
  plotOneParameterSpatial(dta = bne1, parameterName = 'w_sd_me', 
                          mainTitle = 'SD of Weight of MERRA'), 
                          #valueScale = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06)),
  plotOneParameterSpatial(dta = bne1, parameterName = 'w_sd_rk', 
                          mainTitle = 'SD of Weight of Rajesh Model'), 
                         # valueScale = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06)),
  ncol = 2, nrow = 3)

#dev.off()

#### -------------------------------- ####
#### 4. plot bias for five days   ####
#### -------------------------------- ####

#png(here::here(dir.proj, 'outputs', 'plots4m', 'd_bias_fivedays.png'))
cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne1, parameterName = 'bias_mean', 
                          mainTitle = 'Residual Process Jan 1, 2005'),
  plotOneParameterSpatial(dta = bne2, parameterName = 'bias_mean', 
                          mainTitle = 'Residual Process May 31, 2005'),
  #plotOneParameterSpatial(dta = bne3, parameterName = 'bias_mean', 
   #                       mainTitle = 'Residual Process Sept 8, 2005'),
  ncol = 2, nrow = 3)

#dev.off()


#### -------------------------------- ####
#### 5. plot bias uncert for one day   ####
#### -------------------------------- ####
#yvec <- c( 0.03, 0.04, 0.05, 0.06)
#png(here::here(dir.proj, 'outputs', 'plots4m', 'e_biasSd_fivedays.png'))
cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne1, parameterName = 'bias_sd', #valueScale =yvec,
                          mainTitle = 'SD of Residual Process Jan 1, 2005'),
  plotOneParameterSpatial(dta = bne2, parameterName = 'bias_sd', #valueScale =yvec,
                          mainTitle = 'SD of Residual Process May 31, 2005'),
  plotOneParameterSpatial(dta = bne3, parameterName = 'bias_sd', #valueScale =yvec,
                          mainTitle = 'SD of Residual Process Sept 8, 2005'),
  ncol = 2, nrow = 3)

#dev.off()

#### -------------------------------- ####
#### 6. plot preds for five days   ####
#### -------------------------------- ####
ymax <- max(c(bne1$y_mean, bne2$y_mean, bne3$y_mean))
yvec <- c(0, round(ymax/4,2), round(ymax/2,2), round(3*ymax/4,2), round(ymax,2))

#png(here::here(dir.proj, 'outputs', 'plots4m', 'f_predY_fivedays.png'))
cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne1, parameterName = 'y_mean',# valueScale =yvec,
                          mainTitle = 'Estimated Concentration Jan 1, 2005'),
  plotOneParameterSpatial(dta = bne2, parameterName = 'y_mean', #valueScale =yvec,
                          mainTitle = 'Estimated Concentration May 31, 2005'),
  plotOneParameterSpatial(dta = bne3, parameterName = 'y_mean', #valueScale =yvec,
                          mainTitle = 'Estimated Concentration Sept 8, 2005'),
  ncol = 2, nrow = 3)

#dev.off()

#### -------------------------------- ####
#### 7. plot pred uncert for five days   ####
#### -------------------------------- ####

ymax <- max(c(bne1$y_sd, bne2$y_sd, bne3$y_sd))
yvec <- c(0, round(ymax/4,2), round(ymax/2,2), round(3*ymax/4,2), round(ymax,2))

#png(here::here(dir.proj, 'outputs', 'plots4m', 'f_ySD_fivedays.png'))
cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne1, parameterName = 'y_sd', #valueScale =yvec,
                          mainTitle = 'Predictive Uncertainty Jan 1, 2005'),
  plotOneParameterSpatial(dta = bne2, parameterName = 'y_sd', #valueScale =yvec,
                          mainTitle = 'Predictive Uncertainty May 31, 2005'),
  plotOneParameterSpatial(dta = bne3, parameterName = 'y_sd', #valueScale =yvec,
                          mainTitle = 'Predictive Uncertainty Sept 8, 2005'),
  ncol = 2, nrow = 3)

dev.off()

}
