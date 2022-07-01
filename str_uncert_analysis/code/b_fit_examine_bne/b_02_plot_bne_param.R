# File: b_02_plot_bne_param.R
# Authors:
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 03/17/22
#
# Contents:
#  N. notes
#  0. preparation
#  1. gather bne outputs
#  2. plot all weights for one year
#  3. plot js weights for five years
#  4. plot weight sd for one year
#  5. plot model combo SD for each year
#  6. plot residual process for each year
#  7. plot predicted concentration for each year 

#### ------------------ ####
####       N. notes     ####
#### ------------------ ####

#### ---------------- ####
####  0. preparation  ####
#### ---------------- ####

# 0.a. load packages, etc
if(!exists('ran_a_00')) {
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

if(!exists('ran_a_00_uncert')) {
  here::i_am('README.md')
  source(here::here('str_uncert_analysis', 'code', 
                    '0_00_config_env_uncert_analysis.R'))
}

#### ----------------------- ####
####  1. gather bne outputs  ####
#### ----------------------- ####

# 1a function to calcuated scaled uncertainty
compute_scaled_y_sd <- function(bne) {
  bne <- bne %>% 

    mutate(y_sd_scaled = y_sd / y_mean) 
}

# 1b bring in bne outputs for each year, using winning hyperparameter combination
bne2010 <- read_csv(here::here(dir.proj, 'bne_ppd', ppdPath, 
                            'refGrid_2010_2_0-5_2_0-5_0-5_0-0498_0-1353.csv')) %>% 
  compute_scaled_y_sd()
bne2011 <- read_csv(here::here(dir.proj, 'bne_ppd', ppdPath, 
                               'refGrid_2011_2_0-5_2_0-5_0-5_0-0498_0-1353.csv'))%>% 
  compute_scaled_y_sd()
bne2012 <- read_csv(here::here(dir.proj, 'bne_ppd', ppdPath, 
                               'refGrid_2012_2_0-5_2_0-5_0-5_0-0498_0-1353.csv'))%>% 
  compute_scaled_y_sd()
bne2013 <- read_csv(here::here(dir.proj, 'bne_ppd', ppdPath, 
                               'refGrid_2013_2_0-5_2_0-5_0-5_0-0498_0-1353.csv'))%>% 
  compute_scaled_y_sd()
bne2014 <- read_csv(here::here(dir.proj, 'bne_ppd', ppdPath, 
                               'refGrid_2014_2_0-5_2_0-5_0-5_0-0498_0-1353.csv'))%>% 
  compute_scaled_y_sd()
bne2015 <- read_csv(here::here(dir.proj, 'bne_ppd', ppdPath, 
                               'refGrid_2015_2_0-5_2_0-5_0-5_0-0498_0-1353.csv')) %>% 
  compute_scaled_y_sd()

# 1c bring in aqs data 
aqs <- read_csv(here::here('inputs', 'pm25', 'training_datasets', 'annual_combined', 
                           'training_cvfolds.csv'))

#### ---------------------------------- ####
####  2. plot all weights for one year  ####
#### ---------------------------------- ####

# 2a set up the scale
varVec <- c(0, bne2012$w_mean_av, bne2012$w_mean_cc, bne2012$w_mean_cm, 
            bne2012$w_mean_gs, bne2012$w_mean_js, bne2012$w_mean_me, 
            bne2012$w_mean_rk)
varRange <- max(varVec) - min(varVec)
varScale <- c(round(min(varVec), 2), round(min(varVec) + 0.25*varRange, 2), 
              round(min(varVec) + 0.5*varRange, 2), round(min(varVec) + 0.75*varRange, 2),
              round(max(varVec), 2))

# 2b plot
png(here::here(dir.proj, outPath, 'c_description_bne_winner_ppd', '1_allweights_2012.png'))
cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne2012, parameterName = 'w_mean_av',
                          mainTitle = 'Weight of AV Model', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2012, parameterName = 'w_mean_cc',
                          mainTitle = 'Weight of CACES', valueScale = varScale),
  plotOneParameterSpatial(dta = bne2012, parameterName = 'w_mean_cm',
                          mainTitle = 'Weight of CMAQ-Fusion', valueScale = varScale),
  plotOneParameterSpatial(dta = bne2012, parameterName = 'w_mean_gs',
                          mainTitle = 'Weight of GBD', valueScale = varScale),
  plotOneParameterSpatial(dta = bne2012, parameterName = 'w_mean_js', 
                          mainTitle = 'Weight of JS Model', valueScale = varScale),
  plotOneParameterSpatial(dta = bne2012, parameterName = 'w_mean_me', 
                          mainTitle = 'Weight of MERRA', valueScale = varScale),
  plotOneParameterSpatial(dta = bne2012, parameterName = 'w_mean_rk', 
                          mainTitle = 'Weight of RK Model', valueScale = varScale),
  ncol = 2, nrow = 4)

dev.off()

#### ----------------------------------- ####
####  3. plot js weights for five years  ####
#### ----------------------------------- ####

# 3.a. generate value scale
varVec <- c(0, bne2010$w_mean_js, bne2011$w_mean_js, bne2012$w_mean_js, 
            bne2013$w_mean_js, bne2014$w_mean_js, bne2015$w_mean_js)
varRange <- max(varVec) - min(varVec)
varScale <- c(round(min(varVec), 2), round(min(varVec) + 0.25*varRange, 2), 
              round(min(varVec) + 0.5*varRange, 2), round(min(varVec) + 0.75*varRange, 2),
              round(max(varVec), 2))

# 3.b. make plot
png(here::here(dir.proj, outPath, 'c_description_bne_winner_ppd', '2_js_weights_all_years.png'))
cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne2010, parameterName = 'w_mean_js', 
                          mainTitle = 'Weight of JS Model 2010', valueScale = varScale),
  plotOneParameterSpatial(dta = bne2011, parameterName = 'w_mean_js', 
                          mainTitle = 'Weight of JS Model 2011', valueScale = varScale),
  plotOneParameterSpatial(dta = bne2012, parameterName = 'w_mean_js', 
                          mainTitle = 'Weight of JS Model 2012', valueScale = varScale),
  plotOneParameterSpatial(dta = bne2013, parameterName = 'w_mean_js', 
                          mainTitle = 'Weight of JS Model 2013', valueScale = varScale),
  plotOneParameterSpatial(dta = bne2014, parameterName = 'w_mean_js', 
                          mainTitle = 'Weight of JS Model 2014', valueScale = varScale),
  plotOneParameterSpatial(dta = bne2014, parameterName = 'w_mean_js', 
                          mainTitle = 'Weight of JS Model 2015', valueScale = varScale),
  ncol = 2, nrow = 4)

dev.off()

#### -------------------------------- ####
####  4. plot weight sd for one year  ####
#### -------------------------------- ####

# 4.a. generate value scale
varVec <- c(0, bne2012$w_sd_av, bne2012$w_sd_cc, bne2012$w_sd_cm, 
            bne2012$w_sd_gs, bne2012$w_sd_js, bne2012$w_sd_me, 
            bne2012$w_sd_rk)
varRange <- max(varVec) - min(varVec)
varScale <- c(round(min(varVec), 2), round(min(varVec) + 0.25*varRange, 2), 
              round(min(varVec) + 0.5*varRange, 2), round(min(varVec) + 0.75*varRange, 2),
              round(max(varVec), 2))

# 4.b. make plot
png(here::here(dir.proj, outPath, 'c_description_bne_winner_ppd', '3_allweightsSD_2012.png'))
cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne2012, parameterName = 'w_sd_av',
                          mainTitle = 'Weight SD of AV Model', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2012, parameterName = 'w_sd_cc',
                          mainTitle = 'Weight SD of CACES', valueScale = varScale),
  plotOneParameterSpatial(dta = bne2012, parameterName = 'w_sd_cm',
                          mainTitle = 'Weight SD of CMAQ-Fusion', valueScale = varScale),
  plotOneParameterSpatial(dta = bne2012, parameterName = 'w_sd_gs',
                          mainTitle = 'Weight SD of GBD', valueScale = varScale),
  plotOneParameterSpatial(dta = bne2012, parameterName = 'w_sd_js', 
                          mainTitle = 'Weight SD of JS Model', valueScale = varScale),
  plotOneParameterSpatial(dta = bne2012, parameterName = 'w_sd_me', 
                          mainTitle = 'Weight SD of MERRA', valueScale = varScale),
  plotOneParameterSpatial(dta = bne2012, parameterName = 'w_sd_rk', 
                          mainTitle = 'Weight SD of RK Model', valueScale = varScale),
  ncol = 2, nrow = 4)

dev.off()

#### -------------------------------------- ####
####  5. plot model combo SD for each year  ####
#### -------------------------------------- ####

# 5.a. generate value scale
varVec <- c(bne2010$ens_sd, bne2011$ens_sd, bne2012$ens_sd, 
            bne2013$ens_sd, bne2014$ens_sd, bne2015$ens_sd)
varRange <- max(varVec) - min(varVec)
varScale <- c(round(min(varVec), 2), round(min(varVec) + 0.25*varRange, 2), 
              round(min(varVec) + 0.5*varRange, 2), round(min(varVec) + 0.75*varRange, 2),
              round(max(varVec), 2))

# 5.b. make plot
png(here::here(dir.proj, outPath, 'c_description_bne_winner_ppd', '4_ens_SD_allyears.png'))
cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne2010, parameterName = 'ens_sd', extraPointObj = filter(aqs, yyyy==2010),
                          mainTitle = 'Model Combo SD 2010', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2011, parameterName = 'ens_sd', extraPointObj = filter(aqs, yyyy==2011),
                          mainTitle = 'Model Combo SD 2011', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2012, parameterName = 'ens_sd', extraPointObj = filter(aqs, yyyy==2012),
                          mainTitle = 'Model Combo SD 2012', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2013, parameterName = 'ens_sd', extraPointObj = filter(aqs, yyyy==2013),
                          mainTitle = 'Model Combo SD 2013', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2014, parameterName = 'ens_sd', extraPointObj = filter(aqs, yyyy==2014),
                          mainTitle = 'Model Combo SD 2014', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2015, parameterName = 'ens_sd', extraPointObj = filter(aqs, yyyy==2015),
                          mainTitle = 'Model ComboSD 2015', valueScale = varScale), 
  ncol = 2, nrow = 3)
dev.off()

#### ---------------------------------------- ####
####  6. plot residual process for each year  ####
#### ---------------------------------------- ####

# 6.a. generate value scale
varVec <- c(bne2010$rp_mean, bne2011$rp_mean, bne2012$rp_mean, 
            bne2013$rp_mean, bne2014$rp_mean, bne2015$rp_mean)
varRange <- max(varVec) - min(varVec)
varScale <- c(round(min(varVec), 2), round(min(varVec) + 0.25*varRange, 2), 
              round(min(varVec) + 0.75*varRange, 2),
              round(max(varVec), 2))

# 6.b. make plot
png(here::here(dir.proj, outPath, 'c_description_bne_winner_ppd', '5_rp_mean_allyears.png'))
cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne2010, parameterName = 'rp_mean',
                          mainTitle = 'Residual Process 2010', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2011, parameterName = 'rp_mean',
                          mainTitle = 'Residual Process 2011', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2012, parameterName = 'rp_mean',
                          mainTitle = 'Residual Process 2012', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2013, parameterName = 'rp_mean',
                          mainTitle = 'Residual Process 2013', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2014, parameterName = 'rp_mean',
                          mainTitle = 'Residual Process 2014', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2015, parameterName = 'rp_mean',
                          mainTitle = 'Residual Process 2015', valueScale = varScale), 
  ncol = 2, nrow = 3)
dev.off()

#### -------------------------------- ####
#### 5. plot bias uncert for one day   ####
#### -------------------------------- ####

# 4.a. generate value scale
varVec <- c(bne2010$rp_sd, bne2011$rp_sd, bne2012$rp_sd, 
            bne2013$rp_sd, bne2014$rp_sd, bne2015$rp_sd)
varRange <- max(varVec) - min(varVec)
varScale <- c(round(min(varVec), 2), round(min(varVec) + 0.25*varRange, 2), 
              round(min(varVec) + 0.5*varRange, 2), round(min(varVec) + 0.75*varRange, 2),
              round(max(varVec), 2))

# 4.b. make plot
png(here::here(dir.proj, outPath, 'c_description_bne_winner_ppd', '6_rp_SD_allyears.png'))
cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne2010, parameterName = 'rp_sd', 
                          mainTitle = 'Residual Process SD 2010', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2011, parameterName = 'rp_sd', 
                          mainTitle = 'Residual Process SD 2011', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2012, parameterName = 'rp_sd', 
                          mainTitle = 'Residual Process SD 2012', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2013, parameterName = 'rp_sd',
                          mainTitle = 'Residual Process SD 2013', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2014, parameterName = 'rp_sd', 
                          mainTitle = 'Residual Process SD 2014', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2015, parameterName = 'rp_sd', #extraPointObj = filter(aqs, yyyy==2015),
                          mainTitle = 'Residual Process SD 2015', valueScale = varScale), 
  ncol = 2, nrow = 3)
dev.off()

#### -------------------------------- ####
#### 6. plot preds for five days   ####
#### -------------------------------- ####

# 4.a. generate value scale
varVec <- c(bne2010$y_mean, bne2011$y_mean, bne2012$y_mean, 
            bne2013$y_mean, bne2014$y_mean, bne2015$y_mean)
varRange <- max(varVec) - min(varVec)
varScale <- c(round(min(varVec), 2), round(min(varVec) + 0.25*varRange, 2), 
              round(min(varVec) + 0.5*varRange, 2), round(min(varVec) + 0.75*varRange, 2),
              round(max(varVec), 2))

# 4.b. make plot
png(here::here(dir.proj, outPath, 'c_description_bne_winner_ppd', '7_pred_MeanY_allyears.png'))
cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne2010, parameterName = 'y_mean',
                          mainTitle = 'Predicted Concentration 2010', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2011, parameterName = 'y_mean',
                          mainTitle = 'Predicted Concentration 2011', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2012, parameterName = 'y_mean',
                          mainTitle = 'Predicted Concentration 2012', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2013, parameterName = 'y_mean',
                          mainTitle = 'Predicted Concentration 2013', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2014, parameterName = 'y_mean',
                          mainTitle = 'Predicted Concentration 2014', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2015, parameterName = 'y_mean',
                          mainTitle = 'Predicted Concentration 2015', valueScale = varScale), 
  ncol = 2, nrow = 3)
dev.off()

#### ------------------------------------------ ####
#### 6. plot residual process SD for each year  ####
#### ------------------------------------------ ####

# 6.a. generate value scale
varVec <- c(bne2010$y_sd, bne2011$y_sd, bne2012$y_sd, 
            bne2013$y_sd, bne2014$y_sd, bne2015$y_sd)
varRange <- max(varVec) - min(varVec)
varScale <- c(round(min(varVec), 2), round(min(varVec) + 0.25*varRange, 2), 
              round(min(varVec) + 0.5*varRange, 2), round(min(varVec) + 0.75*varRange, 2),
              round(max(varVec), 2))

# 6.b. make plot
png(here::here(dir.proj, outPath, 'c_description_bne_winner_ppd', '8_pred_SD_allyears_aqs.png'))
cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne2010, parameterName = 'y_sd', extraPointObj = filter(aqs, yyyy==2010),
                          mainTitle = 'Predictive Uncertainty 2010', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2011, parameterName = 'y_sd', extraPointObj = filter(aqs, yyyy==2011),
                          mainTitle = 'Predictive Uncertainty 2011', valueScale = varScale),
  plotOneParameterSpatial(dta = bne2012, parameterName = 'y_sd', extraPointObj = filter(aqs, yyyy==2012),
                          mainTitle = 'Predictive Uncertainty 2012', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2013, parameterName = 'y_sd', extraPointObj = filter(aqs, yyyy==2013),
                          mainTitle = 'Predictive Uncertainty 2013', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2014, parameterName = 'y_sd', extraPointObj = filter(aqs, yyyy==2014),
                          mainTitle = 'Predictive Uncertainty 2014', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2015, parameterName = 'y_sd', extraPointObj = filter(aqs, yyyy==2015),
                          mainTitle = 'Predictive Uncertainty 2015', valueScale = varScale), 
  ncol = 2, nrow = 3)
dev.off()

# 6.c. make plot without aqs
png(here::here(dir.proj, outPath, 'c_description_bne_winner_ppd', '8_pred_SD_allyears.png'))
cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne2010, parameterName = 'y_sd',
                          mainTitle = 'Predictive Uncertainty 2010', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2011, parameterName = 'y_sd',
                          mainTitle = 'Predictive Uncertainty 2011', valueScale = varScale),
  plotOneParameterSpatial(dta = bne2012, parameterName = 'y_sd', 
                          mainTitle = 'Predictive Uncertainty 2012', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2013, parameterName = 'y_sd', 
                          mainTitle = 'Predictive Uncertainty 2013', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2014, parameterName = 'y_sd',
                          mainTitle = 'Predictive Uncertainty 2014', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2015, parameterName = 'y_sd', 
                          mainTitle = 'Predictive Uncertainty 2015', valueScale = varScale), 
  ncol = 2, nrow = 3)
dev.off()

#### ----------------------------------------------- ####
####  7. plot predicted concentration for each year  ####
#### ----------------------------------------------- ####

# 7.a create scale
varVec <- c(bne2013$contrib_sd_av, bne2013$contrib_sd_cc, bne2013$contrib_sd_cm,
            bne2013$contrib_sd_gs, bne2013$contrib_sd_js, bne2013$contrib_sd_me,
            bne2013$contrib_sd_rk)
varRange <- max(varVec) - min(varVec)
varScale <- c(round(min(varVec), 2), round(min(varVec) + 0.25*varRange, 2), 
              round(min(varVec) + 0.5*varRange, 2), round(min(varVec) + 0.75*varRange, 2),
              round(max(varVec), 2))

# 7.b generate plot
png(here::here(dir.proj, outPath, 'c_description_bne_winner_ppd', '9_pred_SD_contributions_2013.png'))
cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne2013, parameterName = 'y_sd', 
                          mainTitle = 'Predictive Uncertainty 2013'), 
  plotOneParameterSpatial(dta = bne2013, parameterName = 'contrib_sd_av', 
                          mainTitle = 'SD of AV contribution', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2013, parameterName = 'contrib_sd_cc', 
                          mainTitle = 'SD of CC contribution', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2013, parameterName = 'contrib_sd_cm', 
                          mainTitle = 'SD of CM contribution', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2013, parameterName = 'contrib_sd_gs', 
                          mainTitle = 'SD of GS contribution', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2013, parameterName = 'contrib_sd_js', 
                          mainTitle = 'SD of JS contribution', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2013, parameterName = 'contrib_sd_me', 
                          mainTitle = 'SD of ME contribution', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2013, parameterName = 'contrib_sd_rk', 
                          mainTitle = 'SD of RK contribution', valueScale = varScale), 
  
  ncol = 2, nrow = 4)
dev.off()


#### ---------------------------- ####
#### 6. plot preds for six years  ####
#### ---------------------------- ####

# 4.a. generate value scale
varVec <- c(bne2010$y_sd_scaled, bne2011$y_sd_scaled, bne2012$y_sd_scaled, 
            bne2013$y_sd_scaled, bne2014$y_sd_scaled, bne2015$y_sd_scaled)
varRange <- max(varVec) - min(varVec)
varScale <- c(round(min(varVec), 2), round(min(varVec) + 0.25*varRange, 2), 
              round(min(varVec) + 0.5*varRange, 2), round(min(varVec) + 0.75*varRange, 2),
              round(max(varVec), 2))

# 4.b. make plot
png(here::here(dir.proj, outPath, 'c_description_bne_winner_ppd', '10_pred_SD_scaled.png'))
cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne2010, parameterName = 'y_sd_scaled', 
                          mainTitle = 'Scaled Uncertainty 2010', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2011, parameterName = 'y_sd_scaled', 
                          mainTitle = 'Scaled Uncertainty 2011', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2012, parameterName = 'y_sd_scaled', 
                          mainTitle = 'Scaled Uncertainty 2012', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2013, parameterName = 'y_sd_scaled', 
                          mainTitle = 'Scaled Uncertainty 2013', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2014, parameterName = 'y_sd_scaled', 
                          mainTitle = 'Scaled Uncertainty 2014', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2015, parameterName = 'y_sd_scaled', 
                          mainTitle = 'Scaled Uncertainty 2015', valueScale = varScale), 
  ncol = 2, nrow = 3)
dev.off()

png(here::here(dir.proj, outPath, 'c_description_bne_winner_ppd', '10_pred_SD_scaled_aqs.png'))
cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne2010, parameterName = 'y_sd_scaled', extraPointObj = filter(aqs, yyyy==2010),
                          mainTitle = 'Scaled Uncertainty 2010', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2011, parameterName = 'y_sd_scaled', extraPointObj = filter(aqs, yyyy==2011),
                          mainTitle = 'Scaled Uncertainty 2011', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2012, parameterName = 'y_sd_scaled', extraPointObj = filter(aqs, yyyy==2012),
                          mainTitle = 'Scaled Uncertainty 2012', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2013, parameterName = 'y_sd_scaled', extraPointObj = filter(aqs, yyyy==2013),
                          mainTitle = 'Scaled Uncertainty 2013', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2014, parameterName = 'y_sd_scaled', extraPointObj = filter(aqs, yyyy==2014),
                          mainTitle = 'Scaled Uncertainty 2014', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2015, parameterName = 'y_sd_scaled', extraPointObj = filter(aqs, yyyy==2015),
                          mainTitle = 'Scaled Uncertainty 2015', valueScale = varScale), 
  ncol = 2, nrow = 3)
dev.off()
