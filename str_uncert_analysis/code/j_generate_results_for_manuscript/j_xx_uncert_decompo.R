# File: j_07_eTable1.R
# BNE Annual Uncertainty Analysis
# Authors:
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 10/04/2022
#
# Contents:
#  N. notes
#  0. preparation
#  1. bring in data
#  2. create table

#### ------------------ ####
####       N. notes     ####
#### ------------------ ####


#### ---------------- ####
####  0. preparation  ####
#### ---------------- ####

# 0.a. load packages, etc
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

# 0.b. load objects and packages specific for this work
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

#### ------------------ ####
####  1. bring in data  ####
#### ------------------ ####

# 1.a. function to read in the data
add_model_contributions <- function(bne) {
  bne <- bne %>% 
    
    mutate(y_sd_scaled = 100* y_sd / y_mean) 
}




bne2010 <- read_csv(here::here(dir.proj, 'bne_ppd', ppdPath, 
                               'refGrid_2010_2_0-5_2_0-5_0-5_0-0498_0-1353.csv')) %>% 
  add_model_contributions()
bne2011 <- read_csv(here::here(dir.proj, 'bne_ppd', ppdPath, 
                               'refGrid_2011_2_0-5_2_0-5_0-5_0-0498_0-1353.csv'))%>% 
  add_model_contributions()
bne2012 <- read_csv(here::here(dir.proj, 'bne_ppd', ppdPath, 
                               'refGrid_2012_2_0-5_2_0-5_0-5_0-0498_0-1353.csv'))%>% 
  add_model_contributions()
bne2013 <- read_csv(here::here(dir.proj, 'bne_ppd', ppdPath, 
                               'refGrid_2013_2_0-5_2_0-5_0-5_0-0498_0-1353.csv'))%>% 
  add_model_contributions()
bne2014 <- read_csv(here::here(dir.proj, 'bne_ppd', ppdPath, 
                               'refGrid_2014_2_0-5_2_0-5_0-5_0-0498_0-1353.csv'))%>% 
  add_model_contributions()
bne2015 <- read_csv(here::here(dir.proj, 'bne_ppd', ppdPath, 
                               'refGrid_2015_2_0-5_2_0-5_0-5_0-0498_0-1353.csv')) %>% 
  add_model_contributions()

# bring in aqs data 
aqs <- read_csv(here::here('inputs', 'pm25', 'training_datasets', 'annual_combined', 
                           'training_cvfolds.csv'))


#### -------------------------------- ####
#### 6. plot preds for six years   ####
#### -------------------------------- ####

# 4.a. generate value scale
varVec <- c(bne2012$y_sd, bne2012$ens_sd, bne2012$rp_sd)
varRange <- max(varVec) - min(varVec)
varScale <- c(round(min(varVec), 2), round(min(varVec) + 0.25*varRange, 2), 
              round(min(varVec) + 0.5*varRange, 2), round(min(varVec) + 0.75*varRange, 2),
              round(max(varVec), 2))

# 4.b. make plot
png(here::here(dir.proj, outPath, 'manuscript', 'eFigX_uncert_decompo.png'), 
    height = 400, width = 1500)
cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne2012, parameterName = 'y_sd', 
                          mainTitle = 'Predictive Uncertainty 2012', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2012, parameterName = 'ens_sd', 
                          mainTitle = 'Weighted Model Disagreement 2012', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne2012, parameterName = 'rp_sd', 
                          mainTitle = 'SD of Residual Process 2012', valueScale = varScale), 
  ncol = 3, nrow = 1)
dev.off()


