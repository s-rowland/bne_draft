# File: j_28_eFig4_mean_weights_rp.R
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
readAssignedPPD <- function(yyyy) {
  bne.ppd <- fst::read_fst(here::here(dir.proj, 'data', 'ppd_assigned', ppdPath, 
                                      paste0('bnePPD_expVar_', yyyy, '.fst'))) %>% 
    mutate(time = yyyy, cell_id = row_number()) 
}

# 1.b. bring in all the years of assigned ppd
bne.ppd <- map_dfr(2010:2015, readAssignedPPD)

# 1.c get study-period averages 
bne.ppd.agg <- bne.ppd %>% 
  group_by(lat, lon) %>% 
  summarize_all(.funs= mean)

#### -------------- ####
####  2. make plot ####
#### ------------- ####

# 2.a. generate value scale
varVec <- c(bne.ppd.agg$w_mean_av, bne.ppd.agg$w_mean_cc, bne.ppd.agg$w_mean_cm, 
            bne.ppd.agg$w_mean_gs, bne.ppd.agg$w_mean_js, bne.ppd.agg$w_mean_me, 
            bne.ppd.agg$w_mean_rk)
varRange <- max(varVec) - min(varVec)
varScale <- c(round(min(varVec), 2), round(min(varVec) + 0.25*varRange, 2), 
              round(min(varVec) + 0.5*varRange, 2), round(min(varVec) + 0.75*varRange, 2),
              round(max(varVec), 2))

# 2.b. make plot
png(here::here(dir.proj, 'manuscript', 
               'efig4_mean_weights_rp.png')) #, height = 1800, width = 900)

cowplot::plot_grid(
  plotOneParameterSpatial(dta = bne.ppd.agg, parameterName = 'w_mean_av',
                          mainTitle = 'Weight of AV2021', valueScale = varScale), 
  plotOneParameterSpatial(dta = bne.ppd.agg, parameterName = 'w_mean_cc',
                          mainTitle = 'Weight of SK2020', valueScale = varScale),
  plotOneParameterSpatial(dta = bne.ppd.agg, parameterName = 'w_mean_cm',
                          mainTitle = 'Weight of VB2012', valueScale = varScale),
  plotOneParameterSpatial(dta = bne.ppd.agg, parameterName = 'w_mean_gs',
                          mainTitle = 'Weight of GS2018', valueScale = varScale),
  plotOneParameterSpatial(dta = bne.ppd.agg, parameterName = 'w_mean_js', 
                          mainTitle = 'Weight of QD2019', valueScale = varScale),
  plotOneParameterSpatial(dta = bne.ppd.agg, parameterName = 'w_mean_me', 
                          mainTitle = 'Weight of GM2022', valueScale = varScale),
  plotOneParameterSpatial(dta = bne.ppd.agg, parameterName = 'w_mean_rk', 
                          mainTitle = 'Weight of RK2022', valueScale = varScale),
 plotOneParameterSpatial(dta = bne.ppd.agg, parameterName = 'rp_mean',
                          mainTitle = 'Residual Process', legTitle = 'Value'), 
                   ncol = 2)
dev.off()
