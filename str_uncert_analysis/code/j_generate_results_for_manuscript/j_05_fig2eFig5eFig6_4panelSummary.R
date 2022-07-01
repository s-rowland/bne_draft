# File: j_02_fig2_4panelSummary.R
# BNE Annual Uncertainty Analysis
# Authors:
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 10/04/2022
#
# Contents:
#  N. notes
#  0. preparation

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

#### -------------------------------- ####
#### 1. bring in bne ppd's  ####
#### -------------------------------- ####

# 1.a. function to read in the data
readAssignedPPD <- function(yyyy) {
  bne.ppd <- fst::read_fst(here::here(dir.proj, 'data', 'ppd_assigned', ppdPath, 
                                      paste0('bnePPD_expVar_', yyyy, '.fst'))) %>% 
    mutate(time = yyyy, cell_id = row_number(), 
           y_sd_scaled = y_sd / y_mean) %>% 
    dplyr::select(lat, lon, time, ens_mean, ens_sd, rp_mean, rp_sd, y_mean, y_sd, 
                  y_sd_scaled, region)
}

# 1.b. bring in all the years of assigned ppd
bne.ppd <- map_dfr(2010:2015, readAssignedPPD)

# 1.c. calculate study-period average
bne.ann <- bne.ppd %>% 
  group_by(lat, lon) %>% 
  summarize(ens_mean = mean(ens_mean), 
            ens_sd = mean(ens_sd),
            rp_mean = mean(rp_mean), 
            rp_sd = mean(rp_sd),
    y_mean = mean(y_mean), 
            y_sd = mean(y_sd), 
    y_sd_scaled = mean(y_sd_scaled)) 

bne.region <- bne.ppd %>% 
  group_by(time, region) %>% 
  summarize(ens_mean = mean(ens_mean), 
             ens_sd = mean(ens_sd),
             rp_mean = mean(rp_mean), 
             rp_sd = mean(rp_sd),
            y_mean = mean(y_mean), 
            y_sd = mean(y_sd), 
            y_sd_scaled = mean(y_sd_scaled)) %>% 
  inner_join(key.regionNames, by = 'region') %>%  
  orderRegionNames()

#### ----------------------------- ####
#### 2: generate plots  ####
#### ----------------------------- ####

# 2.a. study-period mean predicted concentration
TPa <- plotOneParameterSpatial(dta = bne.ann, parameterName = 'y_mean',
                          mainTitle = 'Average Predicted Concentration', titleSize = 15) 

# 2.b. study-period mean predictive uncertainty
TPb <- plotOneParameterSpatial(dta = bne.ann, parameterName = 'y_sd',
                               mainTitle = 'Average Predictive Uncertainty', titleSize =15) 

# 2.c. study-period mean predictive uncertainty
TPc <- plotOneParameterSpatial(dta = bne.ann, parameterName = 'y_sd_scaled',
                               mainTitle = 'Average Scaled Uncertainty', titleSize = 15) 

# 2.d. Regional trends of Predicted Pm2.5 
TPd <- ggplot(bne.region) + 
  geom_line(aes(x = time, y = y_mean, color = region_name)) + 
  scale_color_manual(values = col.Region) +
  labs(x = 'Year', y = expression('Predicted Concentration ('*mu*g/m^3*')'), 
       color = 'EPA Region') + 
  ggtitle('Regional Average Predicted Concentration') + 
  tema

# 2.e Regional trends of uncertainty
TPe <- ggplot(bne.region) + 
  geom_line(aes(x = time, y = y_sd, color = region_name)) + 
  scale_color_manual(values = col.Region, ) +
  labs(x = 'Year', y = expression('Predictive Uncertainty ('*mu*g/m^3*')'), 
       color = 'EPA Region') + 
  ggtitle('Regional Average Predictive Uncertainty') +
  tema
TPf <- ggplot(bne.region) + 
  geom_line(aes(x = time, y = y_sd_scaled, color = region_name)) + 
  scale_color_manual(values = col.Region, ) +
  labs(x = 'Year', y = expression('Scaled Uncertainty'), 
       color = 'EPA Region') + 
  ggtitle('Regional Average Scaled Uncertainty') +
  tema


# 2.f Put them together 
png(here::here(dir.proj,'manuscript', 
               'fig3_6panel_summary.png'), height = 900, width = 1300)
cowplot::plot_grid(TPa, TPd,TPb,  TPe, TPc,TPf, nrow = 3, labels = 'AUTO')
dev.off()

#### ---------------------------------------------- ####
#### 3: plot summary of weighted model combination  ####
#### ---------------------------------------------- ####

# 3.a study-period mean predicted concentration
TPa <- plotOneParameterSpatial(dta = bne.ann, parameterName = 'ens_mean',
                               mainTitle = 'Average Model Combination', titleSize = 15) 

# 3.b study-period mean predictive uncertainty
TPb <- plotOneParameterSpatial(dta = bne.ann, parameterName = 'ens_sd',
                               mainTitle = 'Average Model Combination Uncertainty', titleSize = 15) 

# 3.c Regional trends of Predicted Pm2.5 
TPc <- ggplot(bne.region) + 
  geom_line(aes(x = time, y = ens_mean, color = region_name)) + 
  scale_color_manual(values = col.Region) +
  labs(x = 'Year', y = expression('Model Combination ('*mu*g/m^3*')'), 
       color = 'EPA Region') + 
  ggtitle('Regional Average Model Combination') + 
  tema

# 3.d Regional trends of uncertainty
TPd <- ggplot(bne.region) + 
  geom_line(aes(x = time, y = ens_sd, color = region_name)) + 
  scale_color_manual(values = col.Region, ) +
  labs(x = 'Year', y = expression('Weighted Model Combination Uncertainty ('*mu*g/m^3*')'), 
       color = 'EPA Region') + 
  ggtitle('Regional Average Weighted Model Combination Uncertainty') +
  
  tema

# 3.e. Put them together 
png(here::here(dir.proj,'manuscript', 
               'efig5_4panel_summary_ens.png'), height = 900, width = 900)
cowplot::plot_grid(TPa, TPc, TPb, TPd, nrow = 2, labels = 'AUTO')
dev.off()

#### ------------------------------------- ####
####  4: plot summary of residual process  ####
#### ------------------------------------- ####

# 4.a. study-period mean predicted concentration
TPa <- plotOneParameterSpatial(dta = bne.ann, parameterName = 'rp_mean',
                               mainTitle = 'Average Residual Process Term', titleSize = 15) 

# 4.b. study-period mean predictive uncertainty
TPb <- plotOneParameterSpatial(dta = bne.ann, parameterName = 'rp_sd',
                               mainTitle = 'Average of SD of Residual Process', titleSize = 15) 

# 4.c Regional trends of Predicted Pm2.5 
TPc <- ggplot(bne.region) + 
  geom_line(aes(x = time, y = rp_mean, color = region_name)) + 
  scale_color_manual(values = col.Region) +
  labs(x = 'Year', y = expression('Residual Process ('*mu*g/m^3*')'), 
       color = 'EPA Region') + 
  ggtitle('Regional Average Weighted Model Combination') + 
  tema

# 4.d Regional trends of uncertainty
TPd <- ggplot(bne.region) + 
  geom_line(aes(x = time, y = rp_sd, color = region_name)) + 
  scale_color_manual(values = col.Region, ) +
  labs(x = 'Year', y = expression('Residual Process Uncertainty ('*mu*g/m^3*')'), 
       color = 'EPA Region') + 
  ggtitle('Regional Average Residual Process Uncertainty') +
  tema

# 4.e. Put them together 
png(here::here(dir.proj,'manuscript', 
               'efig6_4panel_summary_rp.png'), height = 900, width = 900)
cowplot::plot_grid(TPa, TPc, TPb, TPd, nrow = 2, labels = 'AUTO')
dev.off()
