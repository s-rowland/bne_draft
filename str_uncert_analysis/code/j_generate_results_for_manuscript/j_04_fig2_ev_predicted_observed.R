# File: j_01_table2_dist_main_vars.R
# BNE Annual Uncertainty Analysis
# Authors:
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 10/04/2022
#
# Contents:
#  N. notes
#  0. preparation
#  1. create table
#  2. plot

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

#### ----------------- ####
####  1. create table  ####
#### ----------------- ####

# 1.a. bring in the external vlaidation results 
ppd <- read_csv(here::here(dir.proj, 'bne_ppd', ppdPath, 
                           'ev_2_0-5_2_0-5_0-5_0-0498_0-1353.csv'))
ppd[nrow(ppd)+1,] <- 0
ppd[nrow(ppd)+1,] <- 0
ppd$obs[43] <- 4.5
ppd$obs[44] <- 15.5

#### --------- ####
####  2. plot ####
#### -------- ####

# 2.a make plot 
TP <- ggplot(ppd) + 
  geom_line(aes(x = obs, y = obs), color = 'lightblue') +
  geom_point(aes(x = y_mean, y = obs)) + 
  geom_errorbar(aes(xmin = y_95CIl, xmax = y_95CIu, y = obs)) + 
  labs(x = expression('Estimated Concentration'~'('*mu*g/m^3*')'),
       y = expression('Observed Concentration'~'('*mu*g/m^3*')'))+
  xlim(c(4.5, 16)) +  ylim(c(4.5, 16)) + 
  tema 

# 2.b save plot
png(here::here(dir.proj, 'manuscript', 
               'fig2_EV_coverage_plot.png'), height = 400, width = 400)
print(TP)
dev.off()
