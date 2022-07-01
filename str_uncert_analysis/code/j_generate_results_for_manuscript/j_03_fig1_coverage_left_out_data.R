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

# 1.a make table 
coverage <- data.frame(level = c(95, 90, 85, 80, 75, 70), 
                       percent_covered = c(91.67, 87.49, 82.76, 78.09, 73.08, 69.44), 
                       type = 'Observed')
coverage <- data.frame(level = c(95, 90, 85, 80, 75, 70), 
                       percent_covered = c(95, 90, 85, 80, 75, 70), 
                       type = 'Expected') %>% 
  bind_rows(coverage)


#### --------- ####
####  2. plot ####
#### -------- ####

# 2.a make plot 
TP <- ggplot(coverage) + 
  geom_point(aes(x = level, y = percent_covered, color = type)) + 
  geom_line(aes(x = level, y = percent_covered, color = type)) + 
  labs(x = 'Credible Interval Percentage', y = 'Coverage of Left-Out Data', color = "Type")+
  tema + 
  theme(legend.position = c(0.75, 0.25))

# 2.b save plot
png(here::here(dir.proj, 'manuscript', 
               'fig1_coverage_plot.png'), height = 400, width = 400)
print(TP)
dev.off()





