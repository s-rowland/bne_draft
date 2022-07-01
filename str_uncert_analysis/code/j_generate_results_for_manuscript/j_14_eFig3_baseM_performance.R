# File: j_27_eFig3_baseM_performance.R
# BNE Annual Uncertainty Analysis
# Authors:
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 10/04/2022
#
# Contents:
#  N. notes
#  0. preparation
#  1. compute rmse
#  2. make a nice plot

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
####  1. compute rmse  ####
#### ----------------- ####

# 1.a bring in training data 
train <- read_csv(here::here('inputs', 'pm25', 'training_datasets','annual_combined', 
                           'training_cvfolds_meBlend.csv'))

# 1.b calculate se for each point and prediction model 
train <- train %>% 
  mutate(se_av = (pred_av - obs)^2, 
         se_cc = (pred_cc - obs)^2, 
         se_cm = (pred_cm - obs)^2, 
         se_gs = (pred_gs - obs)^2, 
         se_js = (pred_js - obs)^2, 
         se_me = (pred_me - obs)^2, 
         se_rk = (pred_rk - obs)^2)

# 1.c assign region and region name 
train <- train %>% 
  inner_join(read_csv(here::here('ancillary_data', 'generated', 'key_state_region.csv')), by = 'state') %>% 
  inner_join(read_csv(here::here('ancillary_data', 'generated', 'key_region_regionName.csv')), by = 'region')

# 1.d compute overall regional averages 
train.ave <- train %>% 
  group_by(region_name) %>% 
  summarize(rmse_av = sqrt(mean(se_av)), 
            rmse_cc = sqrt(mean(se_cc)),
            rmse_cm = sqrt(mean(se_cm)),
            rmse_gs = sqrt(mean(se_gs)),
            rmse_js = sqrt(mean(se_js)),
            rmse_me = sqrt(mean(se_me)),
            rmse_rk = sqrt(mean(se_rk))) %>% 
  pivot_longer(cols = contains('rmse')) %>% 
  mutate(base_model = str_sub(name, 6, 10))

# 1.e compute year-regional averages
train.ave.yyyy <- train %>% 
  group_by(region_name, yyyy) %>% 
  summarize(rmse_av = sqrt(mean(se_av)), 
            rmse_cc = sqrt(mean(se_cc)),
            rmse_cm = sqrt(mean(se_cm)),
            rmse_gs = sqrt(mean(se_gs)),
            rmse_js = sqrt(mean(se_js)),
            rmse_me = sqrt(mean(se_me)),
            rmse_rk = sqrt(mean(se_rk)))

train.ave.yyyy.box <- train.ave.yyyy %>% 
  pivot_longer(cols = contains('rmse')) %>% 
  rename(rmse = value) %>%
  mutate(model = str_sub(name, 6, 10)) %>% 
  renameBaseM()


#%>% 
 # mutate(value = round(value, 2)) %>%
  #pivot_wider(names_from = c(region, yyyy), 
   #           values_from = value)

# 1.f. save table


#### --------------------- ####
####  2. make a nice plot  ####
#### --------------------- ####
train.ave.yyyy.box <- train.ave.yyyy.box  %>% 
  orderRegionNames()
# 2.a create plot
png(here::here(dir.proj, 'manuscript', 'eFig3_baseM_performance.png'), 
    height = 1000, width = 1000)
ggplot(train.ave.yyyy.box) +
  geom_line(aes(x = yyyy, y = rmse, color = model)) + 
  geom_point(aes(x = yyyy, y = rmse, color = model, shape = model)) + 
  facet_wrap(vars(region_name)) + 
  scale_color_brewer(palette = 'Set2') + 
  scale_fill_brewer(palette = "Set2") +
  scale_y_log10() + 
  scale_x_continuous(breaks = c(2010, 2013, 2015) )+
  labs(x = 'Year', y = 'RMSE', color = 'Base Model', shape = 'Base Model') + 
  tema + theme_bw() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 15), 
        strip.text = element_text(size = 15), 
        strip.background = element_rect(color = 'white')) + 
  theme(legend.position = c(0.60, 0.15), 
        legend.title = element_text(size = 22), 
        legend.text = element_text(size = 18)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
dev.off()


