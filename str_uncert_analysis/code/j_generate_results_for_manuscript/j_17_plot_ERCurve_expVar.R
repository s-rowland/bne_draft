# File: j_04_table2_propDevExp.R
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

# 0.d. set seed 
# from tutorial
set.seed(1)

#### ------------------------------ ####
####  1: bring in assigned bne ppd  ####
#### ------------------------------ ####

# 1.a. function to read in the data
readAssignedPPD <- function(yyyy) {
  bne.ppd <- fst::read_fst(here::here(dir.proj, 'data', 'ppd_assigned', 'annual', 
                                      paste0('bnePPD_expVar_', yyyy, '.fst'))) %>% 
    mutate(time = yyyy, cell_id = row_number())
}

# 1.b. bring in all the years of assigned ppd
bne.ppd <- map_dfr(2010:2015, readAssignedPPD)

# 1.c. assign popd to be minimal value if it is na, since it is na for very low 
# densities
pop_d.min <- min(bne.ppd$pop_d, na.rm = TRUE)
bne.ppd <- bne.ppd %>% 
  mutate(pop_d = if_else(is.na(pop_d), pop_d.min, pop_d))

bne.ppd <- bne.ppd %>% 
  filter(complete.cases(.))

#### ----------------------------- ####
####  2: set ingredients of model  ####
#### ----------------------------- ####

# 2.a. get plm library
library(plm)

# 2.b. convert index to an character
# plm wants the index to be a character
bne.ppd <- bne.ppd %>% 
  mutate(cell_id = paste0('id_', cell_id))

# 2.c. put the cell id variable first, followed by YYYY
bne.ppd.names <- data.frame(Name = names(bne.ppd)) %>% 
  filter(!Name %in% c('cell_id', 'time'))
bne.ppd <- bne.ppd %>% 
  dplyr::select(cell_id, time, !!bne.ppd.names$Name)

# 2.d. convert to factor
uniqueRegion <- unique(bne.ppd$region)
uniqueCellID <- unique(bne.ppd$cell_id)
bne.ppd <- bne.ppd %>% 
  mutate(region = factor(region, levels = uniqueRegion), 
         cell_id = factor(cell_id, levels = uniqueCellID))

bne.ppd <- bne.ppd %>% 
  mutate(yyyy = time)

#### ----------------------------------------------------------- ####
####  3: create relationship plots for main explanatory factors  ####
#### ----------------------------------------------------------- ####

# 3.a make model
mod.predPM <- plm(y_sd ~ ns(y_mean, 4) + state, data = bne.ppd, model = 'pooling')

# 3.b make the dataset we will predict on 
dta.marg <- expand_grid(state = statesAbb, y_mean = seq(0,25, by = 0.25))

# 3.c make predictions
dta.marg$p1 <- plm:::predict.plm(mod.predPM, dta.marg)

# 3.d average across states
dta.marg <- dta.marg %>% 
  group_by(y_mean) %>% 
  summarize(p1 = mean(p1))

# 3.e generate plot
TP <- ggplot(dta.marg, aes(x=y_mean, y=p1)) +
  geom_point() + 
  geom_line() + 
  theme(axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20)) + 
  labs(x = expression('Predicted Concentration'~'('*mu*g/m^3*')'),
       y = expression('Expected Predictive Uncertainty'~'('*mu*g/m^3*')')) 

# 3.f save plot
png(here::here(dir.proj, 'outputs', 'manuscript', 'fig3_expVar_variable_ySD.png'), 
    height = 800, width = 400)
print(TP)
dev.off()

#### ---------------------------------------------------------- ####
####  4: create relationship plots for all explanatory factors  ####
#### ---------------------------------------------------------- ####

# 4.a. create the objects we need to do our loop
covarDF <- data.frame(covarName = c('y_mean', 'pop_d', 'elev', 'temp_winter', 
                                    'temp_summer', 'wind_speed', 'precip', 'albedo', 
                                    'cloud_cover', 'boundary_h'), 
                      prop_dev = 0) %>% 
  mutate(covarTerm = paste0('ns(', covarName, ', 4)+'))

# 4.b empty dataset to fill 
dta.margTot <- data.frame(expvar_name = NA, exp_var =0 , p1 = 0)

# 4.c loop to make predictions for each factor
for (i in 1:nrow(covarDF)) {
covarName1 <- covarDF$covarName[i]
bne.ppd.renamed <- bne.ppd %>% 
  rename(exp_var = !!covarName1)

# 4.d fit updated model 
mod.predPM <- plm(y_sd ~ ns(exp_var,4) + state, 
                  data = bne.ppd.renamed, model = 'pooling')

# 4.e make predictions
statesAbb <- unique(bne.ppd$state)
# 4.b make the dataset we will predict on 
dta.marg <- expand_grid(state = statesAbb, 
                        exp_var = seq(min(bne.ppd.renamed$exp_var), 
                                      max(bne.ppd.renamed$exp_var), 
                                      length.out =100))

dta.marg$p1 <- plm:::predict.plm(mod.predPM, dta.marg)

# 4.f average across states
dta.marg <- dta.marg %>% 
  group_by(exp_var) %>% 
  summarize(p1 = mean(p1)) %>% 
  mutate(expvar_name = covarName1)

# 4.g add to total data
dta.margTot <- dta.margTot %>% 
  bind_rows(dta.marg) %>% 
  dplyr::filter(!is.na(expvar_name))

}

# 4.h generate plot
TP <- ggplot(dta.margTot) +
  geom_line(aes(x=exp_var, y=p1)) + 
  facet_wrap(vars(expvar_name), scales = 'free_x', nrow =2) +
  theme(axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20), 
        strip.text = element_text(size = 14)) + 
  labs(x = 'Explanatory Variable',
       y = expression('Expected Predictive Uncertainty'~'('*mu*g/m^3*')')) 

# 4.i save plot
png(here::here(dir.proj, 'manuscript', 'efig3_expVar_variable.png'), 
    height = 800, width = 400)
print(TP)
dev.off()

