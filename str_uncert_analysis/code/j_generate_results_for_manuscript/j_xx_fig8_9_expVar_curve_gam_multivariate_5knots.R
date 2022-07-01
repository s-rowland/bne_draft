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
    mutate(time = yyyy, cell_id = row_number(), 
           y_sd_scaled = y_sd / y_mean)
}

# 1.b. bring in all the years of assigned ppd
bne.ppd <- map_dfr(2010:2015, readAssignedPPD)

# 1.c. assign popd to be minimal value if it is na, since it is na for very low 
# densities
pop_d.min <- min(bne.ppd$pop_d, na.rm = TRUE)
bne.ppd <- bne.ppd %>% 
  mutate(pop_d = if_else(is.na(pop_d), pop_d.min, pop_d)) %>% 
  mutate(pop_d = sqrt(pop_d))

bne.ppd <- bne.ppd %>% 
  filter(complete.cases(.))



#### ----------------------------------------------------------- ####
####  3: create relationship plots for main explanatory factors  ####
#### ----------------------------------------------------------- ####
covarDF <- data.frame(covarName = c('y_mean', 'pop_d', 'mon_dist', 'elev', 'temp_winter', 
                                    'temp_summer', 'wind_speed', 'precip', 'albedo', 
                                    'cloud_cover', 'boundary_h'), 
                      prop_dev = 999) %>% 
  mutate(covarTerm = paste0('s(', covarName, ')+'), 
         var_order = row_number())


#### ---------------------------------------------------------- ####
####  4: create relationship plots for all explanatory factors  ####
#### ---------------------------------------------------------- ####

# 4.a define function to generate the plots
make_gam_responses <- function(expVarName) {
  
  # 4.a.i rename the covariate's column in the dataframe
  bne.ppd.renamed <- bne.ppd %>% 
    rename(expVar := all_of(expVarName))
  
  # 4.a.ii make onebasis object 
  ob.expVar <- onebasis(bne.ppd.renamed$expVar, fun = 'cr')
  
  # make the formula statement with a single onebasis term
  covarDF.sm <- covarDF %>% 
    filter(covarName != expVarName)
  
  covariateStatement.full <- paste0('y_sd_scaled ~ ', 'ob.expVar + ',
                                       paste0(covarDF.sm$covarTerm, collapse = ''), 
                                       'te(lat, lon,  k = c(10, 5), bs =', ' \'cr\'', ')+ region*time')
  
  # 4.a.iii fit the models
  mod.partial <- gam(as.formula(covariateStatement.full), 
                     data = bne.ppd)
  
  expVar.vec <- 
  # 4.a.iv generate crosspred object (predictions)
  cp.expVar <- crosspred(ob.expVar, mod.partial, 
                         cen = mean(bne.ppd.renamed$expVar), 
                         at = bne.ppd.renamed$expVar)
  # 4.a.v put cross pred in tidy format
  responses <- data.frame(expVar = cp.expVar$predvar, 
                          fit = cp.expVar$allfit, 
                          se = cp.expVar$allse) %>% 
    mutate(lci = fit - 1.96 *se, 
           uci = fit + 1.96 *se) %>% 
    mutate(expVarName = !!expVarName)
  
  
  return(responses)
}

# 4.b generate plots for all of the potentail explanatory variables
responses.combined <- map_dfr(covarDF$covarName, make_gam_responses)


# 4.c clean up names of explanatory variables 
responses.combined <- responses.combined %>% 
  renameExpVar()

# 4.c make facetted plot 
TP <- ggplot(responses.combined, aes(x=expVar)) +
  geom_line(aes(y = fit)) +
  geom_hline(yintercept= 0, linetype = 'dashed') +
  geom_ribbon(aes(ymin = lci, ymax = uci), color = 'grey', fill = 'grey', alpha = 0.25) +
  geom_rug() + 
  facet_wrap(vars(expVarName), ncol = 3, scales='free_x') +
  labs(x = 'Potential Explanatory Variable',
       y = expression('Predictive Uncertainty'~'('*mu*g/m^3*')'~'Relative to Mean')) + 

  tema + 
  theme(axis.text = element_text(size = 15), 
        axis.title = element_text(size = 23), 
        strip.text = element_text(size = 15))

png(here::here(dir.proj, outPath, 'manuscript', 
               paste0('eFig10_expVar_ySD_scaled_gam_5knots_notime.png')), 
    height = 1000, width = 1000)
print(TP)
dev.off()

