# Join Daily AQS and MERRA
# Uncertainty Analysis for ISEE
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation
# 1: Wrangle BNE Outputs
# 2: Plot BNE Weights 


####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_set_up_env.R"))
}

if(!exists("Ran_a_00_ISEE")){
  here::i_am("README.md")
  source(here::here('uncertainty_factor_analysis_ISEE', 'scripts', 
                    "a_00_set_up_env_ISEE.R"))
}

####****************************
#### 1: Wrangle BNE Outputs ####
####****************************


# mayabe we do like map 2? 

a <- map2(c(2010:2015), rep(3.5, 6), averageMetricsAcrossFolds)

b <- bind_rows(a) %>% 
  summarize_all(mean)
b %>% write_csv(here::here('uncertainty_factor_analysis_ISEE', 'outputs','presentation', 
                           'performance_metrics.csv'))


