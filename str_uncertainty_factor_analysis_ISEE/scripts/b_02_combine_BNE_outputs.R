# Combine BNE Outputs
# Uncertainty Analysis for ISEE
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Combine BNE Outputs

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
#### 1: Combine BNE Outputs ####
####****************************

# 1a Combine
dta <- map(c(2010:2015), readBNEoutputISEE) %>% 
  bind_rows()

# 1b Save 
dta %>% 
  write_fst(here::here('uncertainty_factor_analysis_ISEE', 'BNE_outputs',
                       'BNEoutputs_combined.fst'))

# 1c Clean environment
rm(dta)
