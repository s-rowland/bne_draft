# Spatial Join of Data 
# Uncertainty Analysis for ISEE
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# N: Notes
# 0: Preparation 
# 1: Readin Uncertainty Dataset 
# 2: Process Explanatory Variable Datasets
# 3: Make Monitor Variables
# 4: Compute Area-Weighted Averaged of Explanatory Variables 
# 5: Join Data
# 6: Save Results

####**************
#### N: Notes ####
####**************

# N1: 

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_set_up_env.R"))
}

if(!exists("Ran_a_00_uncert")){
  here::i_am("README.md")
  source(here::here('str_uncert_analysis', 'code', 
                    "0_00_set_up_env_uncert_analysis.R"))
}

# 0b set the projection string 
projStringRas <- paste0('+init=', projString)

####************************************
#### 1: Process Uncertainty Dataset ####
####************************************



YYYY <- 2010
source(here::here(dir.proj, 'code', 'd_collect_explanatory_factors',
                  'b_07b_harmonize_units_join_script.R'))

YYYY <- 2011
source(here::here(dir.proj, 'code','d_collect_explanatory_factors',
                  'b_07b_harmonize_units_join_script.R'))

YYYY <- 2012
source(here::here(dir.proj, 'code','d_collect_explanatory_factors',
                  'b_07b_harmonize_units_join_script.R'))

# start here 
YYYY <- 2013
source(here::here(dir.proj, 'code','d_collect_explanatory_factors',
                  'b_07b_harmonize_units_join_script.R'))

YYYY <- 2014
source(here::here(dir.proj, 'code','d_collect_explanatory_factors',
                  'b_07b_harmonize_units_join_script.R'))

YYYY <- 2015
source(here::here(dir.proj, 'code','d_collect_explanatory_factors',
                  'b_07b_harmonize_units_join_script.R'))

