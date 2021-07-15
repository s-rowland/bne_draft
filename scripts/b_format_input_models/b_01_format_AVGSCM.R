# Format AVGSCM
# Prepare BNE Inputs
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Readin Training Data

####**************
#### N: Notes ####
####**************

# Right now we are using training and avgscm prediction datasets inherited 
# from the Capstone students' project, so the code it not yet 100% reproducible

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_setUp_env.R"))
}

####********************************
#### 1: Separate Inputs by Year ####
####********************************

# 1a Read dataset 
# note that this is not actually the raw data - these models have already been combined
avgscm  <- read_csv(here::here('data_input_models', 'raw', 'AVGSCM_annual_raw',
                               'avgscm_annual_2010_2016_raw.csv')) 

# 1b Rename columns 
avgscm <- avgscm %>% 
  rename(AV = pred_AV, GS = pred_GS, SC = pred_SC, CM = pred_CM) %>% 
  dplyr::select(-SC)

# 1c Separate and save
# 1c.i Begin loop
for(YYYY in 2010:2016){
  # 1c.i Keep only predictions for the relevant year
  avgscm %>% 
    filter(time == !!YYYY) %>% 
    write_csv(here::here('data_input_models', 'formatted', 'AVGSCM_annual_formatted',
                                paste0('avgscm_', YYYY, '.fst')))
}
