# Process Daily AQS Data
# Assess Daily Input Models
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Read Data 
# 2: Curate Monitors 
# 3: Save

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.rtf")
  source(here::here('scripts', 'a_set_up', "a_00_setUp_env.R"))
}

####********************
#### 1: Read Data ####
####********************

# 1a Read the aqs data 
read_aqs <- function(YYYY){
  read_csv(here::here('data_ground_truth', 'formatted', 
                      paste0('aqs_annual_', YYYY, '_formatted.csv')))
}
dta <- map(c(2010:2015), read_aqs) %>% bind_rows()
 
# Save 
dta %>% 
  write_csv(here::here('data_ground_truth', 'formatted', 
                       paste0('aqs_annual_formatted.csv')))
