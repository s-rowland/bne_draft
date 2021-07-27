# Project Set up 
# BNE Fast Uncertainty Analysis for ISEE
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Function to Plot Spatially One BNE Parameter

####********************
#### 0: Preparation ####
####********************


####*****************
#### 1: Function ####
####*****************

# 1a Begin function
readBNEoutput <- function(YYYY, InputSet, kScale, activeFold){
# InputSet should be a vector of strings
  # 1b Set the names of the columns 
  ColNames <- c('lat', 'lon',paste0('w_mean', '_', InputSet),
                paste0('w_sd', '_', InputSet), 'bias_mean', 'bias_sd', 
                'pred_mean', 'pred_sd', 'pred_05CI', 'pred_95CI', 
                'pred_min', 'pred_max', 'pred_median')
  
  # 1c Create the runID that uniquely identifies this BNE run 
  runID <- paste0(YYYY, '_', paste(InputSet, collapse = ''), '_', kScale, '_', activeFold)
  
  # 1d Read the BNE output 
  BNEoutput <- read_csv(here::here('BNE_Outputs/annual',
                                   paste0(runID, '.csv')), 
                        col_names = ColNames) %>%
    mutate(run_id = runID)
  
  # 1e Return that dataframe of BNE output 
  return(BNEoutput)
}
  