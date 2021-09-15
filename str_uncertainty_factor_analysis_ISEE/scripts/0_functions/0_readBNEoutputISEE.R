# Combine BNE Outputs
# Uncertainty Analysis for ISEE
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Define Function to Read BNE Outputs from Specific Folder
# 2: Combine BNE Outputs

####********************
#### 0: Preparation ####
####********************

####*****************************************************************
#### 1: Define Function to Read BNE Outputs from Specific Folder ####
####*****************************************************************

# 1a Begin function
readBNEoutputISEE <- function(YYYY){
  # InputSet should be a vector of strings
  # 1b Set the other parameters of the runs
  kScale <- 3.5
  activeFold <- 'all' # the 'all' fold is the complete prediction dataset
  InputSet <- c('AV', 'GS', 'CM', 'JS', 'CC')
  
  # 1c Set the names of the columns 
  ColNames <- c('lat', 'lon',paste0('w_mean', '_', InputSet),
                paste0('w_sd', '_', InputSet), 'bias_mean', 'bias_sd', 
                'pred_mean', 'pred_sd')
  
  # 1d Create the RunID that uniquely identifies this BNE run 
  runID <- paste0(YYYY, '_', paste(InputSet, collapse = ''), '_', kScale, '_', activeFold)
  
  # 1e Read the BNE output 
  BNEoutput <- read_csv(here::here('uncertainty_factor_analysis_ISEE', 'BNE_outputs',
                                   paste0(runID, '.csv')), 
                        col_names = ColNames) %>%
    mutate(run_id = runID, YYYY = YYYY)
  
  # 1f Return that dataframe of BNE output 
  return(BNEoutput)
}
