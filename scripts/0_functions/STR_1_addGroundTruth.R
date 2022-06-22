# File: STR_1_addGroundTruth.R
# Author: Sebastian T. Rowland <sr3463@cumc.columbia.edu>
# Date: 12/21/2021

# Note: This code is specifically designed for annual BNE from the Matlab version
# revisions may be required for daily data, the Python version, etc

#' \code{addGroundTruth} loads outputs from a single BNE run, and assigns a 
#' column with run_id, a unique identifier for that run. For now, we specify run 
#' based on the year of the run, input models, the kernel_sp, and the folds. 
#' 
#' @param bneOut the outputs of a test run of BNE
#' @param YYYY An integer giving the year of predictions for the model.
#' @param activeFold the testing fold
#' 
#' @return A tibble (in wide format) of the original BNE outputs, plus a column
#' for aqs observations 
#' 
#' @export
#' @importFrom magrittr %>%

addGroundTruth <- function(bneOut, YYYY, activeFold){
  
  #bneOut <- readBNEoutput(2010, c('av', 'gs', 'cm', 'js', 'cc'), 3.5, 1, 'fold01')
  # YYYY <- 2010; activeFold <- 'fold01'
  #--------------------#
  #### 1. load PPD: ####
  #--------------------#

# 1a. read ground truth data set  
  groundTruth <- readr::read_csv(here::here('inputs', 'pm25', 'prediction_datasets', 
                                            'annual_individual',
                                            paste0('predictions_avgscmjscc_', YYYY, 
                                                   '_', activeFold, '.csv'))) %>% 
    dplyr::select(obs_pm2_5, region) %>% 
    dplyr::rename(aqs = obs_pm2_5)
  
  # 1b. combine with bne outputs
  BNEoutput <- dplyr::bind_cols(groundTruth, bneOut)
  
  # 1c. return that dataframe of BNE output 
  return(BNEoutput)
}
  