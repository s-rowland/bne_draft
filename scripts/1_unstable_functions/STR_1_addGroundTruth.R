# File: STR_1_readBNEoutput.R
# Author: Sebastian T. Rowland <sr3463@cumc.columbia.edu>
# Date: 09/14/21

# Note: This code needs to be updated when we update BNE, especially for daily models. 

#' \code{readBNEoutput} loads outputs from a single BNE run, and assigns a 
#' column with run_id, a unique identifier for that run. For now, we specify run 
#' based on the year of the run, input models, the kernel_sp, and the folds. 
#' 
#' @param YYYY An integer giving the year of predictions for the model.
#' @param inputSet A vector of strings, where each string is the acronym of a model. 
#' Should be in the order that the models are presented in the training & prediction dataset.
#' @param kernel_sp An integer giving the size of the spatial kernel of BNE.
#' @param fold A string determining which data the model was trained on - 'all' 
#' refers to model trained on the entire dataset; 01, 02, ect refer to cross-validation folds.
#' 
#' @return A tibble (in wide format) containing various values summarizing the 
#' Posterior Predictive Distribution of the BNE run, such as the mean value of the 
#' weights and offset term, as well as the mean, SD, and 95% CI of the predicted value. 
#' 
#' @export
#' @importFrom magrittr %>%

addGroundTruth <- function(bneOut, YYYY, activeFold){
  
  #--------------------#
  #### 1. load PPD: ####
  #--------------------#

# 1a. read ground truth data set  
  groundTruth <- readr::read_csv(here::here('BNE_inputs', 'prediction_datasets', 'individual_annual',
                                            paste0('predictions_avgscmjscc_', YYYY,  '_', activeFold, '.csv'))) %>% 
    dplyr::select(obs_pm2_5, region) %>% 
    rename(aqs = obs_pm2_5)
  
  # 1b. combine with bne outputs
  BNEoutput <- bind_cols(groundTruth, bneOut)
  
  # 1c. return that dataframe of BNE output 
  return(BNEoutput)
}
  