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

readBNEoutput_offset <- function(YYYY, inputSet, kernel_sp, fold, offset){
  
  #--------------------#
  #### 1. load PPD: ####
  #--------------------#
  
  # 1a set the names of the columns 
  ColNames <- c('lat', 'lon',paste0('w_mean', '_', inputSet),
                paste0('w_sd', '_', inputSet), 'bias_mean', 'bias_sd', 
                'pred_mean', 'pred_sd', 'pred_95CIl', 'pred_95CIu',  
                'pred_min', 'pred_max', 'pred_median')
  
  # 1b create the runID that uniquely identifies this BNE run 
  runID <- paste0(paste(inputSet, collapse = ''), '_', kernel_sp, '_', YYYY,  '_', fold)
  
  # 1c read the BNE output 
  if(offset == 'noOffset'){
    dir.outputs <- 'individual_annual_noOffset'
    } else { dir.outputs <- 'individual_annual'}
  BNEoutput <- readr::read_csv(here::here('BNE_outputs', dir.outputs,
                                   paste0('BNE_', runID, '.csv')), 
                        col_names = ColNames) %>%
    mutate(run_id = runID)
  
  # 1d return that dataframe of BNE output 
  return(BNEoutput)
}
  