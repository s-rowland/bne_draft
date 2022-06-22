# File: STR_1_readbBNEoutput.R
# Author: Sebastian T. Rowland <sr3463@cumc.columbia.edu>
# Date: 12/21/21

#' \code{readBNEoutout} loads outputs from a single BNE run, and assigns a 
#' column with run_id, a unique identifier for that run. 
#' The code is specially designed to deal with the outputs of the Matlab version.
#' 
#' @param YYYY An integer giving the year of predictions for the model.
#' @param baseModelSet A vector of strings, where each string is the acronym of a base model. 
#' Should be in the order that the models are presented in the training & prediction dataset.
#' @param lenScaleSpace  An integer giving the size of the spatial kernel of BNE.
#' @param lenScaleTime  An integer giving the size of the temporal kernel of BNE.
#' 'spatialBNE' refers to the spatial BNE, where there is no temporal kernel 
#' (ie stratified by time)
#' @param fold A string determining which data the model was trained on - 'all' 
#' refers to model trained on the entire dataset; 01, 02, ect refer to cross-validation folds.
#' 'NYS' is New York State, 'cities' is the selected cities.
#' @param residualProcess A string determining whether we use a version of BNE 
#' with (resid) or without (noResid) the residual process. Useful for testing
#' 
#' @return A tibble (in wide format) containing various values summarizing the 
#' Posterior Predictive Distribution of the BNE run, such as the mean value of the 
#' weights and offset term, as well as the mean, SD, and 95% CI of the predicted value. 
#' 
#' @export
#' @importFrom magrittr %>%
#' 

readBNEoutput <- function(YYYY = 2010, 
                          baseModelSet = c('av', 'gs', 'cm', 'js', 'cc'), 
                          lenScaleSpace = 1.5, 
                          lenScaleTime = 'spatialBNE',
                          fold = 'all', 
                          residualProcess = 'resid'){
  
  #-----------------------------#
  #### 0. example arguments: ####
  #-----------------------------#
  
  #spT = 'spatial'; YYYY = 2010; baseModelSet = c('av', 'gs', 'cm', 'js', 'cc') 
  #lenScaleSpace = 3.5; lenScaleTime = '0.008'; fold = 'fold01'
  
  #--------------------#
  #### 1. load PPD: ####
  #--------------------#
  
  # 1a. set the names of the columns 
  ColNames <- c('lat', 'lon', paste0('w_mean', '_', baseModelSet),
                paste0('w_sd', '_', baseModelSet),  'ens_mean', 'ens_sd', 'res_mean', 'res_sd', 
                'pred_mean', 'pred_sd', 'pred_95CIl', 'pred_95CIu',  'pred_68CIl', 'pred_68CIu', 
                'pred_min', 'pred_max', 'pred_median', 'pred_skew', 'pred_kurtosis')
  
  # 1b. make some objects that differ by spatial versus spatiotemporal
  if(lenScaleTime == 'spatialBNE') {
    parameterString <- paste0(lenScaleSpace)
    spt <- 'spatial'; skipCount <- 0
  } else {
    parameterString <- paste0(lenScaleSpace, '_', lenScaleTime)
    spt <- 'spatiotemp'; skipCount <- 1
  }
  
  # 1c. get the string for the residual process
  # since the default is to include the residual process, we only bother
  # mentioning it in the runID if we don't include the residual
  if(residualProcess == 'resid') {resid <- ''}
  if(residualProcess == 'noResid') {resid <- '_noResid'}
  
  # 1d. generate runID that uniquely identifies this BNE run 
  runID <- paste0(paste(baseModelSet, collapse = ''), '_', parameterString, '_', YYYY, '_', fold, resid)
  
  # 1e. read the BNE output 
  bneOut <- readr::read_csv(here::here('outputs', 'pm25',
                                          paste0(spt, '_annual'),
                                   paste0('BNE_', runID, '.csv')), 
                        col_names = ColNames, skip = skipCount) %>%
    mutate(run_id = runID)
  
  # 1e. return that dataframe of BNE output 
  return(bneOut)
}
  