# File: STR_1_averageMetricsAcrossFolds.R
# Author: Sebastian T. Rowland <sr3463@cumc.columbia.edu>
# Date: 09/14/21

# Note: This code needs to be updated when we update BNE, especially for daily models. 

#' \code{averageMetricsAcrossFoldes} combines errors and other metrics from the 
#' cross-folds, as part of cross-validation.
#' 
#' @param YYYY An integer giving the year of predictions for the model.
#' @param inputSet A vector of strings, where each string is the acronym of a model. 
#' Should be in the order that the models are presented in the training & prediction dataset.
#' @param kernel_sp An integer giving the size of the spatial kernel of BNE.
#' 
#' @return A list with various named performance metrics 
#' 
#' @export
#' @importFrom magrittr %>%


averageMetricsAcrossFolds <- function(YYYY, inputSet, kernel_sp){
  
  #--------------------------#
  #### 0. example values: ####
  #--------------------------#
  
  # YYYY <- 2010; input <- c('av', 'gs', 'cm', 'js', 'cc')
  # kernel_sp <- 3.5;
  
  #---------------------------#
  #### 1. combine metrics: ####
  #---------------------------#
  
  # 1a. make foldList 
  foldList <- c(paste0('fold', str_pad(1:10, 2, 'left', '0')))
  
  # 1b. calculate metrics for each fold
  foldMetrics <- purrr::pmap(list(rep(YYYY, 10), rep(inputSet, 10), rep(kernel_sp, 10), 
                           foldList), 
                      calculateMetricsOneFold) %>% 
    bind_rows()
  
  # 1d. determine weights 
  foldMetrics <- foldMetrics %>% 
    dplyr::mutate(w = obsCount / sum(foldMetrics$obsCount))
  
  # 1e. average the metrics
  metricList <- list(MAE = sum(foldMetrics$E * w), 
                     RMSE = sqrt(sum(foldMetrics$SE * w)), 
                     corr = sum(foldMetrics$corr * w), 
                     slope = sum(foldMetrics$slope * w), 
                     Rsq = sum(foldMetrics$Rsq * w), 
                     cover = sum(foldMetrics$cover * w))
  
  # 1d. return Metrics 
  return(metricList)
}

