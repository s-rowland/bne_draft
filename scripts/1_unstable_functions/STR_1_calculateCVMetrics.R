# File: STR_1_calculateCVMetrics.R
# Author: Sebastian T. Rowland <sr3463@cumc.columbia.edu>
# Date: 12/21/2021

# Note: This code is specifically designed for annual BNE from the Matlab version
# revisions may be required for daily data, the Python version, etc

#' \code{calculateCVMetrics} combines errors and other metrics from the 
#' cross-folds, as part of cross-validation.
#' 
#' @param yearSet vector of the years we will look at. Can be a single year
#' @param baseModelSet A vector of strings, where each string is the acronym of a model. 
#' Should be in the order that the models are presented in the training & prediction dataset.
#' @param lenScaleSpace  An integer giving the size of the spatial kernel of BNE.
#' @param lenScaleTime  An integer giving the size of the temporal kernel of BNE.
#' 'spatialBNE' refers to the spatial BNE, where there is no temporal kernel 
#' (ie stratified by time)
#' @param region An integer for the EPA region, if you want to look at average 
#' performance within a particular region 
#' @param season An character string for season, if you want to look at average
#'  performance during a particular season
#' @param residualProcess A string determining whether we use a version of BNE 
#' with (resid) or without (noResid) the residual process. Useful for testing
#' 
#' @return A list with various named performance metrics 
#' 
#' @export
#' @importFrom magrittr %>%

calculateCVMetrics <- function(yearSet = 2010:2016,
                                baseModelSet = c('av', 'gs', 'cm', 'js', 'cc'), 
                                  lenScaleSpace = 1.5, 
                                  lenScaleTime = 'spatialBNE', 
                                  region =  'all', 
                                  season = 'all', 
                               residualProcess = 'resid') {
  
  #--------------------------#
  #### 0. example values: ####
  #--------------------------#
  
  # baseModelSet <- c('av', 'gs', 'cm', 'js', 'cc', 'me')
  # baseModelSet = c('av', 'gs', 'cm', 'js', 'cc'); lenScaleSpace = 3.5
  # lenScaleTime =1; region =  'all'; season = 'all'
  
  #------------------------------#
  #### 1. gather BNE outputs: ####
  #------------------------------#
  
  # 1a. create a list of the folds
  folds.list <- c(paste0('fold', str_pad(1:10, 2, 'left', '0')))
  
  # 1b. read the BNE outputs
  # we rep everything by 10 for the folds 
  # and by yearSet because we need to read the outputs for each year
  bne.out <- purrr::pmap(list(rep(yearSet, 10), 
                      list(baseModelSet), 
                      lenScaleSpace, 
                      lenScaleTime,
                      #rep(rep(lenScaleSpace, length(yearSet)), 10),
                      #rep(rep(lenScaleTime, length(yearSet)), 10),
                      sort(rep(folds.list, length(yearSet))), 
                      residualProcess), 
                 readBNEoutput) 
  
  # 1c. add the ground truth
  bne.out <- purrr::pmap(list(bne.out, 
                      rep(yearSet),
                      sort(rep(folds.list, length(yearSet)))), 
                 addGroundTruth) %>%
    dplyr::bind_rows() 
  
  # 1d. add column for year 
 if (lenScaleTime == 'spatialBNE') {
   bne.out <- bne.out %>% 
     tidyr::separate(run_id, c('baseModelSet', 'len_scale_sp', 'year', 'fold'), sep = '_')
 } else {
   bne.out <- bne.out %>% 
     tidyr::separate(run_id, c('baseModelSet', 'len_scale_sp', 'len_scale_t', 'year', 'fold'), sep = '_')
 }
  
  #--------------------------------------#
  #### 2. isolate points of interest: ####
  #--------------------------------------#
  
  # 2a. identify EPA region 
  # 2a.i read EPA region shapefile 
  if (region != 'all'){
    bne.out <- bne.out %>% 
      dplyr::filter(region == !!region)
  }
  
  #-----------------------------#
  #### 3. calculate metrics: ####
  #-----------------------------#
  
  # 3a. mean error
  bne.out <- bne.out %>% 
    dplyr::mutate(E = pred_mean - aqs)
  ME <- mean(bne.out$E)
  
  # 3b. mean absolute error 
  MAE <- mean(abs(bne.out$E))
  
  # 3c. root mean square error 
  bne.out <- bne.out %>% 
    dplyr::mutate(SE = E^2) 
  RMSE <- sqrt(mean(bne.out$SE))
  
  # 3d. R-squared
  # 3d.i calculate mean of ground truth 
  gt.mean <- mean(bne.out$aqs)
  # 3d.ii calculate total sum of squares 
  SStot <- sum((bne.out$aqs - gt.mean)^2)
  # 3d.iii calculate residual sum of squares 
  SSres <- sum(bne.out$SE)
  # 3d.iv calculate r-squared
  Rsq <- 1 - SSres / SStot
  
  # 3e. calibrated error 
  calErr <- mean(bne.out$E / (bne.out$pred_95CIu - pred_95CIl))
    
  # 3f. slope between prediction and ground truth 
  slope <- summary(lm(aqs ~ pred_mean, data = bne.out))$coefficients[2,1]
  
  # 3g. coverage of 95% confidence interval 
  bne.out <- bne.out %>% 
    dplyr::mutate(coverage = if_else(aqs > pred_95CIl & aqs < pred_95CIu, 1, 0))
  coverage <- mean(bne.out$coverage)
  
  # 3h. combine metrics into table 
  metrics <- data.frame(len_scale_sp = lenScaleSpace, len_scale_t = lenScaleTime,
    region = region, season = season, 
    Rsq = round(Rsq, 4), coverage = round(coverage, 4), 
    RMSE = round(RMSE, 2), calErr = round(calErr, 2),
    MAE = round(MAE, 2), slope = round(slope, 2), ME = round(ME, 2))
  
  # 3g. return performace metrics 
  return(metrics)
}

