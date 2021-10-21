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


calculateCVMetricsSpt <- function(inputSet = c('av', 'gs', 'cm', 'js', 'cc'), 
                                  lenScaleSpace = 3.5, 
                                  lenScaleTime = 'spatialOnly', 
                                  region =  'all', 
                                  season = 'all'){
  
  #--------------------------#
  #### 0. example values: ####
  #--------------------------#
  
  # inputSet = c('av', 'gs', 'cm', 'js', 'cc'); lenScaleSpace = 3.5
  # lenScaleTime = 0.008; region =  'all'; season = 'all'
  
  #---------------------------#
  #### 1. combine metrics: ####
  #---------------------------#
  
  # 1a. Read the bne outputs, combined with ground truth
  inputSetList <- list(inputSet, inputSet, inputSet, inputSet, inputSet, inputSet)
  foldList <- c(paste0('fold', str_pad(1:10, 2, 'left', '0')))
   
  bneOut <- pmap(list(rep(rep('spatiotemp',6), 10),
                      rep(2010:2015, 10), 
                      rep(inputSetList, 10), 
                      rep(rep(lenScaleSpace, 6), 10),
                      rep(rep(lenScaleTime, 6), 10),
                      sort(rep(foldList, 6))), 
                 readBNEoutput) 
  bneOut <- pmap(list(bneOut, 
                      rep(2010:2015, 10),
                      sort(rep(foldList, 6))), 
                 addGroundTruth) %>%
    bind_rows() 
  
  # 1b. add column for year 
 if (lenScaleTime == 'spatialOnly'){
   bneOut <- bneOut %>% 
     tidyr::separate(run_id, c('inputSet', 'len_scale_sp', 'year', 'fold'), sep = '_')
 } else {
   bneOut <- bneOut %>% 
     tidyr::separate(run_id, c('inputSet', 'len_scale_sp', 'len_scale_t', 'year', 'fold'), sep = '_')
 }
  
  #--------------------------------------#
  #### 2. isolate points of interest: ####
  #--------------------------------------#
  
  # 2a. identify EPA region 
  # 2a.i read EPA region shapefile 
  if (region != 'all'){
    bneOut <- bneOut %>% filter(region == !!region)
  }
  
  #-----------------------------#
  #### 3. calculate metrics: ####
  #-----------------------------#
  
  # 3a. mean error
  bneOut <- bneOut %>% 
    dplyr::mutate(E = pred_mean - aqs)
  ME <- mean(bneOut$E)
  
  # 3b. mean absolute error 
  MAE <- mean(abs(bneOut$E))
  
  # 3c. mean square error 
  bneOut <- bneOut %>% 
    dplyr::mutate(SE = E^2) 
  RMSE <- sqrt(mean(bneOut$SE))
  
  # 3d. R-squared
  # 3d.i calculate mean of ground truth 
  gt.mean <- mean(bneOut$aqs)
  # 3d.ii calculate total sum of squares 
  SStot <- sum((bneOut$aqs - gt.mean)^2)
  # 3d.iii calculate residual sum of squares 
  SSres <- sum(bneOut$SE)
  # 3d.iv calculate r-squared
  Rsq <- 1 - SSres / SStot
  
  # 3e. correlation of prediction and ground truth 
  corr <- cor(bneOut$aqs, bneOut$pred_mean) 
  
  # 3f. slope between prediction and ground truth 
  slope <- summary(lm(aqs ~ pred_mean, data = bneOut))$coefficients[2,1]
  
  # 3g. coverage of 95% confidence interval 
  bneOut <- bneOut %>% 
    dplyr::mutate(coverage = if_else(aqs > pred_95CIl & aqs < pred_95CIu, 1, 0))
  coverage <- mean(bneOut$coverage)
  
  # 3h. combine metrics into table 
  metrics <- data.frame(len_scale_sp = len_scale_sp, len_scale_t = len_scale_t,
    region = region, season = season, 
                  ME = round(ME, 2), MAE = round(MAE, 2), RMSE = round(RMSE, 2), 
                  Rsq = round(Rsq, 2), coverage = round(coverage, 2), 
                  corr = round(corr, 2), slope = round(slope, 2))
  
  # 3g. return Metrics 
  return(metrics)
}

