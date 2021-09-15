# File: STR_1_calculateMetricsOneFold.R
# Author: Sebastian T. Rowland <sr3463@cumc.columbia.edu>
# Date: 09/14/21

# Note: This code needs to be updated when we update BNE, especially for daily models. 

#' \code{calcMetricsOneFold} calculates a range of performance metrics for one BNE 
#' run for one fold. These will be aggregated in the cross validation. 
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

calculateMetricsOneFold <- function(YYYY, inputSet, kernel_sp, fold){
  
  #--------------------------#
  #### 0. example values: ####
  #--------------------------#
  
  # YYYY <- 2010; input <- c('av', 'gs', 'cm', 'js', 'cc')
  # kernel_sp <- 3.5; activeFold <- 'fold01'
    
  #------------------------------------------------#
  #### 1. wrangle predictions and ground truth: ####
  #------------------------------------------------#
  
  # 1a. read the ground truth values from that fold
  # note that (at least for now) the ground truth is kept as a column in the 
  # predictions dataset for that fold. 
  groundTruth <- readr::read_csv(here::here('BNE_inputs', 'input_models', 'combined', 'annual',
                               paste0('Predictions_', YYYY, '_' , 'avgscmjscc', '_', activeFold, '.csv'))) %>% 
    dplyr::select(lat, lon, aqs)
  
  # 1b. read the BNE output
  bneOut <- readBNEoutput(YYYY, inputSet, kernel_sp, fold)
  
  # 1e. Combine predictions and truth
  # we can't just use inner_join(c('lat', 'lon')) because some rounding can occur
  # 1e.i Convert to simple features
  bneOut <- bneOut %>%
    sf::st_as_sf(., coords = c("lon", "lat"), crs=st_crs('epsg:4326')) %>% 
    sf::st_transform(., crs=st_crs(projString)) 
  
  groundTruth <- groundTruth %>%
    sf::st_as_sf(., coords = c("lon", "lat"), crs=st_crs('epsg:4326')) %>% 
    sf::st_transform(., crs=st_crs(projString)) 
  
  # 1e.ii join via nearest neighbor
  groundTruth$pred_id  <- unlist(sf::st_nn(groundTruth, bneOut, k = 1, returnDist = FALSE))
  
  bneOut <- bneOut %>% 
    as.data.frame() %>%
    dplyr::mutate(pred_id  = row_number())
  
  dta <- bneOut %>% 
    dplyr::inner_join(ground_truth, by = 'pred_id')
  
  #-----------------------------#
  #### 2. calculate metrics: ####
  #-----------------------------#
  
  # 2a. mean absolute error 
  dta <- dta %>% 
    dplyr::mutate(E = pred_mean - aqs)
  MAE <- mean(abs(dta$E))
    
  # 2b. mean square error 
  dta <- dta %>% 
    dplyr::mutate(SE = E^2) 
  MSE <- mean(dta$SE)
  
  # 2c. R-squared
  # 2b.i calculate mean of ground truth 
  gt.mean <- mean(dta$aqs)
  # 2b.ii calculate total sum of squares 
  SStot <- sum((dta$aqs - gt.mean)^2)
  # 2b.iii calculate residual sum of squares 
  SSres <- sum(dta$SE)
  # 2b.iv calculate r-squared
  Rsq <- 1 - SSres / SStot
  
  # 2d. correlation of prediction and ground truth 
  corr <- cor(dta$aqs, dta$pred_mean) 
  
  # 2e. slope between prediction and ground truth 
  slope <- lm(aqs ~ pred_mean, data = dta)
  
  # 2f. coverage of 95% confidence interval 
  dta <- dta %>% 
    dplyr::mutate(cover = if_else(aqs > pred_05CI & aqs < pred_95CI, 1, 0))
  cover <- mean(dta$cover)

  # 2g. combine metrics into table 
  metrics <- data.frame(
    MAE = MAE, MSE = MSE, Rsq = Rsq, corr = corr, slope = slope, cover = cover, 
    obsCount = nrow(dta)
  )
  
  # 2h. Return result 
  return(metrics)

}
