# File: STR_1_plotOneParameterHist.R
# Author: Sebastian T. Rowland <sr3463@cumc.columbia.edu>
# Date: 12/21/21

#' \code{plotOneParameterHist} makes a plot of a single parameter, typically from 
#' a BNE run. The parameter could be, for example the weight of a particular model,
#' or the predictive uncertainty. The code can also accept base model predictions, 
#' AQS observations, disagreement among base models, etc. A key feature is that 
#' the user can set the x-axis and bins to ensure consistent plotting across plots.
#' 
#' @param dta The dataset containing the parameter we will plot. Must have columns 
#' for lat and lon. Typical dta is the dataset of BNE PPD parameter summaries. 
#' An upcoming version could include option to average spatiotemporal dataset.
#' @param parameterName A string of the parameter of interest, such as 'w_mean_av' 
#' for the mean of the weight of the av input model. Valid names follow this pattern: 
#' {[parameter] _ [metric] _ [base model]} Some possible parameter names are:
#' w (weight) 
#' ens (core ensemble; the model combination from BNE)
#' res (residual process) 
#' pred (predicted variable; the dependent variable in the model)
#' for w, ens, res, and pred, metrics can be 'mean' or 'sd.' For now, pred can also 
#' have metrics '05CI', '95CI', 'min', 'max', and 'median'.
#' @param valueRange Either the string 'unique range' or a vector of the range to 
#' display. The unique range is the range of the data
#' @param binWidth Width of the bins, in units of the data.
#' @param mainTitle plot title. Use 'defaultMainTitle' if you want the 
#' default title for that parameterName
#' @param axisTextSize size of the text for the x axis 
#' @param axisTtitleSize size of the title for the x axis 
#' 
#' @return A ggplot object that can be printed as a png, pdf, etc. 
#' 
#' #' @seealso \code{\link{readBNEoutput}}
#' 
#' @export
#' @importFrom magrittr %>%

plotOneParameterHist <- function(
  dta, 
  parameterName, 
  valueRange = 'unique range', 
  binWidth = 5,
  mainTitle = 'defaultMainTitle', 
  xTitle = 'defaultMainTitle',
  axisTextSize = 15, 
  axisTitleSize = 20, 
  meanLine = 'none', 
  meanAnnY = 5, 
  meanAnnX = 0
  ){

  #--------------------------#
  #### 0. example values: ####
  #--------------------------#
  
  # dta <-bne.out; parameterName <- 'pred_mean'; titleYN = 'titleY'
  # valueRange = 'unique range'; mainTitle = 'plotTitle'; BW = 10; axisSize = 20 
  
  #-------------------------------#
  #### 1. wrangle BNE outputs: ####
  #-------------------------------#
  
  # 1a. rename the parameterName of interest 
  dta <- dta %>% 
    dplyr::rename(p := !!parameterName)
    
  # 1b. extract the name of the base model, if relevant
  if (stringr::str_detect(parameterName, 'w_mean') | 
      stringr::str_detect(parameterName, 'w_sd')) {
    baseModel <- stringr::str_split_fixed(parameterName, '_', 3)[3]
  }
  
  #--------------------------------#
  #### 2. create plot elements: ####
  #--------------------------------#
  
  #--------------------------#
  #### 2A. create titles: ####
  #--------------------------#
  
  # 2A.a create default titles
  # we only use these if the user does not define a title
  if (stringr::str_detect(parameterName, 'w_mean')) {
    mainTitle.default <- paste0('Weight of ', baseModel)
    
  } else if (stringr::str_detect(parameterName,'w_sd')) {
    mainTitle.default <- paste0('Uncertainty of ', baseModel, ' Weight')
    
  } else if (parameterName == 'ens_mean') {
    mainTitle.default <- 'Model Combination' 
    
  } else if (parameterName == 'ens_sd') {
    mainTitle.default <- 'Uncertainty of Model Combination' 
    
  } else if (parameterName == 'res_mean') {
    mainTitle.default <- 'Residual Process' 
    
  } else if (parameterName == 'res_sd') {
    mainTitle.default <- 'Uncertainty of Residual Process' 
    
  } else if (parameterName == 'y_mean' | 
             stringr::str_detect(parameterName, 'pred_')) {
    mainTitle.default <- expression('Predicted'~'Concentration'~'of'~'PM'[2.5])
    
  } else if (parameterName == 'y_sd') {
    mainTitle.default <- expression('Absolute'~'Uncertainty'~'of'~'PM'[2.5]~'Predictions')
  
  } else if (parameterName == 'y_sd_scaled') {
    mainTitle.default <- expression('Scaled'~'Uncertainty'~'of'~'PM'[2.5]~'Predictions')
    
  } else if (parameterName == 'base_model_sd') {
    mainTitle.default <- expression('Disagreement'~'of'~'PM'[2.5]~'Models')
    
  } else if (parameterName == 'base_maxW') {
    mainTitle.default <- 'Highest-Weighted Base Model'
    
  } else if (parameterName == 'pred_sd_scaled') {
    mainTitle.default <-'Uncertainty Scaled by Predicted Concentration'
    
  } else if (stringr::str_detect(parameterName, 'err_')) {
    mainTitle.default <-'Error'
    
  } else if (stringr::str_detect(parameterName, 'rmse_')) {
    mainTitle.default <-'RMSE'
    
  } else if (stringr::str_detect(parameterName, 'me_')) {
    mainTitle.default <-'Mean Error'
    
  } else {
    mainTitle.default <- parameterName
  }
  
  # 2A.b. set the titles we will use for the plot
  if (mainTitle == 'defaultMainTitle') {mainTitle <- mainTitle.default}
  if (xTitle == 'defaultMainTitle') {xTitle <- mainTitle.default}
  
  #----------------------------------#
  #### 2B. get values for x-axis: ####
  #----------------------------------#
  
  # 2B.a. get the relevant values 
  if (valueRange[1] == 'unique range') {
    p.min = min(dta$p)
    p.max = max(dta$p)
  } else {
    p.min <- valueRange[1]
    p.max <- valueRange[2]
    }

  #-----------------------#
  #### 3. create plot: ####
  #-----------------------#
  
  if (meanLine == 'meanLine') {
    meanLineObject <- ggplot2::geom_vline(xintercept = mean(dta$p), color = 'orangered4') 
    meanLineAnn <-  annotate('text', x = meanAnnX, y = meanAnnY, 
               label = paste0('Mean: ', round(mean(dta$p),2)), color = 'orangered4', 
               size = axisTextSize/2)
  } else {
    meanLineObject <- ggplot2::ggtitle(mainTitle)
    meanLineAnn <-  ggplot2::ggtitle(mainTitle) 
  }
  
  # 3a. create plot object
  p.gg <- ggplot2::ggplot(dta, aes(x = p)) + 
    ggplot2::geom_histogram(aes(y = stat(count) / sum(count)), binwidth = binWidth) + 
    meanLineObject + 
    meanLineAnn + 
    ggplot2::ggtitle(mainTitle) + 
    ggplot2::labs(x = xTitle, y = 'Percentage') + 
      scale_y_continuous(labels = scales::percent) + 
    ggplot2::xlim(p.min, p.max) + 
    ggplot2::theme_classic() + 
    ggplot2::theme(panel.grid.minor = element_blank()) + 
    ggplot2::theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), 
                   axis.text = element_text(size = axisTextSize), 
                   axis.title = element_text(size = axisTitleSize))  
  
  # 3b. return plot
  print(p.gg)
}  
