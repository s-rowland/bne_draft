# File: STR_1_plotSpatialOneBNEParameterConus.R
# Author: Sebastian T. Rowland <sr3463@cumc.columbia.edu>
# Date: 09/14/21

# Note: For now this code hard-codes values, but ideally it would not. 

#' \code{plotSpatialOneBNEParameterConus} makes a plot of a single parameter from 
#' a BNE run. The parameter could be, for example the weight of a particular model,
#' or the predictive uncertainty. This version of the code is specialized for the 
#' current CONUS-based model testing and application; a more generalized verzion is coming. 
#' 
#' 
#' @param BNEoutput A dataset of summarizing the PPD of a BNE run. Note that once 
#' have a time-varying BNE, we will need a parameter for capture_time, which should 
#' also allow for taking an average
#' @param parameter A string of the parameter of interest, such as 'w_mean_av' 
#' for the mean of the weight of the av input model. Valid names follow this pattern: 
#' {[parameter] _ [metric] _ [input model name]} Possible parameter names are:
#' w (weight) 
#' bias (offset term) 
#' pred (predicted variable; the dependent variable in the model)
#' for w, bias, and pred, metrics can be 'mean' or 'sd.' For now, pred can also 
#' have metrics '05CI', '95CI', 'min', 'max', and 'median'. 
#' The input model name is only used for weights
#' @param legYN A string determining whether to include a legend. 
#' Default is to include a legend. Possible values are 'legY' and 'legN'.
#' @param titleYN A string determining whether to include the main title. 
#' Default is to include a title. Possible values are 'titleY' and 'titleN'.
#' @param valueArray An array containing the values to be used as break points for 
#' the legend and color scheme. There should be 5 values per metric. If a valueArray
#' is not provided, then the program will calculate breakpoints from just the 
#' given BNE run. Could also set it to create a value array from all of the capture_times
#' 
#' @return A ggplot object that can be printed as a png, pdf, etc. 
#' 
#' #' @seealso \code{\link{readBNEoutput}}
#' 
#' @export
#' @importFrom magrittr %>%

plotOneParameterHist <- function(
  dta, 
  parameter, 
  titleYN = 'titleY',
  xRange = 'unique range', 
  mainTitle = 'plotTitle', 
  binCount = 'unique binCount',
  axisSize = 15, 
  axisTitleSize = 20
  ){

  #--------------------------#
  #### 0. example values: ####
  #--------------------------#
  
  # dta <-bne.out; parameter <- 'pred_mean'; titleYN = 'titleY'
  # xRange = 'unique range'; mainTitle = 'plotTitle'; BW = 10; axisSize = 20 
  
  #-------------------------------#
  #### 1. wrangle BNE outputs: ####
  #-------------------------------#
  
  # 1a. Rename the parameter of interest 
  dta <- dta %>% 
    dplyr::rename(p := !!parameter)
    
  # 1d. extract the name of the input model, if relevant
  if (stringr::str_detect(parameter, 'w_mean')){
    input <- stringr::str_replace(parameter, 'w_mean', '')
  }
  if (stringr::str_detect(parameter, 'w_sd')){
    input <- stringr::str_replace(parameter, 'w_sd', '')
  }
  
  #--------------------------------#
  #### 2. create plot elements: ####
  #--------------------------------#
  
  # 2a create titles 
  # we need a title for the legend for the the plot 
  if (stringr::str_detect(parameter, 'w_mean')){
    plotTitle <- paste0('Weight of ', input)
    
    } else if (stringr::str_detect(parameter,'w_sd')){
      plotTitle <- paste0('Standard Deviation of ', input, ' Weight')
      
    } else if (parameter == 'bias_mean'){
      plotTitle <- 'Residual Process' 
      
    } else if (parameter == 'bias_sd'){
      plotTitle <- 'Standard Deviation of Residual Process' 
      
    } else if (parameter == 'pred_mean' | str_detect(parameter, '_pred')){
      plotTitle <- expression('Predicted'~'Concentration'~'of'~'PM'[2.5])
      
    } else if (parameter == 'pred_sd'){
      plotTitle <- expression('Standard'~'Deviation'~'of'~'PM'[2.5]~'Predictions')
    
    } else if (parameter == 'pred_sd_scaled'){
      plotTitle <- 'Scaled Uncertainty (%)'
      
    } else if (parameter == 'input_maxW'){
      plotTitle <- 'Highest-Weighted inputs'
    } else{
      plotTitle <- parameter
    }
  
  # 2b. get the relevant values 
  if (xRange == 'unique range'){ p.min = min(dta$p); p.max = max(dta$p)}
  # here you would extract the correct values from the object
  if(xRange != 'unique range'){p.min <- xRange[1];  p.max <- xRange[2]}
  
  # 2c. decide whether there is a plot title by setting the size of the plot title
  if(titleYN == 'titleY'){plotSize <- 18}
  if(titleYN == 'titleN'){plotSize <- 0}
  
  # 2d. get the binwidth 
  if(binCount == 'unique binCount'){binCount <- 20}
  
  #-----------------------#
  #### 3. create plot: ####
  #-----------------------#
  
  if(mainTitle == 'plotTitle'){pTitle <- plotTitle
  } else {pTitle <- mainTitle}
  
  # 3a. create plot object
  p <- ggplot2::ggplot(dta, aes(x = p)) + 
    ggplot2::geom_histogram(aes(y = stat(count) / sum(count)), bins = binCount) + 
    ggplot2::ggtitle(' ') + 
    labs(x = pTitle, y = 'Percentage') + 
      scale_y_continuous(labels = scales::percent) + 
    xlim(p.min, p.max) + 
    ggplot2::theme_bw() + 
    ggplot2::theme(panel.grid.minor = element_blank()) + 
    ggplot2::theme(plot.title = element_text(size = plotSize), 
                   plot.margin = unit(c(0, 0, 0, 0), "cm"), 
                   axis.text = element_text(size = axisSize), 
                   axis.title = element_text(size = axisTitleSize)) 
  
  # 3b. return plot
  print(p)
}  


# test
#YYYY <- 2012;  inputSet <- c('AV', 'GS', 'CM', 'JS',  'CC');
# parameterName <- 'bias_mean'; input <- ''
# parameterName <- 'w_mean'; input <- 'AV'
#png('~/Desktop/test.png')

#plotSpatial_oneBNEParameter(2012, 'AVGSCMJSCC', 
 #                           'bias_mean', '', 'test')
#dev.off()
#plotSpatial_oneBNEParameter(2012, c('AV', 'GS', 'CM', 'JS',  'CC'), 
 #                           'w_mean', 'AV')
