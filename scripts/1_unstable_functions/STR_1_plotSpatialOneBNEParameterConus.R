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

plotSpatialOneBNEParameter <- function(
  dta, 
  parameter, 
  legYN = 'legY', 
  titleYN = 'titleY',
  valueScale = 'unique scale', 
  mainTitle = 'plotTitle', 
  plotAQS = 'noAQS', 
  pointSize = 0.9, 
  pointShape =21
  ){

  #--------------------------#
  #### 0. example values: ####
  #--------------------------#
  
  # dta <- bne2010.offset
  #parameter <- 'pred_mean';  legYN <- 'LegY'
  #   titleYN = 'titleY'; valueArray = 'unique scale'
 # parameter <- 'pred_sd'; legYN <- 'LegY'
  
  #-------------------------------#
  #### 1. wrangle BNE outputs: ####
  #-------------------------------#
  
  # 1a. Rename the parameter of interest 
  dta <- dta %>% 
    dplyr::rename(p := !!parameter)
    
  # 1b. Convert to  simple feature
  dta <- dta %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326"))
  
  # 1c. Transform geographical coordinates to Albers Projection
  dta <- dta %>% 
    sf::st_transform(crs=sf::st_crs(projString))
  
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
    legendTitle <- 'Weight'
    plotTitle <- paste0('Weight of ', input)
    
    } else if (stringr::str_detect(parameter,'w_sd')){
      legendTitle <- 'Weight SD'
      plotTitle <- paste0('Standard Deviation of ', input, ' Weight')
      
    } else if (parameter == 'bias_mean'){
      legendTitle <- expression(atop('Offset', '('*mu*g/m^3*')'))
      plotTitle <- 'Offset Term' 
      
    } else if (parameter == 'bias_sd'){
      legendTitle <- expression(atop('Offset SD', '('*mu*g/m^3*')'))
      plotTitle <- 'Standard Deviation of Offset Term' 
      
    } else if (parameter == 'pred_mean'){
      legendTitle <- expression(atop(atop(' ', 'Predicted'),
                                 atop('PM'[2.5], '('*mu*g/m^3*')')))
      plotTitle <- expression('Predicted'~'Concentration'~'of'~'PM'[2.5])
      
    } else if (parameter == 'pred_sd'){
      legendTitle <- expression(atop(atop(' ', 'PM'[2.5]),
                                      atop('SD', '('*mu*g/m^3*')')))
      plotTitle <- expression('Standard'~'Deviation'~'of'~'PM'[2.5]~'Predictions')
    
    } else if (parameter == 'input_maxW'){
      legendTitle <- 'Most-Weighted'
      plotTitle <- 'Highest-Weighted inputs'
    } else{
      legendTitle <- parameter
      plotTitle <- parameter
    }
  
  # 2b. get the relevant values 
  if (valueScale == 'unique scale'){
    p.min = min(dta$p); p.max = max(dta$p)
    if (p.min >= 0){
      p.1 <- round(p.max*0.25, 2); p.2 = round(p.max*0.5, 2); p.3 = round(p.max*0.75, 2)
    } else{
       p.1 <- round(p.min/2, 2);  p.2 <- round(p.max/2, 2); 
    }
    p.min <- round(p.min, 2); p.max <- round(p.max, 2)
  }
  
  if(valueScale != 'unique scale'){
    # here you would extract the correct values from the object
    p.min <- valueScale[1]; p.1 <- valueScale[2]; p.2 <- valueScale[3];
    p.3 <- valueScale[4]; p.max <- valueScale[5]
  }
  
  # 2c. Set the aesthetic schemes 
  if(stringr::str_detect(parameter, 'w_mean') | stringr::str_detect(parameter, 'w_sd')){
    fillScheme <- viridis::scale_fill_viridis(
      direction = -1, 
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      limits = c(0, 1))
    colorScheme <- viridis::scale_color_viridis(
      direction = -1, 
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      limits = c(0, 1))
    aqsCol <- 'black'
    
  } else if(parameter == 'bias_mean'){
    fillScheme <- ggplot2::scale_fill_gradient2(
      low = "red", mid = "white",high = "blue", midpoint = 0, 
      breaks = c(p.min, p.1, 0, p.2, p.max),
      limits = c(p.min, p.max)) 
    colorScheme <- ggplot2::scale_color_gradient2(
      low = "red", mid = "white", high = "blue", midpoint = 0, 
      breaks = c(p.min, p.1, 0, p.2, p.max),
      limits = c(p.min, p.max)) 
    aqsCol <- 'black'
    
  } else if (parameter == 'pred_mean' | str_detect(parameter, '_pred')){
    fillScheme <- scico::scale_fill_scico(
      direction = -1, palette = 'hawaii',
      breaks = c(0, p.1, p.2, p.3, p.max),
      limits = c(0, p.max))
    colorScheme <- scico::scale_color_scico(
      direction = -1, palette = 'hawaii',
      breaks = c(0, p.1, p.2, p.3, p.max),
      limits = c(0, p.max))
    aqsCol <- 'orchid'
    
    } else if (parameter == 'pred_sd' | parameter == 'bias_sd' | parameter == 'pred_sd_scaled'){
    fillScheme <- viridis::scale_fill_viridis(
      direction = -1, option = 'magma',
      breaks = c(0, p.1, p.2, p.3, p.max),
      limits = c(0, p.max))
    colorScheme <- viridis::scale_color_viridis(
      direction = -1, option = 'magma',
      breaks = c(0, p.1, p.2, p.3, p.max),
      limits = c(0, p.max))
    aqsCol <- 'steelblue4'
    
    } else if (parameter == 'HighWI'){
  cbp1 <- c("#E69F00", "#56B4E9", "#009E73",
            "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  fillScheme <- ggplot2::scale_fill_manual(values=cbp1) 
  colorScheme <- ggplot2::scale_color_manual(values=cbp1) 
    } else if (p.min > 0 ){
      fillScheme <- viridis::scale_fill_viridis(
        direction = -1, option = 'magma',
        breaks = c(0, p.1, p.2, p.3, p.max),
        limits = c(0, p.max))
      colorScheme <- viridis::scale_color_viridis(
        direction = -1, option = 'magma',
        breaks = c(0, p.1, p.2, p.3, p.max),
        limits = c(0, p.max))
      aqsCol <- 'steelblue4'
      
    } else if (p.min < 0 ){
      fillScheme <- ggplot2::scale_fill_gradient2(
        low = "red", mid = "white",high = "blue", midpoint = 0, 
        breaks = c(p.min, p.1, 0, p.2, p.max),
        limits = c(p.min, p.max)) 
      colorScheme <- ggplot2::scale_color_gradient2(
        low = "red", mid = "white", high = "blue", midpoint = 0, 
        breaks = c(p.min, p.1, 0, p.2, p.max),
        limits = c(p.min, p.max)) 
      aqsCol <- 'black'
      
    } 
  
  # 2d. set legend position
  if(legYN == 'legY'){legPos <- 'right'}
  if(legYN == 'legN'){legPos <- 'none'}
  
  # 2e. decide whether there is a plot title by setting the size of the plot title
  if(titleYN == 'titleY'){plotSize <- 18}
  if(titleYN == 'titleN'){plotSize <- 0}
  
  #-----------------------#
  #### 3. create plot: ####
  #-----------------------#
  
  if(mainTitle == 'plotTitle'){pTitle <- plotTitle
  } else {pTitle <- mainTitle}
  
  
  if(is.data.frame(plotAQS)){
    aqsPoint <- ggplot2::geom_sf(data = plotAQS, fill = aqsCol, color = aqsCol, size = pointSize)
  } else {aqsPoint <- ggplot2::labs(x='', y='') }

  
  # 3a. create plot object
  p <- ggplot2::ggplot() + 
    ggplot2::geom_sf(fill = NA)  + 
    ggplot2::geom_sf(data = dta, aes(fill= p, color = p), size = pointSize, shape = pointShape) + 
    aqsPoint + 
    fillScheme + colorScheme +
    ggplot2::guides(fill = guide_colorbar(title = legendTitle), 
           color = guide_colorbar(title = legendTitle)) + 
    ggplot2::ggtitle(pTitle) + 
    ggplot2::labs(x='', y='') +
    ggplot2::theme(plot.title = element_text(size = plotSize), 
                  plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
    ggplot2::theme_void() + 
    ggplot2:: theme(legend.position = legPos, 
          legend.title  = element_text(size = 15)) 
  
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
