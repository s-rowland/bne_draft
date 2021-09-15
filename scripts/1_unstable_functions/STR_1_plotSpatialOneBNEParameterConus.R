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
  valueArray = 'unique scale'
  ){

  #--------------------------#
  #### 0. example values: ####
  #--------------------------#
  
  # dta <- readBNEoutput(2010, 'avgscmjscc', 3.5, 'all')
  #parameter <- 'bias_mean';  legYN <- 'LegY'
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
  if (string::str_detect(parameter, 'w_mean')){
    input <- stringr::str_replace(parameter, 'w_mean', '')
  }
  if (string::str_detect(parameter, 'w_sd')){
    input <- stringr::str_replace(parameter, 'w_sd', '')
  }
  
  #--------------------------------#
  #### 2. create plot elements: ####
  #--------------------------------#
  
  # 2a create titles 
  # we need a title for the legend for the the plot 
  if (string::str_detect(parameter, 'w_mean')){
    legendTitle <- 'Weight'
    plotTitle <- paste0('Weight of ', input)
    
    } else if (parameter == 'w_sd'){
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
    }
  
  # 2b. get the relevant values 
  if (valueArray == 'unique scale'){
    p.min = min(dta$p); p.max = min(dta$p)
    if (minVal >= 0){
      p.1 <- round(p.max*0.25, 2); p.2 = round(p.max*0.5, 2); p.3 = round(p.max*0.75, 2)
    } else{
       p.1 <- round(p.min/2, 2);  p.2 <- round(p.max/2, 2); 
    }
  }
  
  if(valueArray != 'unique scale'){
    # here you would extract the correct values from the object
  }
  
  # 2c. Set the aesthetic schemes 
  if(stringr::str_detect(parameter, 'w_')){
    fillScheme <- viridis::scale_fill_viridis(
      direction = -1, 
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      limits = c(0, 1))
    colorScheme <- viridis::scale_color_viridis(
      direction = -1, 
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      limits = c(0, 1))
    
  } else if(parameterName == 'bias_mean'){
    fillScheme <- ggplot::scale_fill_gradient2(
      low = "red", mid = "white",high = "blue", midpoint = 0, 
      breaks = c(p.min, p.1, 0, p.2, p.max),
      limits = c(p.min, p.max)) 
    colorScheme <- ggplot::scale_color_gradient2(
      low = "red", mid = "white", high = "blue", midpoint = 0, 
      breaks = c(p.min, p.1, 0, p.2, p.max),
      limits = c(p.min, p.max)) 
    
  } else if (parameter == 'pred_mean'){
    fillScheme <- scico::scale_fill_scico(
      direction = -1, palette = 'hawaii',
      breaks = c(0, p.1, p.2, p.3, p.max),
      limits = c(0, p.max))
    colorScheme <- scico::scale_color_scico(
      direction = -1, palette = 'hawaii',
      breaks = c(0, p.1, p.2, p.3, p.max),
      limits = c(0, p.max))
    
    } else if (parameter == 'pred_sd'){
    fillScheme <- viridis::scale_fill_viridis(
      direction = -1, option = 'magma',
      breaks = c(0, p.1, p.2, p.3, p.max),
      limits = c(0, p.max))
    colorScheme <- viridis::scale_color_viridis(
      direction = -1, option = 'magma',
      breaks = c(0, p.1, p.2, p.3, p.max),
      limits = c(0, p.max))
    
    }  else if (parameterName == 'HighWI'){
  cbp1 <- c("#E69F00", "#56B4E9", "#009E73",
            "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  fillScheme <- ggplot::scale_fill_manual(values=cbp1) 
  colorScheme <- ggplot::scale_color_manual(values=cbp1) 
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
  
  # 3a. create plot object
  p <- ggplot::ggplot() + 
    ggplot::geom_sf(fill = NA)  + 
    ggplot::geom_sf(data = dta, aes(fill= activeVar, color = activeVar), size = 0.9) + 
    fillScheme + colorScheme +
    ggplot::guides(fill = guide_colorbar(title = ParameterTitle), 
           color = guide_colorbar(title = ParameterTitle)) + 
    ggplot::ggtitle(PlotTitle) + 
    ggplot::labs(x='', y='') +
    ggplot::theme(plot.title = element_text(size = plotSize), 
                  plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
    ggplot::theme_void() + 
    ggplot:: theme(legend.position = legPos, 
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
