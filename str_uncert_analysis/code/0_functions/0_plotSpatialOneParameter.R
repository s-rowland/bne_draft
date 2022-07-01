# Project Set up 
# BNE Fast Uncertainty Analysis for ISEE
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Function to Plot Spatially One BNE Parameter

####********************
#### 0: Preparation ####
####********************

####*****************
#### 1: Function ####
####*****************

# 1a Begin function
plotSpatialOneParameter <- function(dta, VarName, Subtitle, LegYN){

  # dta <- dtaPred; VarName <-'inputSD'; Subtitle <- ''; LegYN <- 'LegY'

  ####**********************
  #### 1A: Wrangle Data ####
  ####**********************
    
  # 1A.a Rename the parameter of interest 
  dta <- dta %>% 
    rename(activeVar := !!VarName)
  
  # 1A.b Keep only observations within conus
  if(!exists("conus")){
    conus <- st_read(here::here('Data', 'general', 'spatial_outlines', 'conus.shp'), 
                     crs = projString)
  }
  
  # 1A.c Filter by CONUS
  dta <- dta %>% 
    st_join(conus, st_intersects) %>% 
    filter(!is.na(g))
  
  ####************************
  #### 1B: Plot Parameter ####
  ####************************
  
  # 1B.a Create ParameterTitle 
  #if(ParameterName == 'w_mean'){ParameterTitle <- paste0('Mean of ', Input, ' Weight')}
 # if(ParameterName == 'w_sd'){ParameterTitle <- paste0('SD of ', Input, ' Weight')}
  #if(ParameterName == 'bias_mean'){ParameterTitle <- 'Mean of Bias Term'}
  #if(ParameterName == 'bias_std'){ParameterTitle <- 'SD of Bias Term'}
  #if(ParameterName == 'y_mean'){ParameterTitle <- 'Mean of Prediction'}
  #if(ParameterName == 'y_std'){ParameterTitle <- 'SD of Prediction'}
  if(VarName %in% set_AllInputSet(2010)){ParameterTitle <- 'Input Prediction'}
  if(VarName == 'aqs'){ParameterTitle <- 'AQS'}
  if(VarName == 'pred_uncert'){ParameterTitle <- 'Prediction Uncertainty'}
  if(VarName == 'MonitorCount'){ParameterTitle <- 'Monitor Count'}
  if(VarName == 'popD'){ParameterTitle <- 'Population Density'}
  if(VarName == 'pred_mean'){ParameterTitle <- 'Predicted Concentration'}
  if(VarName == 'summerTemp'){ParameterTitle <- 'Summer Temperature'}
  if(VarName == 'winterTemp'){ParameterTitle <- 'Winter Temperature'}
  if(VarName == 'inputSD'){ParameterTitle <- 'Input SD'}
  if(VarName == 'inputRange'){ParameterTitle <- 'Input Range'}
  if(VarName == 'y_sd'){ParameterTitle <- 'Prediction Uncertainty'}
  # 1B.c Set range
  #if(ParameterName == 'bias_mean'){
    #Parameter.min <- min(BNEout$bias_mean); Parameter.midneg <- min(BNEout$bias_mean)/2
    #Parameter.midpos <- max(BNEout$bias_mean)/2; Parameter.max <- max(BNEout$bias_mean)
  #}
  
  # 1B.a Set the aesthetic schemes 
  # 1B.a.i for weights' expected value
  
  if(VarName %in% set_AllInputSet(2010) | VarName == 'aqs'){
    fillScheme <- scale_fill_scico(
      direction = -1, palette = 'hawaii',
      breaks = c(0, 4, 9, 16, 28), limits = c(0, 28), trans = 'sqrt')
    colorScheme <- scale_color_scico(
      direction = -1, palette = 'hawaii',
      breaks = c(0, 4, 9, 16, 28), limits = c(0, 28), trans = 'sqrt')
  }else if(VarName == 'inputSD'){
    fillScheme <- viridis::scale_color_viridis(
      direction = -1, option = 'magma',
      breaks = c(0, 1, 2, 3, 4, 4.76),
      limits = c(0, 4.76), trans = 'sqrt')
    
    colorScheme <- viridis::scale_color_viridis(
      direction = -1, option = 'magma',
      breaks = c(0, 1, 2, 3, 4, 4.76),
      limits = c(0, 4.76), trans = 'sqrt')
  }else{
    fillScheme <- scale_fill_gradientn(colours = palTurbo())
    colorScheme <- scale_color_gradientn(colours = palTurbo())
  }
  
  # Set legend position
  if(LegYN == 'LegY'){LegPos <- 'right'}
  if(LegYN == 'LegN'){LegPos <- 'none'}
  
  # Make the plot
  p <- ggplot(conus) + 
    geom_sf()  + 
    geom_sf(data = dta, aes(fill= activeVar, color = activeVar), size =0.25) + 
    fillScheme + colorScheme +
    guides(fill = guide_colorbar(title = ParameterTitle), 
           color = guide_colorbar(title = ParameterTitle)) + 
    ggtitle(paste0(VarName, ' for ', YYYY, '\n', Subtitle)) + 
    labs(x='', y='') +
    theme(plot.title = element_text(size = 18)) + 
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
    theme_void() + 
    theme(legend.position = LegPos) 
  
  # 3i Output plot
  print(p)
  
}

# test
#YYYY <- 2012;  InputSet <- c('AV', 'GS', 'CM', 'JS',  'CC');
# ParameterName <- 'bias_mean'; Input <- ''
# ParameterName <- 'w_mean'; Input <- 'AV'
#png('~/Desktop/test.png')

#plotSpatial_oneBNEParameter(2012, 'AVGSCMJSCC', 
 #                           'bias_mean', '', 'test')
#dev.off()
#plotSpatial_oneBNEParameter(2012, c('AV', 'GS', 'CM', 'JS',  'CC'), 
 #                           'w_mean', 'AV')
