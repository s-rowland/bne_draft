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

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.rtf")
  source(here::here('scripts', 'a_set_up', "a_00_setUp_env.R"))
}

####*****************
#### 1: Function ####
####*****************

# 1a Begin function
plotHist_oneParameter <- function(dta, VarName){

  # VarName <-'AV'; Subtitle <- 'test'

  ####**********************
  #### 1A: Wrangle Data ####
  ####**********************
    
  # 1A.a Rename the parameter of interest 
  dta <- dta %>% 
    rename(activeVar := !!VarName)
  
  ####************************
  #### 1B: Plot Parameter ####
  ####************************
  
  # 1B.a Create ParameterTitle 
  #if(ParameterName == 'w_mean'){ParameterTitle <- paste0('Mean of ', Input, ' Weight')}
 # if(ParameterName == 'w_std'){ParameterTitle <- paste0('SD of ', Input, ' Weight')}
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
  
  
  # 1B.c Set range
  #if(ParameterName == 'bias_mean'){
    #Parameter.min <- min(BNEout$bias_mean); Parameter.midneg <- min(BNEout$bias_mean)/2
    #Parameter.midpos <- max(BNEout$bias_mean)/2; Parameter.max <- max(BNEout$bias_mean)
  #}
  
  # 1B.a Set the aesthetic schemes 
  # 1B.a.i for weights' expected value
  

  # Set legend position

  
  # Make the plot
  p <- ggplot(dta) +
    geom_histogram(aes(activeVar) ) + 
    ggtitle(paste0('Distribution of ',ParameterTitle)) + 
    labs(x = ParameterTitle, y = 'Count')
    theme(plot.title = element_text(size = 18)) + 
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
  
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
