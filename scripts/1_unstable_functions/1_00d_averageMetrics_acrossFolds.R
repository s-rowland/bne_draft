# Join Input Models 
# Prepare BNE Inputs
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Define Function
# 2: Calculate Metrics 

####**************
#### N: Notes ####
####**************

# Right now we are using training and avgscm prediction datasets inherited 
# from the Capstone students' project, so the code it not yet 100% reproducible

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.rtf")
  source(here::here('scripts', 'a_set_up', "a_00_setUp_env.R"))
}

####********************************************************
#### 1: Define Function to Average Metrics Across Folds ####
####********************************************************

# 1a Name function
averageMetrics_acrossFolds <- function(YYYY, kScale){
  # YYYY <- 2010; kScale <- 3.5
  # 1b Make foldList 
  foldList <- c(paste0('fold', str_pad(1:10, 2, 'left', '0')))
  
  # 1c calculate metrics for each fold
  foldMetrics <- pmap(list(rep(YYYY, 10), rep(kScale, 10), foldList), calculateMetrics_oneFold) %>% 
    bind_rows()
  
  # 1d Average the metrics
  metricList <- list(MAE = mean(foldMetrics$E), 
                     RMSE = sqrt(mean(foldMetrics$SE)), 
                     corr = mean(foldMetrics$corr), 
                     slope = mean(foldMetrics$slope), 
                     Rsq = mean(foldMetrics$Rsq), 
                     cover = mean(foldMetrics$cover))
  
  # 1e Return Metrics 
  return(metricList)
}

