# Project Set up 
# BNE Fast Uncertainty Analysis for ISEE
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Load Packages for Analysis

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.rtf")
  source(here::here('scripts', 'a_set_up', "a_00_setUp_env.R"))
}

####***********************************
#### 1: Function ####
####***********************************

# 1a Begin function
set_AllInputSet <- function(YYYY){
  #YYYY <- 2010;
  # 1b Determine the appropriate inputset
  if(YYYY == 2010){
    AllInputSet <- c('AV', 'GS', 'CM', 'JS', 'CC' ) #, #'M14', 'M20','CA', 
                    # 'NN', 'BG', 'RD')
  } else {
    AllInputSet <- c('AV', 'GS', 'CM', 'JS','CC' ) #, 
                     #'NN', 'BG', 'RD')
  }
  # 1c Return input set 
  return(AllInputSet)
  
}


