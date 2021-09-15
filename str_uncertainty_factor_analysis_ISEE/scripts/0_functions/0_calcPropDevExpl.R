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

####***********************************
#### 1: Load Packages for Analysis ####
####***********************************
calcPropDevExpl <- function(modPartial, modFull){
  (deviance(modPartial)-deviance(modFull))/deviance(modPartial)
}