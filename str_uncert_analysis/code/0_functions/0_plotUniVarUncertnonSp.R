# Combine BNE Outputs
# Uncertainty Analysis for ISEE
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Define Function to Read BNE Outputs from Specific Folder
# 2: Combine BNE Outputs

####********************
#### 0: Preparation ####
####********************

####*****************************************************************
#### 1: Define Function to Read BNE Outputs from Specific Folder ####
####*****************************************************************

# 1a Begin function
plotUniVarUncertnonSp <- function(bneOut, expVar){
  #expVar <- 'pred_mean'
  
  # fit model
  activeFormula <- as.formula(paste0('pred_sd ~ s(', expVar, ')'))
  mod.uniV <- gam(activeFormula, data = bneOut)
  
  # determine axis names 
  if(expVar == 'pred_mean'){
    activeXLab <- expression('Predicted PM'[2.5]~'('*mu*g/m^3*')')
    activeMain <- expression('Predicted PM'[2.5])
  } else if(expVar == 'mon_dist1' | expVar == 'monDist1.mean'){
    activeXLab <- 'Distance to Nearest Monitor (km)'
    activeMain <- 'Distance to Nearest Monitor'
  } else if(expVar == 'winter_temp'){
    activeXLab <- 'Winter Mean Temperature (C)'
    activeMain <- 'Winter Temperature'
  } else if(expVar == 'summer_temp'){
    activeXLab <- 'Summer Mean Temperature (C)'
    activeMain <- 'Summer Temperature'
  }else if(expVar == 'cloud_cover'){
    activeXLab <- 'Cloud Cover (%)'
    activeMain <- 'Cloud Cover'
  }else if(expVar == 'elev'){
    activeXLab <- 'Elevation (M)'
    activeMain <- 'Elevation'
  }
  
  
 plot(mod.uniV, rug = TRUE, select = 1, 
       xlab = activeXLab, 
       ylab = expression('Predictive Uncertainty ('*mu*g/m^3*')'), 
       main = activeMain, 
       cex.lab = 1.4, cex.axis = 1.4, cex.main = 1.4)
  #return(ap)
}
