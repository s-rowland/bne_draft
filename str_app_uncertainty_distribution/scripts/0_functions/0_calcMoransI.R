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
calcMoransI <- function(varVec, coordMatrix){
  # 1b Create distance matrix
  dta.dists <- as.matrix(dist(coordMatrix))
  # 1c Compute the inverse of the distance
  dta.dists.inv <- 1/dta.dists
  # 1d Make all of the diagonals (self-distance) zero
  diag(dta.dists.inv) <- 0
  # 1e Calculate Moran's I 
  Moran.I(varVec, dta.dists.inv)
}
