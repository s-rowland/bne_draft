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
#### 1: Load Packages for Analysis ####
####***********************************

compute_areaWeightedAverage <- function(AggregationLayer, FeatureLayer, FeatureVar){
  #AggregationLayer <- baseGrid;  FeatureLayer <- uncert.sf; FeatureVar <- 'std'

  # 1a Assign Gridid 
 # AggregationLayer <- AggregationLayer %>% 
   # mutate(gridID = row_number())
  
  # 1b Create Intersections
  # not too bad, takes less than 5 min to run. 
  intersec <- st_intersection(AggregationLayer, FeatureLayer)
  
  # 1c Calculate Area
  intersec <- intersec %>% 
    mutate(area = st_area(intersec)) %>% 
    mutate(area = as.numeric(as.character(area)))
  
  # 1d now make it not spatial, we not longer need geometry 
  intersec <- as.data.frame(intersec)
  
  # 1e fix variable names for the Feature layer, do the 
  intersec <- intersec %>% 
    rename(var = !!FeatureVar)
  
  # 1f Calculate Weighted average
  # 1f.i calculate total area
  intersec.grouped <- intersec %>%
    group_by(gridID) %>% 
    summarize(areaTot = sum(area))
  # 1f.ii add total area column to dataset
  intersec <- intersec %>% 
    inner_join(intersec.grouped, by = 'gridID') 
  # compute area-weighted average
  intersec.mean <- intersec %>% 
    group_by(gridID) %>% 
    summarize(varMean = sum(var * area / areaTot)) %>% 
      rename(!!FeatureVar := varMean)
  # return dataframe 
  intersec.mean
}

# this takes over 30 minutes when I just od it... maybe better if it is outside 
# of function 
compute_areaWeightedAverage_Raster <- function(AggregationLayer, FeatureLayer){
  AggregationLayer <- baseGrid;  FeatureLayer <- prism; 
  
  # 1b Assign Gridid 
  AggregationLayer <- AggregationLayer %>% 
    mutate(gridID = row_number())
  # extract
  intersec.mean <- raster::extract(FeatureLayer, AggregationLayer, 
                               df = TRUE, factors = TRUE, fun = mean) 

  # Add grid id's 
  intersec.mean$gridID <- AggregationLayer$gridID
    # return dataframe 
  intersec.mean
}
