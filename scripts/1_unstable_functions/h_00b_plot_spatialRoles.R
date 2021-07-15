# Identify Spatial Folds
# Prepare BNE Inputs
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Define Functions for Plotting Roles

####**************
#### N: Notes ####
####**************


####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.rtf")
  source(here::here('scripts', 'a_set_up', "a_00_setUp_env.R"))
}

####********************************************
#### 1: Define Functions for Plotting Folds ####
####********************************************

plot_spatialRoles <- function(foldNum, Location){
  # foldNum <- 1; Location <- 'NYC'
  activeFold <- paste0('fold', str_pad(foldNum, 2, 'left', '0'))
  aqsFoldsOutSubset <- aqsFolds.rmN %>% 
    filter(fold == activeFold) 
  
  if(Location == 'NYC'){
    xLim = c(1786631, 1862726); yLim <- c(2134159,  2235867)}
  if(Location == 'LA'){
    xLim = c(-2223007, -1847148); yLim <- c(1308677, 1705880)}
  
  ggplot(states) + 
    geom_sf(fill = 'white')  + 
    geom_sf(data = aqsFoldsOutSubset, aes(fill= role, color = role)) + 
    ggtitle(paste0('Leave-out Folds for Spatial Cross Validation \n', 
                   'Based on ',threshold, 'm Distance Threshold and N+1 Exclusion', 
                   'Fold: ', foldNum)) + 
    labs(x='', y='') +
    lims(x = xLim, y = yLim) + 
    scale_color_manual(values = TurboPalette(4)[1:3]) + 
    scale_fill_manual(values = TurboPalette(4)[1:3]) + 
    theme(plot.title = element_text(size = 18)) + 
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
    theme_void() 
}
