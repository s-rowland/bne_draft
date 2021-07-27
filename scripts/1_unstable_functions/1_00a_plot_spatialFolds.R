# Identify Spatial Folds
# Prepare BNE Inputs
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Define Functions for Plotting Folds

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

plot_spatialFolds <- function(dta, Location, Description){
  # Description <- paste0('Based on N+1 Removal with a \n',threshold/1000, 'km Distance Threshold')
  # set file name 
  if(str_detect(Description, 'Hierarchial')){
    plotName <- paste0('hc_dist', threshold/1000, 'km')
  }
  if(str_detect(Description, 'Remove Neighbors')){
    plotName <- paste0('RmNeighbors_dist', threshold/1000, 'km')
  }
  if(str_detect(Description, 'Metropolitan')){
    plotName <- paste0('cbsa')
  }
  # Set coordinates for zoomed-in image
  if(Location == 'NYC'){
    xLim = c(1706631, 1852726); yLim <- c(2064159,  2215867)}
  if(Location == 'LA'){
    xLim = c(-2223007, -1847148); yLim <- c(1308677, 1755880)}
  
  # Decide whether we want zoom aesthetics nor not
  if(Location == 'CONUS' | str_detect(Location, 'Region')){
    GP <- theme(axis.text = element_blank(), 
                axis.ticks = element_blank(), 
                #panel_grid_major = element_blank(), 
                #panel_border = element_blank(), 
                panel.background = element_blank())
    }else{GP <- lims(x = xLim,  y = yLim) }
  
  # Set regions to restrict to 
  if(str_detect(Location, 'Region')){
    epaRegionSelect <- states %>% filter(region %in% Location)
  }else{epaRegionSelect <- states}
  
  aqsFoldsOutRegion <- dta %>% 
    st_intersection(epaRegionSelect, ., join = st_intersects) 

  schema <- data.frame(Col = c(watlington()[3], watlington()[5], polychrome()[[18]], 
              watlington()[7], watlington()[9:13], watlington()[15]), 
              fold = c('fold01', 'fold02','fold03','fold04','fold05',
                       'fold06', 'fold07','fold08','fold09','fold10'))
  schema <- schema %>% 
    filter(fold %in% aqsFoldsOutRegion$fold)
  #schema <- aqsFoldsOutRegion %>% 
   # dplyr::select(fold) %>% 
    #distinct() %>% 
    #inner_join(schema)
  
  
  TP <- ggplot(epaRegionSelect) + 
    geom_sf(fill = 'white')  + 
    geom_sf(data = aqsFoldsOutRegion, aes(fill= fold, color = fold)) + 
    scale_color_manual(values = schema$Col) + 
    scale_fill_manual(values = schema$Col) + 
    ggtitle(paste0('Leave-out Folds for Spatial Cross Validation \n', 
                   Description)) + 
    labs(x='', y='') + 
    theme(plot.title = element_text(size = 18), 
          legend.key.size = unit(1, 'cm'), 
          legend.title = element_text(size = 14), 
          legend.text = element_text(size = 12), 
          legend.key = element_rect(fill = alpha("white", 0.0))) +  
    guides(color = guide_legend(override.aes = list(size = 6)) ) +#,
           #fill = guide_legend(override.aes = list(size = 6))) +
          
    #theme(plot.title = element_text(size = 18), 
     #     plot.margin = unit(c(0.1, 0.5, 0.1, 0.5), units = 'cm')) + 
    #theme(margin(t = 0.5, b = 0.5, l = 0.5, r = 1, unit = 'cm')) + 
    GP
  
  # Print plot ina  png
  png(here::here('BNE_Inputs', 'd_01_CV_folds_plots', 
                 paste0(plotName, '_', Location, '.png')), 
      height = 440, width = 600)
  print(TP)
  dev.off()
}
