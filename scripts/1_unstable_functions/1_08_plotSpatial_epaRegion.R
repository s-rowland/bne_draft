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

# 2c.i Readin EPA region table

# 1a Begin function
plotSpatial_epaRegion <- function(activeRegion){

  # VarName <-'AV'; Subtitle <- 'test'

  ####**********************
  #### 1A: Wrangle Data ####
  ####**********************
  
  # 1A.b Keep only observations within conus
  if(!exists("conus")){
    conus <- st_read(here::here('Data', 'general', 'spatial_outlines', 'conus.shp'), 
                     crs = projString)
  }
  
  # 1A.c Filter by Region 
  dta <- dta %>% 
    filter(region == !!paste0('Region', activeRegion))
  
  # 1A.d Filter by CONUS
  dta <- dta %>% 
    st_join(conus, st_intersects) %>% 
    filter(!is.na(g))
  
  ####************************
  #### 1B: Plot Parameter ####
  ####************************
  
  colorSet <- rainbow(10)
  
  # Make the plot
  p <- ggplot(conus) + 
    geom_sf()  + 
    geom_sf(data = dta, fill = colorSet[as.numeric(activeRegion)], 
            col = colorSet[as.numeric(activeRegion)])  + 
    ggtitle(paste0('EPA Region ', activeRegion)) + 
    labs(x='', y='') +
    theme(plot.title = element_text(size = 18)) + 
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
    theme_void() + 
    theme(legend.position = 'none') 
  
  # 3i Output plot
  png(here::here('Results_Outputs', 'epaRegion_maps', 
                 paste0('Epa_region', activeRegion, '.png')))
  print(p)
  dev.off()
  
}


