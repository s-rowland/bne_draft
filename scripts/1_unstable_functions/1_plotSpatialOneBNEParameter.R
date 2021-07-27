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
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_setUp_env.R"))
}

####*****************
#### 1: Function ####
####*****************

plotSpatialOneBNEParameter <- function(BNEoutput, runID, parameterName,
                                        input, subtitle, legYN){

# 1a Begin function
  # BNEoutput <- BNEoutputs
  #YYYY <- 2010;  inputSet <- 'AVGSCMJSCC';
  #kScale <- 3.5; activeFold <- 'all'
  # runID <- paste0(YYYY, '_', paste(inputSet, collapse =''), '_', kScale, '_', activeFold)
  #parameterName <- 'bias_mean'; input <- ''
  #parameterName <- 'pred_sd'; input <- ''; subtitle <- ''; legYN <- 'LegY'
  # 
  
  ####**********************
  #### 1A: Wrangle Data ####
  ####**********************
  
  # 1A.b Set columns Names 
  if(str_sub(parameterName, 0, 1) == 'w'){
    parameterName2 <- paste0(parameterName, '_')
  }else{
    parameterName2 <- parameterName
    }
  
  # 1A.d Rename the parameter of interest 
  VarName <- paste0(parameterName2, input)
  BNEoutput <- BNEoutput %>% 
    rename(activeVar := !!VarName)
  
  # curate outputs
  dta <- BNEoutput %>% 
    filter(run_id == !!runID)
    
  # 1A.e Convert to  simple feature
  dta <- st_as_sf(dta, coords = c("lon", "lat"), 
                      crs=st_crs("epsg:4326"))
  
  # 1A.f Transform geographical coordinates to Albers Projection
  dta <- st_transform(dta, crs=st_crs(projString))
  
  # 1A.g Keep only observations within conus
  if(!exists("conus")){
    conus <- st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                                'conus.shp'))}
  #dta <- dta %>% 
   # st_join(conus, st_intersects) %>% 
    #filter(!is.na(g))
  
  ####************************
  #### 1B: Plot Parameter ####
  ####************************
  
  # 1B.a Create ParameterTitle 
  if(parameterName == 'w_mean'){
    ParameterTitle <- 'Weight'
    PlotTitle <- paste0('Weight of ', input)
  }
  if(parameterName == 'w_sd'){ParameterTitle <- paste0('SD of ', input, ' Weight')}
  if(parameterName == 'bias_mean'){ParameterTitle <- 'Mean of Bias Term'}
  if(parameterName == 'bias_sd'){
    ParameterTitle <- 'SD of Bias Term'; PlotTitle <- 'SD of Bias Term'}
  if(parameterName == 'pred_mean'){
    ParameterTitle <- expression(atop(atop(' ', 'Predicted'),
                                 atop('PM'[2.5], '('*mu*g/m^3*')')))
    PlotTitle <- expression('Predicted'~'Concentration'~'of'~'PM'[2.5])
        }
    #ParameterTitle <- TeX(r'(PM_{2.5} Mean ($\mu g/ m^3)$)')}
  if(parameterName == 'pred_sd'){
    ParameterTitle <- expression(atop(atop(' ', 'PM'[2.5]),
                                      atop('SD', '('*mu*g/m^3*')')))
    PlotTitle <- expression('SD'~'of'~'PM'[2.5]~'Predictions')
    }
  if(parameterName == 'HighWI'){
    ParameterTitle <- 'Most-Weighted'
    PlotTitle <- 'Highest-Weighted inputs'
    }
  
  # 1B.c Set range
  if(parameterName == 'bias_mean'){
    Parameter.min <- min(BNEoutput$activeVar); Parameter.midneg <- min(BNEoutput$activeVar)/2
    Parameter.midpos <- max(BNEoutput$activeVar)/2; Parameter.max <- max(BNEoutput$activeVar)
    PlotTitle <- 'Mean of Spatial Offset'
  } 
  
  # 1B.a Set the aesthetic schemes 
  # 1B.a.i for weights' expected value
  if(parameterName == 'w_mean' | parameterName == 'w_sd' ){
    fillScheme <- viridis::scale_fill_viridis(
      direction = -1, 
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      limits = c(0, 1))
    colorScheme <- viridis::scale_color_viridis(
      direction = -1, 
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      limits = c(0, 1))
  } else if(parameterName == 'bias_mean'){
    fillScheme <- scale_fill_gradient2(
      low = "red", mid = "white",high = "blue", midpoint = 0, 
      breaks = c(round(Parameter.min, 2), round(Parameter.midneg,2),0,
                 round(Parameter.midpos,2), round(Parameter.max,2)),
      limits = c(Parameter.min, Parameter.max)) 
    colorScheme <- scale_color_gradient2(
      low = "red", mid = "white", high = "blue", midpoint = 0, 
      breaks = c(round(Parameter.min, 2), round(Parameter.midneg,2),0,
                 round(Parameter.midpos,2), round(Parameter.max,2)),   
      limits = c(Parameter.min, Parameter.max)) 
  }  else if(parameterName == 'HighWI'){
    cbp1 <- c("#E69F00", "#56B4E9", "#009E73",
              "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    fillScheme <- scale_fill_manual(values=cbp1) 
    colorScheme <- scale_color_manual(values=cbp1) 
  } else if(parameterName == 'pred_mean'){
    fillScheme <- scale_fill_scico(
      direction = -1, palette = 'hawaii',
      breaks = c(0, round(max(BNEoutput$activeVar)*0.25,2), 
                 round(max(BNEoutput$activeVar)*0.5,2), 
                 round(max(BNEoutput$activeVar)*0.75,2), 
                 round(max(BNEoutput$activeVar),2)),
      limits = c(0, round(max(BNEoutput$activeVar),2)))
    colorScheme <- scale_color_scico(
      direction = -1, palette = 'hawaii',
      breaks = c(0, round(max(BNEoutput$activeVar)*0.25,2), 
                 round(max(BNEoutput$activeVar)*0.5,2), 
                 round(max(BNEoutput$activeVar)*0.75,2), 
                 round(max(BNEoutput$activeVar),2)),
      limits = c(0, round(max(BNEoutput$activeVar),2)))
    }  else {
    fillScheme <- viridis::scale_fill_viridis(
      direction = -1, option = 'magma',
      breaks = c(0, round(max(BNEoutput$activeVar)*0.25,2), 
                 round(max(BNEoutput$activeVar)*0.5,2), 
                 round(max(BNEoutput$activeVar)*0.75,2), 
                 round(max(BNEoutput$activeVar),2)),
    limits = c(0, round(max(BNEoutput$activeVar),2)))
    colorScheme <- viridis::scale_color_viridis(
      direction = -1, option = 'magma',
      breaks = c(0, round(max(BNEoutput$activeVar)*0.25,2), 
                 round(max(BNEoutput$activeVar)*0.5,2), 
                 round(max(BNEoutput$activeVar)*0.75,2), 
                 round(max(BNEoutput$activeVar),2)),
      limits = c(0, round(max(BNEoutput$activeVar),2)))
  } 
  
  # Set legend position
  if(legYN == 'LegY'){legPos <- 'right'}
  #if(legYN == 'LegY'){legPos <- c(0.94, 0.4)}
  if(legYN == 'LegN'){legPos <- 'none'}
  
  # Make the plot
  #ggplot(conus)
  p <- ggplot() + 
    geom_sf(fill = NA)  + 
    geom_sf(data = dta, aes(fill= activeVar, color = activeVar), size = 0.9) + 
    fillScheme + colorScheme +
    guides(fill = guide_colorbar(title = ParameterTitle), 
           color = guide_colorbar(title = ParameterTitle)) + 
    ggtitle(PlotTitle) + 
    labs(x='', y='') +
    theme(plot.title = element_text(size = 18)) + 
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
    theme_void() + 
    theme(#legend.position = legPos, 
          legend.title  = element_text(size = 15)) 
  
  # 3i Output plot
  print(p)
}  


# test
#YYYY <- 2012;  inputSet <- c('AV', 'GS', 'CM', 'JS',  'CC');
# parameterName <- 'bias_mean'; input <- ''
# parameterName <- 'w_mean'; input <- 'AV'
#png('~/Desktop/test.png')

#plotSpatial_oneBNEParameter(2012, 'AVGSCMJSCC', 
 #                           'bias_mean', '', 'test')
#dev.off()
#plotSpatial_oneBNEParameter(2012, c('AV', 'GS', 'CM', 'JS',  'CC'), 
 #                           'w_mean', 'AV')
