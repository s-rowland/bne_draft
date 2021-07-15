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

plotSpatial_oneBNEParameter <- function(YYYY, InputStr, ScaleK, ParameterName,
                                        Input, Subtitle, LegYN){

# 1a Begin function
  #YYYY <- 2010;  InputStr <- 'AVGSCMJSCC';
   #ScaleK <- 3.5
  # ParameterName <- 'bias_mean'; InputStr <- ''
   #ParameterName <- 'pred_sd'; Input <- ''; Subtitle <- ''; LegYN <- 'LegY'
  # 
  
  ####**********************
  #### 1A: Wrangle Data ####
  ####**********************
  
  # 1A.b Set columns Names 
  if(str_sub(ParameterName, 0, 1) == 'w'){
    ParameterName2 <- paste0(ParameterName, '_')
  }else{
    ParameterName2 <- ParameterName
    }
  
  # 1A.d Rename the parameter of interest 
  VarName <- paste0(ParameterName2, Input)
  BNEout <- BNEout %>% 
    rename(activeVar := !!VarName)
  
  # curate outputs
  dta <- BNEout %>% 
    filter(RunID == !!paste0(YYYY, '_', InputStr, '_', ScaleK, '_all_all'))
    
  # 1A.e Convert to  simple feature
  dta <- st_as_sf(dta, coords = c("lon", "lat"), 
                      crs=st_crs("epsg:4326"))
  
  # 1A.f Transform geographical coordinates to Albers Projection
  dta <- st_transform(dta, crs=st_crs(projString))
  
  # 1A.g Keep only observations within conus
  if(!exists("conus")){
    conus <- st_read(here::here('Data', 'general', 'spatial_outlines', 'conus.shp'),
                    crs = projString)}
  #dta <- dta %>% 
   # st_join(conus, st_intersects) %>% 
    #filter(!is.na(g))
  
  ####************************
  #### 1B: Plot Parameter ####
  ####************************
  
  # 1B.a Create ParameterTitle 
  if(ParameterName == 'w_mean'){
    ParameterTitle <- paste0('Mean of ', Input, ' Weight')
    PlotTitle <- paste0('Weight of ', Input)
  }
  if(ParameterName == 'w_sd'){ParameterTitle <- paste0('SD of ', Input, ' Weight')}
  if(ParameterName == 'bias_mean'){ParameterTitle <- 'Mean of Bias Term'}
  if(ParameterName == 'bias_sd'){
    ParameterTitle <- 'SD of Bias Term'; PlotTitle <- 'SD of Bias Term'}
  if(ParameterName == 'pred_mean'){
    ParameterTitle <- expression(atop(atop(' ', 'Predicted'),
                                 atop('PM'[2.5], '('*mu*g/m^3*')')))
    PlotTitle <- expression('Predicted'~'Concentration'~'of'~'PM'[2.5])
        }
    #ParameterTitle <- TeX(r'(PM_{2.5} Mean ($\mu g/ m^3)$)')}
  if(ParameterName == 'pred_sd'){
    ParameterTitle <- expression(atop(atop(' ', 'PM'[2.5]),
                                      atop('SD', '('*mu*g/m^3*')')))
    PlotTitle <- expression('SD'~'of'~'PM'[2.5]~'Predictions')
    }
  if(ParameterName == 'HighWI'){
    ParameterTitle <- 'Most-Weighted'
    PlotTitle <- 'Highest-Weighted Inputs'
    }
  
  # 1B.c Set range
  if(ParameterName == 'bias_mean'){
    Parameter.min <- min(BNEout$activeVar); Parameter.midneg <- min(BNEout$activeVar)/2
    Parameter.midpos <- max(BNEout$activeVar)/2; Parameter.max <- max(BNEout$activeVar)
  } 
  
  # 1B.a Set the aesthetic schemes 
  # 1B.a.i for weights' expected value
  if(ParameterName == 'w_mean' | ParameterName == 'w_sd' ){
    fillScheme <- viridis::scale_fill_viridis(
      direction = -1, 
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      limits = c(0, 1))
    colorScheme <- viridis::scale_color_viridis(
      direction = -1, 
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      limits = c(0, 1))
  } else if(ParameterName == 'bias_mean'){
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
  }  else if(ParameterName == 'HighWI'){
    cbp1 <- c("#E69F00", "#56B4E9", "#009E73",
              "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    fillScheme <- scale_fill_manual(values=cbp1) 
    colorScheme <- scale_color_manual(values=cbp1) 
  } else if(ParameterName == 'pred_mean'){
    fillScheme <- scale_fill_scico(
      direction = -1, palette = 'hawaii',
      breaks = c(0, round(max(BNEout$activeVar)*0.25,2), 
                 round(max(BNEout$activeVar)*0.5,2), 
                 round(max(BNEout$activeVar)*0.75,2), 
                 round(max(BNEout$activeVar),2)),
      limits = c(0, round(max(BNEout$activeVar),2)))
    colorScheme <- scale_color_scico(
      direction = -1, palette = 'hawaii',
      breaks = c(0, round(max(BNEout$activeVar)*0.25,2), 
                 round(max(BNEout$activeVar)*0.5,2), 
                 round(max(BNEout$activeVar)*0.75,2), 
                 round(max(BNEout$activeVar),2)),
      limits = c(0, round(max(BNEout$activeVar),2)))
    }  else {
    fillScheme <- viridis::scale_fill_viridis(
      direction = -1, option = 'magma',
      breaks = c(0, round(max(BNEout$activeVar)*0.25,2), 
                 round(max(BNEout$activeVar)*0.5,2), 
                 round(max(BNEout$activeVar)*0.75,2), 
                 round(max(BNEout$activeVar),2)),
    limits = c(0, round(max(BNEout$activeVar),2)))
    colorScheme <- viridis::scale_color_viridis(
      direction = -1, option = 'magma',
      breaks = c(0, round(max(BNEout$activeVar)*0.25,2), 
                 round(max(BNEout$activeVar)*0.5,2), 
                 round(max(BNEout$activeVar)*0.75,2), 
                 round(max(BNEout$activeVar),2)),
      limits = c(0, round(max(BNEout$activeVar),2)))
  } 
  
  # Set legend position
  #if(LegYN == 'LegY'){LegPos <- 'right'}
  if(LegYN == 'LegY'){LegPos <- c(0.94, 0.4)}
  if(LegYN == 'LegN'){LegPos <- 'none'}
  
  # Make the plot
  #ggplot(conus)
  p <- ggplot() + 
    geom_sf(fill = NA)  + 
    geom_sf(data = dta, aes(fill= activeVar, color = activeVar), size = 0.5) + 
    fillScheme + colorScheme +
    guides(fill = guide_colorbar(title = ParameterTitle), 
           color = guide_colorbar(title = ParameterTitle)) + 
    ggtitle(PlotTitle) + 
    labs(x='', y='') +
    theme(plot.title = element_text(size = 18)) + 
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
    theme_void() + 
    theme(legend.position = LegPos, 
          legend.title  = element_text(size = 15)) 
  
  # 3i Output plot
  print(p)
}  


# test
#YYYY <- 2012;  InputSet <- c('AV', 'GS', 'CM', 'JS',  'CC');
# ParameterName <- 'bias_mean'; Input <- ''
# ParameterName <- 'w_mean'; Input <- 'AV'
#png('~/Desktop/test.png')

#plotSpatial_oneBNEParameter(2012, 'AVGSCMJSCC', 
 #                           'bias_mean', '', 'test')
#dev.off()
#plotSpatial_oneBNEParameter(2012, c('AV', 'GS', 'CM', 'JS',  'CC'), 
 #                           'w_mean', 'AV')
