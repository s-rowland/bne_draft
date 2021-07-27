# Join Daily AQS and MERRA
# Assess Daily Input Models 
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: 

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_set_up_env.R"))
}

# 0b Readin Conus
conus <- st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                            'conus.shp'))

####****************************
#### 1: Wrangle BNE Outputs ####
####****************************

#1a First, declare the parameters of the BNE run that you are interested in 
YYYY <- 2010 
kScale <- 3.5
activeFold <- 'all' # the 'all' fold is the complete prediction dataset
InputSet <- c('AV', 'GS', 'CM', 'JS', 'CC')

runID <- paste0(YYYY, '_', paste(InputSet, collapse = ''), '_', kScale, '_',
                activeFold)

# 1b Read the BNE outputs
BNEoutput <- readBNEoutput(YYYY, InputSet, kScale, activeFold)

####*******************
#### 2: Make Plots ####
####*******************

pdf(here::here('BNE_plots', 'summaries',
               paste0(runID, '_summary.pdf')))

# weights
cowplot::plot_grid(
  plotSpatialOneBNEParameter(BNEoutput,runID, 'w_mean','AV', '', 'LegN'),
  plotSpatialOneBNEParameter(BNEoutput,runID, 'w_mean','GS', '', 'LegN'),
  plotSpatialOneBNEParameter(BNEoutput,runID, 'w_mean','CM', '', 'LegN'),
  plotSpatialOneBNEParameter(BNEoutput,runID, 'w_mean','JS', '', 'LegN'),
  plotSpatialOneBNEParameter(BNEoutput,runID, 'w_mean','CC', '', 'LegY'),
  nrow = 3)

# bias distribution 
plotSpatialOneBNEParameter(BNEoutput,runID, 'bias_mean','', '', 'LegY')

# bias uncertainty
plotSpatialOneBNEParameter(BNEoutput,runID, 'bias_sd','', '', 'LegY')

# pred distribution 
plotSpatialOneBNEParameter(BNEoutput,runID, 'pred_mean','', '', 'LegY')

# pred uncertainty
plotSpatialOneBNEParameter(BNEoutput, runID, 'pred_sd','', '', 'LegY')


dev.off()



