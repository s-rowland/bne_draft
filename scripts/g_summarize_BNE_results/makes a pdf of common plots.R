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
  source(here::here('scripts', 'a_set_up', "a_00_setUp_env.R"))
}

# 0b Readin Conus
conus <- st_read(here::here('data_ancillary', 'formatted', 'spatial_outlines', 
                            'conus.shp'))

####****************************
#### 1: Wrangle BNE Outputs ####
####****************************

#1a First, declare the parameters of the BNe run that you are interested in 
YYYY <- 2010 
kScale <- 3.5
activeFold <- 'all' # the 'all' fold is the complete prediction dataset
InputSet <- c('AV', 'GS', 'CM', 'JS', 'CC')
# note that current
cat(InputSet, sep = '')
format(InputSet, sep = '')

# 1b First, readin the BNE outputs 
ColNames <- c('lat', 'lon',paste0('w_mean', '_', InputSet),
              paste0('w_sd', '_', InputSet), 'bias_mean', 'bias_sd', 
              'pred_mean', 'pred_sd', 'pred_05CI', 'pred_95CI', 
              'pred_min', 'pred_max', 'pred_median')


# 1c Readin the output of interest
RunID <- paste0(YYYY, '_', 'AVGSCMJSCC', '_', kScale, '_', activeFold)

output <- read_csv(here::here('BNE_Outputs/annual',
                              paste0(RunID, '.csv')), 
                   col_names = ColNames) %>%
  mutate(RunID = RunID, fold = activeFold)



png(here::here('BNE_plots', 'summaries',
               paste0(RunID, '_summary.pdf')))

# weights
cowplot::plot_grid(
  plotSpatial_oneBNEParameter(RunID, 'w_mean','AV', '', 'LegN'),
  plotSpatial_oneBNEParameter(RunID, 'w_mean','GS', '', 'LegN'),
  plotSpatial_oneBNEParameter(RunID, 'w_mean','CM', '', 'LegN'),
  plotSpatial_oneBNEParameter(RunID, 'w_mean','JS', '', 'LegN'),
  plotSpatial_oneBNEParameter(RunID, 'w_mean','CC', '', 'LegY'),
  nrow = 3)

# bias distribution 
plotSpatial_oneBNEParameter(RunID, 'bias_mean','', '', 'LegY')

# bias uncertainty
plotSpatial_oneBNEParameter(RunID, 'bias_sd','', '', 'LegY')

# pred distribution 
plotSpatial_oneBNEParameter(RunID, 'pred_mean','', '', 'LegY')

# pred uncertainty
plotSpatial_oneBNEParameter(RunID, 'pred_sd','', '', 'LegY')


dev.off()



