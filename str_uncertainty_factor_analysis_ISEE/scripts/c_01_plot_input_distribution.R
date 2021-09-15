# Plot Where JS Dominates 
# Assess BNE Properties: Round 1
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Wrangle Weights 
# 2: Assess Whether JS is Strictly Dominant
# 3: Define Function to Plot Weights 
# 4: Plot Weights 

####**************
#### N: Notes ####
####**************

# Right now we are using training and avgscm prediction datasets inherited 
# from the Capstone students' project, so the code it not yet 100% reproducible

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_0_00")){
  # change this to something .md when I can 
  here::i_am("README.rtf")
  source(here::here('Scripts', '0_set_up',
                    "0_00_setUp_env.R"))
}

####****************************
#### 1: Wrangle Predictions ####
####****************************

# 1a Set Year
YYYY <- 2014

# 1b Readin predictions
dtaPred <- read_csv(here::here(
  'BNE_Inputs', 'c_03_parcels', 'annual',
  paste0('Predictions_', YYYY, '_' , paste(set_AllInputSet(YYYY), collapse = ''),
         '_1percent.csv')))

# 1c Wrangle predictions
# 1c.i Convert to simple features
dtaPred <- dtaPred %>% 
  st_as_sf(., coords = c("lon", "lat"), crs=st_crs('epsg:4326')) %>% 
  st_transform(crs=st_crs(projString))

####*************************
#### 2: Plot Predictions ####
####*************************

png(here::here('ISEE_analysis', 'outputs', 
               paste0('Input_distribution_', YYYY, '.png')))
cowplot::plot_grid(
  plotSpatial_oneParameter(dtaPred, 'AV', ' ', 'LegN'),
  plotSpatial_oneParameter(dtaPred, 'GS', ' ', 'LegN'), 
  plotSpatial_oneParameter(dtaPred, 'CM', ' ', 'LegN'),
  plotSpatial_oneParameter(dtaPred, 'JS', ' ', 'LegN'),
  plotSpatial_oneParameter(dtaPred, 'CC', ' ', 'LegN'),
  nrow = 3)
dev.off()

png(here::here('ISEE_analysis', 'outputs', 
               paste0('Input_distribution_', YYYY, 'Legend.png')))
cowplot::plot_grid(
  plotSpatial_oneParameter(dtaPred, 'AV', ' ', 'LegY'),

  nrow = 1)
dev.off()
