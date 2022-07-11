# Identify Spatial Folds
# Prepare BNE Inputs
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Define Functions for Plotting Folds
# 2: Read Training Data
# 3: Set Threshold
# 4: Create Folds From Hierarchical Cluster
# 5: Create Folds from N+1 Criteria

####**************
#### N: Notes ####
####**************


# This need to get totally re-written
# Robbie: OK so won't look at this for now?

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_set_up_env.R"))
}

####********************************************
#### 4: Define Functions for Plotting Folds ####
####********************************************

plot_folds <- function(dta, Location, Description){
  
  # dta <- aqs; Location <- 'NYC'; Description = 'fun'
  # Description <- paste0('Based on N+1 Removal with a \n',threshold/1000, 'km Distance Threshold')
  # set file name 
    plotName <- paste0(Description, ' ', Location)
    
    
    # 1b. Convert to  simple feature
    dta <- dta %>% 
      sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326")) %>% 
      sf::st_transform(crs=sf::st_crs(projString))
  
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
  png(here::here('BNE_inputs', 'CV_folds_plots', 
                 paste0(plotName, '_', Location, '.png')), 
      height = 440, width = 600)
  print(TP)
  dev.off()
}


plot_roles <- function(foldNum, Location){
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
    theme_void()  + 
    theme(legend.key.size = unit(0.2, "cm"), 
          legend.text = element_text(size = 3))
}

####***************************
#### 2: Read Training Data ####
####***************************

# 2a Pick Year
YYYY <- 2010

activeFold <- 'fold01'

# 2b Readin training dataset

aqs <- readr::read_csv(here::here('BNE_inputs', 'prediction_datasets', 'individual_annual', 
                                  paste0('predictions_avgscmjscc_',  YYYY, '_', activeFold, '.csv'))) %>% 
  #bind_rows(readr::read_csv(here::here('BNE_inputs', 'prediction_datasets', 'individual_annual', 
   #                                    paste0('predictions_avgscmjscc_',  YYYY, '_', activeFold, '.csv')))) %>%
  dplyr::filter(year == YYYY) %>% 
  mutate(fold = activeFold)

for (f in 2:10){
  activeFold <- paste0('fold', str_pad(f, 2, 'left', 0))
  aqs <- readr::read_csv(here::here('BNE_inputs', 'prediction_datasets', 'individual_annual', 
                                    paste0('predictions_avgscmjscc_',  YYYY, '_', activeFold, '.csv'))) %>% 
    #bind_rows(readr::read_csv(here::here('BNE_inputs', 'prediction_datasets', 'individual_annual', 
    #                                    paste0('predictions_avgscmjscc_',  YYYY, '_', activeFold, '.csv')))) %>%
    dplyr::filter(year == YYYY) %>% 
    mutate(fold = activeFold) %>% 
    bind_rows(aqs)
}

# 2c Create aqs dataset 
aqs <- aqs %>% 
  dplyr::select(obs_pm2_5, ref_id, lat, lon, fold)

# 2d Convert to simple features
# 2d.i keep df version 
aqs.df <- aqs
aqs <- aqs %>% 
  st_as_sf(., coords = c("lon", "lat"), crs=st_crs('epsg:4326')) %>% 
  st_transform(., crs=st_crs(projString))

# 2e Add EPA Regions
# 2e.i Readin EPA region table
epaRegion <- read_csv(here::here('data_ancillary', 'generated', 'epa_regions.csv'))
# 2e.ii Readin state spatial data
states <- read_sf(here::here('data_ancillary', 'raw', 'Census','cb_2015_us_state_500k', 
                             'cb_2015_us_state_500k.shp')) %>% 
  st_transform(., crs=st_crs(projString))
# 2e.iii Calculate area 
states <- states %>% 
  mutate(area = ALAND + AWATER) %>% 
  filter(!(STUSPS%in%c('HI', 'AK', 'VI')))
# 2e.iv Combine 
states <- states %>% 
  rename(state = STUSPS) %>%
  dplyr::select(-AFFGEOID, -ALAND, -AWATER, -GEOID, -LSAD, -NAME, 
                -STATEFP, -STATENS) %>% 
  inner_join(epaRegion, by = 'state')
# 2e.v Join
aqs <- st_intersection(states, aqs, join = st_intersects)

# 2f Clean up 
rm(epaRegion)



plot_folds(aqs,'CONUS',
           paste0( 'test'))

plot_folds(aqs,'NYC',
           paste0( 'test'))


# another test 

# just check with st_nn and make sure the nearest distance is under 20 
aqs.train <- readr::read_csv(here::here('BNE_inputs', 'training_datasets', 'individual_annual', 
                                  paste0('training_avgscmjscc_',  YYYY, '_', activeFold, '.csv')))  %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326")) %>% 
  sf::st_transform(crs=sf::st_crs(projString))

aqs.test <- readr::read_csv(here::here('BNE_inputs', 'prediction_datasets', 'individual_annual', 
                                      paste0('predictions_avgscmjscc_',  YYYY, '_', activeFold, '.csv'))) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326")) %>% 
  sf::st_transform(crs=sf::st_crs(projString))

a <- st_nn(aqs.test, aqs.train, 1, returnDist = TRUE)$dist
  
min(unlist(a))

# okay, the spatial folds are okay, its just that 10km is very small so you basically can't really see it when you look at conus
####**********************
#### 3: Set Threshold ####
####**********************

# 3a Set Threshold 
# in km 
threshold <- 10000

# Plot - big plot 
aqsFoldsOut <- aqsFolds.rmN %>% 
  filter(role == 'Test') 

plot_folds(aqsFoldsOut,'CONUS',
           paste0('Based on Remove Neighbors with a \n',threshold/1000, 'km Distance Threshold'))
plot_folds(aqsFoldsOut, 'Region02',
           paste0('Based on Remove Neighbors with a \n',threshold/1000, 'km Distance Threshold'))
plot_folds(aqsFoldsOut, 'NYC',
           paste0('Based on Remove Neighbors with a \n',threshold/1000, 'km Distance Threshold'))
plot_folds(aqsFoldsOut,'Region09',
           paste0('Based on Remove Neighbors with a \n',threshold/1000, 'km Distance Threshold'))
plot_folds(aqsFoldsOut,'LA',
           paste0('Based on Remove Neighbors with a \n',threshold/1000, 'km Distance Threshold'))

# Plot the LeaveOut vs test set for NYC 
png(here::here('BNE_inputs', 'CV_folds_plots', 
               paste0('RmNeighbors', '_', 'NYC_', 'roles_folds1to4.png')), 
    height = 880, width = 1200)
cowplot::plot_grid(
  plot_roles(1, 'NYC'), plot_roles(2, 'NYC'),
plot_roles(3, 'NYC'), plot_roles(4, 'NYC'), 
  nrow = 2)
dev.off()

png(here::here('BNE_inputs', 'CV_folds_plots', 
               paste0('RmNeighbors', '_', 'NYC_', 'roles_folds5to8.png')), 
    height = 880, width = 1200)
cowplot::plot_grid(
  plot_roles(5, 'NYC'), plot_roles(6, 'NYC'),
  plot_roles(7, 'NYC'), plot_roles(8, 'NYC'), 
  nrow = 2)
dev.off()

png(here::here('BNE_inputs', 'CV_folds_plots', 
               paste0('RmNeighbors', '_', 'LA_', 'roles_folds1to4.png')), 
    height = 880, width = 1200)
cowplot::plot_grid(
  plot_roles(1, 'LA'), plot_roles(2, 'LA'),
  plot_roles(3, 'LA'), plot_roles(4, 'LA'), 
  nrow = 2)
dev.off()

png(here::here('BNE_inputs', 'CV_folds_plots', 
               paste0('RmNeighbors', '_', 'LA_', 'roles_folds5to8.png')), 
    height = 880, width = 1200)
cowplot::plot_grid(
  plot_roles(5, 'LA'), plot_roles(6, 'LA'),
  plot_roles(7, 'LA'), plot_roles(8, 'LA'), 
  nrow = 2)
dev.off()


#### Save foldes 

a <- aqs %>% 
  mutate(foldMethod = 'all', fold = 'all', role = 'Train')  %>% 
  bind_rows(aqsFolds.hc) %>% 
  bind_rows(aqsFolds.rmN) %>% 
  dplyr::select(geometry, state, region, aqs, ID, foldMethod, fold, role) %>% 
  st_transform(., crs=st_crs('epsg:4326'))

b <- a %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)
b$lon <- st_coordinates(a)[,1]
b$lat <- st_coordinates(a)[,2]

dgk <- Train.df %>% 
  dplyr::select(-aqs, -lat, -lon) %>%
  inner_join(b, by = c('ID'))

dgk %>% write_csv(here::here('BNE_inputs', 'CV_folds_plots', paste0('folds_', YYYY)))
  
ab <- dgk %>% group_by(foldMethod, fold, role) %>% summarize(count =n())
