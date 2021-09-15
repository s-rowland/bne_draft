# Join Daily AQS and MERRA
# Uncertainty Analysis for ISEE
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation
# 1: Wrangle BNE Outputs
# 2: Plot BNE Weights 
# 3: Plot Predicted Concentration 
# 4: Plot Predictive Uncertainty
# 5: Make Table of Mean Uncertainty by EPA Region 
# 6: Plot Time Series of Nationwide-Mean Uncertainty

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_set_up_env.R"))
}

if(!exists("Ran_a_00_ISEE")){
  here::i_am("README.md")
  source(here::here('uncertainty_factor_analysis_ISEE', 'scripts', 
                    "a_00_set_up_env_ISEE.R"))
}

# 0b Readin Conus
conus <- st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                            'conus.shp'))


#p_load(gridGraphics)
####****************************
#### 1: Wrangle BNE Outputs ####
####****************************

# 1a Read combined BNE outputs
BNEoutputs <- read_fst(here::here('uncertainty_factor_analysis_ISEE','BNE_outputs',
                           'BNEoutputs_combined.fst'))




pdf(here::here('uncertainty_factor_analysis_ISEE', 'outputs', 'EDA',
               'uncert check.pdf'))

plotSpatialOneBNEParameter(BNEoutputs, 
                           paste0(2010, '_AVGSCMJSCC_3.5_all'), 'pred_sd','', 
                           '', 'LegY')
plotSpatialOneBNEParameter(BNEoutputs, 
                           paste0(2011, '_AVGSCMJSCC_3.5_all'), 'pred_sd','', 
                           '', 'LegY')
plotSpatialOneBNEParameter(BNEoutputs, 
                           paste0(2012, '_AVGSCMJSCC_3.5_all'), 'pred_sd','', 
                           '', 'LegY')
plotSpatialOneBNEParameter(BNEoutputs, 
                           paste0(2013, '_AVGSCMJSCC_3.5_all'), 'pred_sd','', 
                           '', 'LegY')
plotSpatialOneBNEParameter(BNEoutputs, 
                           paste0(2014, '_AVGSCMJSCC_3.5_all'), 'pred_sd','', 
                           '', 'LegY')
plotSpatialOneBNEParameter(BNEoutputs, 
                           paste0(2015, '_AVGSCMJSCC_3.5_all'), 'pred_sd','', 
                           '', 'LegY')

dev.off()

####*************************
#### 2: Plot BNE Weights ####
####*************************

# 2a slide 1: weights vary over space (AV and JS) 
# 2a.i Collect the actual plots
pRow <- plot_grid(
  plotSpatialOneBNEParameter(BNEoutputs, 
                             paste0(2010, '_AVGSCMJSCC_3.5_all'), 'w_mean','AV', 
                             '', 'LegN') + theme(legend.position="none"), 
  plotSpatialOneBNEParameter(BNEoutputs, 
                             paste0(2010, '_AVGSCMJSCC_3.5_all'), 'w_mean','JS', 
                             '', 'LegN') + theme(legend.position="none"), 
  nrow = 1)
# 2a.ii Make a version of plot with legend
# and then extract the legend
legend <- get_legend(plotSpatialOneBNEParameter(BNEoutputs, 
                                                paste0(2010, '_AVGSCMJSCC_3.5_all'), 
                                                'w_mean','AV', '', 'LegN') + 
                       theme(legend.box.margin = margin(5, 5, 5, 12)))

# 2a.iii Print the plots
png(here::here('uncertainty_factor_analysis_ISEE', 'outputs', 'presentation',
               'Fig1_weightsAVGS.png'))

plot_grid(pRow, legend, rel_widths = c(3, .4))
dev.off()

# slide 2: weights vary over time (AV 2010, AV 2011)
# 2a.i Collect the actual plots
pRow <- plot_grid(
  plotSpatialOneBNEParameter(BNEoutputs, 
                             paste0(2010, '_AVGSCMJSCC_3.5_all'), 'w_mean','AV', 
                             '', 'LegN') + theme(legend.position="none"), 
  plotSpatialOneBNEParameter(BNEoutputs, 
                             paste0(2011, '_AVGSCMJSCC_3.5_all'), 'w_mean','AV', 
                             '', 'LegN') + theme(legend.position="none"), 
  nrow = 1)
# 2a.ii Make a version of plot with legend
# and then extract the legend
legend <- get_legend(plotSpatialOneBNEParameter(BNEoutputs, 
                                                paste0(2010, '_AVGSCMJSCC_3.5_all'), 
                                                'w_mean','AV', '', 'LegY') + 
                       theme(legend.box.margin = margin(5, 5, 5, 12)))

# 2a.iii Print the plots
png(here::here('uncertainty_factor_analysis_ISEE', 'outputs', 'presentation',
               'Fig2_weightsAV_20102011.png'))

plot_grid(pRow, legend, rel_widths = c(3, .4))
dev.off()


####*************************************
#### 3: Plot Predicted Concentration ####
####*************************************

# slide 3: we get predictions (pred_mean 2010)
png(here::here('uncertainty_factor_analysis_ISEE', 'outputs', 'presentation',
               'Fig3_predMean_2010.png'))
plotSpatialOneBNEParameter(BNEoutput = BNEoutputs, 
                           runID = paste0(2010, '_AVGSCMJSCC_3.5_all'), 
                           parameterName = 'pred_mean', input = '', 
                           pTitle = expression('Estimated'~'Concentration'~'of'~'PM'[2.5]~'for'~'2010'), 
                           subtitle ='', 'LegY')
dev.off()

png(here::here('uncertainty_factor_analysis_ISEE', 'outputs', 'presentation',
               'Fig1_weightAV_2010.png'))
plotSpatialOneBNEParameter(BNEoutput = BNEoutputs, 
                           runID = paste0(2010, '_AVGSCMJSCC_3.5_all'), 
                           parameterName = 'w_mean', input = 'AV', 
                           pTitle = expression('Weight'~'of'~'AV'~'for'~'2010'), 
                           subtitle ='', 'LegY')
dev.off()

####************************************
#### 4: Plot Predictive Uncertainty ####
####************************************

# slide 4: we get uncertainity (pred_mean 2010)
png(here::here('uncertainty_factor_analysis_ISEE', 'outputs', 'presentation',
               'Fig4_predUncert_2010.png'))

plotSpatialOneBNEParameter(BNEoutputs, 
                           paste0(2010, '_AVGSCMJSCC_3.5_all'), 'pred_sd','', 
                           '', 'LegY')
dev.off()


####*****************************************************
#### 5: Make Table of Mean Uncertainty by EPA Region ####
####*****************************************************

# 5a Read the BNE-factor combined dataset 

# 5b Average by EPA region 

# 5c Save table

####********************************************************
#### 6: Plot Time Series of Nationwide-Mean Uncertainty ####
####********************************************************

BNEoutputs %>% 
  group_by(YYYY) %>% 
  summarize(pred_mean = mean(pred_mean)) %>%
  ggplot(.) + 
  #geom_ribbon(aes(x = YYYY, ymin = uncert_q1, ymax = uncert_q3), 
  #           color = 'chocolate3', fill = 'chocolate3', alpha = 0.25) + 
  geom_line(aes(x=YYYY, y = pred_mean), color = 'chocolate3')  + 
  labs(x = 'Year', 
       y = expression(atop(
         'Nationwide'~'Average'~'Predicted','PM'[2.5]~'('*mu*g/m^3*')')))

# 6a Summarize 
png(here::here('uncertainty_factor_analysis_ISEE', 'outputs', 'presentation',
               'Fig7_predUncert_overTime.png'))
BNEoutputs %>% 
  group_by(YYYY) %>% 
  summarize(uncert_mean = mean(pred_sd), 
            uncert_q1 = quantile(pred_sd, 0.25), 
            uncert_q3 = quantile(pred_sd, 0.75)) %>%
  ggplot(.) + 
  #geom_ribbon(aes(x = YYYY, ymin = uncert_q1, ymax = uncert_q3), 
   #           color = 'chocolate3', fill = 'chocolate3', alpha = 0.25) + 
  geom_line(aes(x=YYYY, y = uncert_mean), color = 'mediumpurple4')  + 
  plotTheme + 
  labs(x = 'Year', 
       y = expression(atop(
         'Nationwide'~'Average'~'Predictive'~'Uncertainty','for'~'PM'[2.5]~'('*mu*g/m^3*')')))

dev.off()
  

 

####******************************
#### 7: Plot Mean Uncertainty ####
####******************************
####*
####*

readCombinedData <- function(YYYY) {
  read_fst(here::here('uncertainty_factor_analysis_ISEE', 'data', 'processed', 
                      paste0('AVGSCMJSCC_3.5_explanatory_', YYYY, '_015deg_final.fst'))) %>% 
    mutate(YYYY = YYYY)
}
dta <- map(2010:2015, readCombinedData) %>% 
  bind_rows()
rm(readCombinedData)

# 1b Convert units of population density to thousand ppl per km 
dta <- dta %>% 
  mutate(pop_density_1kper1000km2 = pop_density*1000000 / 1000) %>% 
  mutate(lat = as.numeric(as.character(lat)), 
         lon = as.numeric(as.character(lon)))

# 1d Average across years 
dta <- dta %>% 
  group_by(lat, lon) %>% 
  summarize_all(mean) %>% 
  ungroup() %>% 
  dplyr::select(-state, -region, -YYYY) %>% 
  mutate(cell_id = row_number()) 

dta.sf <- dta %>% 
  st_as_sf(coords = c('lon', 'lat'), crs=st_crs('epsg:4326')) %>% 
  st_transform(crs=st_crs(projString))
# 1e Scale all of the variables... yes! 
# Let's not scale, it makes it a bit harder to interpret the plots
#dta <- dta %>% 
# mutate_at(vars(-lat, -lon,  -stateNum), scale) 


# 2c.ii Readin state spatial data
states <- read_sf(here::here('ancillary_data', 'raw', 'Census','cb_2015_us_state_500k', 
                             'cb_2015_us_state_500k.shp'))
# 2c.iii Combine 
states <- states %>% 
  rename(state = STUSPS) %>%
  mutate(state_num = as.numeric(STATEFP)) %>% 
  dplyr::select(-AFFGEOID, -ALAND, -AWATER, -GEOID, -LSAD, -NAME, 
                -STATEFP, -STATENS)
states <- states %>% st_transform(crs=st_crs(projString))
excludedAreas <- c("Alaska", "Hawaii", "Puerto Rico", 
                   "Commonwealth of the Northern Mariana Islands", "Guam", 
                   "American Samoa", "United States Virgin Islands")
states <- states[!states%in%excludedAreas,]



states.ref <- computeIntersectionMax(dta.sf, states, 'state_num') 
states.ref <- states.ref %>% 
  mutate(state_um = round(state_num, 0)) 

states.ref <- states.ref %>% 
  inner_join(states, by ='state_num')

dta <- states.ref %>%
  as.data.frame() %>% 
  dplyr::select(-geometry) %>% 
  right_join(dta, by = 'cell_id')

dta.state.uncert <- dta %>% 
  group_by(state) %>%
  summarize(mean_uncert = mean(pred_sd)) %>% 
  arrange(desc(mean_uncert)) %>% 
  slice(1:5) %>% 
  inner_join(states, by = 'state')

dta.state.predMean <- dta %>% 
  group_by(state) %>%
  summarize(pred_mean = mean(pred_mean)) %>% 
  arrange(desc(pred_mean)) %>% 
  slice(1:5) %>% 
  inner_join(states, by = 'state')





###### Actually-used plots ##### 
states <- read_sf(here::here('ancillary_data', 'raw', 'Census','cb_2015_us_state_500k', 
                             'cb_2015_us_state_500k.shp'))
# 2c.iii Combine 
states <- states %>% 
  rename(state = STUSPS) %>%
  mutate(state_num = as.numeric(STATEFP)) %>% 
  dplyr::select(-AFFGEOID, -ALAND, -AWATER, -GEOID, -LSAD, -NAME, 
                -STATEFP, -STATENS)
states <- states %>% st_transform(crs=st_crs(projString))
# 1c Read EPA region table
epaRegion <- read_csv(here::here('ancillary_data', 'generated', 'epa_regions.csv'))

# 1d Combine 
states  <- states %>% 
  inner_join(epaRegion, by = 'state') %>% 
  filter(!state%in%c('AK', 'HI'))

dta <- read_fst(here::here('uncertainty_factor_analysis_ISEE', 'BNE_outputs',
                     'BNEoutputs_combined.fst'))

dta.mean <- dta %>% 
  mutate(lat2 = round(lat, 2), lon2 = round(lon, 2)) %>%
  group_by(lat2, lon2) %>% 
  summarize_all(mean) 

table(dta.mean$YYYY)

dta.mean <- dta.mean %>%
  mutate(run_id = 'mean_run')

dta.mean <- dta.mean %>% 
  mutate(YYYY2 = abs(YYYY - 2012.5)) %>%
  arrange(desc(YYYY2)) 

dta.mean.sf <- dta.mean %>% 
  st_as_sf(coords = c('lon', 'lat'), crs=st_crs('epsg:4326'))  %>% 
  st_transform(crs=st_crs(projString)) %>% 
  sample_frac(0.1)


state.mean <- st_intersection(dta.mean.sf, states) %>% 
  group_by(state) %>% 
  summarize_all(mean) %>% 
  arrange(desc(pred_sd)) %>% 
  slice(1:5)

states.highUncert <- states %>% 
  filter(state%in%state.mean$state)

png(here::here('uncertainty_factor_analysis_ISEE', 'outputs', 'presentation',
               'mean_uncert_2.png'))
plotSpatialOneBNEParameterWithStates(BNEoutput = dta.mean, 
                           runID = 'mean_run', 
                           parameterName = 'pred_sd', input = '', 
                           pTitle = expression('Mean'~'Uncertainty'~'of'~'Annual'~'PM'[2.5]~'Estimates'), 
                           subtitle ='', 'LegY', 
                           stateLayer = states.highUncert)
dev.off()

png(here::here('uncertainty_factor_analysis_ISEE', 'outputs', 'presentation',
               'mean_pred_2.png'))
plotSpatialOneBNEParameter(BNEoutput = dta.mean,
                                     runID = 'mean_run', 
                                     parameterName = 'pred_mean', input = '', 
                                     pTitle = expression('Mean'~'Estimated'~'Annual'~'PM'[2.5]), 
                                     subtitle ='', 'LegY')
dev.off()

summary(dta.mean$pred_mean)
,