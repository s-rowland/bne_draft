# Examine Relationship Input Disagreement and Prediction Uncertainty 
# Assess BNE Properties: Round 1
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Calculate Input Disagreement 
# 2: Plot Disagreement 
# 3: Calculate Correlations
# 4: Plot Correlations

####**************
#### N: Notes ####
####**************

# Right now we are using training and avgscm prediction datasets inherited 
# from the Capstone students' project, so the code it not yet 100% reproducible

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists('Ran_0_00')){
  # change this to something .md when I can 
  here::i_am('README.rtf')
  source(here::here('Scripts', '0_set_up', '0_00_setUp_env.R'))
}

# 0b Make the BNE output table, if it has not already been made 
if(!exists('Ran_b_01')){
  # change this to something .md when I can 
  here::i_am('README.rtf')
  source(here::here('Scripts', '0_set_up', 'b_01_readin_BNEoutputs.R'))
}

# 1a Set Year
YYYY <- 2010

####*************************************
#### 1: Calculate Input Disagreement ####
####*************************************

# 1b Readin predictions
dtaInput <- read_csv(here::here(
  'BNE_Inputs', 'c_03_parcels', 'annual',
  paste0('Predictions_', YYYY, '_' , paste(set_AllInputSet(YYYY), collapse = ''), '_1percent.csv')))

# 1c Wrangle predictions
# 1c.i Convert to simple features
dtaInput <- dtaInput %>% 
  st_as_sf(., coords = c('lon', 'lat'), 
           crs=st_crs('epsg:4326'))
# 1c.ii Change projections
dtaInput <- st_transform(dtaInput, crs=st_crs(projString))

# 1d Calculate standard deviation and range of input models
selectedInputs <- dtaInput %>%
  as.data.frame() %>%
  dplyr::select(AV, GS, CM, JS, CC) 

dtaInput$inputSD <- apply(selectedInputs, 1, function(x) muStat::stdev(x, unbiased=FALSE))
dtaInput$inputRange <- apply(selectedInputs, 1, function(x) max(x)) - 
  apply(selectedInputs, 1, function(x) min(x))



cor(dtaInput$inputSD, dtaInput$inputRange)

####***************************************
#### 2: Add BNE Prediction Uncertainty ####
####***************************************

# 2a Combine data sets 
# 2a.i Convert BNEout.run to simple features
BNEout.run <- BNEout %>% 
  filter(RunID == paste0(YYYY, '_AVGSCMJSCC_3.5_all_all')) %>% 
  st_as_sf(., coords = c('lon', 'lat'), 
           crs=st_crs('epsg:4326')) %>%
  st_transform(., crs=st_crs(projString))
# 2a.ii Combine 
dtaInput$PredIndex <- unlist(st_nn(dtaInput, BNEout.run, k=1))
BNEout.run <- BNEout.run %>% 
  mutate(PredIndex = row_number()) %>%
  as.data.frame() %>% 
  dplyr::select(-geometry) 
dtaInput <- dtaInput %>% 
  inner_join(BNEout.run, by = 'PredIndex')

####******************************
#### 3: Add Monitor Locations ####
####******************************

# 3a Read Dataset
# 3a.i determine which input models are active for that year
AllInputSet <- set_AllInputSet(YYYY)
# 3a.ii Actually readin the dataset
train <- read_csv(here::here(
  'BNE_Inputs', 'c_01_trainingData', 
  paste0('Training_', YYYY, '_' ,  paste(AllInputSet, collapse = ''), '.csv')))

# 3b Create aqs dataset 
aqs <- train %>% 
  dplyr::select(aqs, lat, lon)

# 3c Convert to simple features
aqs <- aqs %>% 
  st_as_sf(., coords = c("lon", "lat"), crs=st_crs('epsg:4326')) %>% 
  st_transform(., crs=st_crs(projString))

# 3d Combine with other data
dtaInput$NNMonDist <- as.numeric(st_nn(dtaInput, aqs, returnDist = TRUE)[[2]])

####**************************
#### 4: Plot Disagreement ####
####**************************

png(here::here('ISEE_analysis', 'outputs', 
               paste0('InputSDandYSD_sqrtcol_', YYYY, '.png')))
cowplot::plot_grid(
  plotSpatial_oneParameter(dtaInput, 'inputSD', ' ', 'LegY'),
  plotSpatial_oneBNEParameter(YYYY, 'AVGSCMJSCC', 3.5, 'pred_sd','', '', 'LegY'),
  nrow = 2)
dev.off()

####**************************
#### 5: Plot Correlations ####
####**************************

# Main one 
png(here::here('ISEE_analysis', 'outputs',  
               paste0('corr_InputSD_ySD_', YYYY, '.png')))
ggplot(dtaInput) + 
  geom_point(aes(x=inputSD, y = pred_sd)) + 
  #geom_smooth(aes(x=NNMonDist, y = inputSD),  color = 'blue') +
  ggtitle('Relationship Between Input SD and \n Prediction Uncertainty') + 
  labs(x= 'SD of Input Models', y = 'Prediction Uncertainty') + 
  annotate('text', x = 1, y = 3.5,
           label= paste0('Correlation = ',
                         round(cor(dtaInput$inputSD, dtaInput$pred_sd), 2 )))
dev.off()

# Looking at NNMonitors
png(here::here('ISEE_analysis', 'outputs', 
               paste0('corr_NNMon_ySD_', YYYY, '.png')))
ggplot(dtaInput) + 
  geom_point(aes(x=NNMonDist, y = pred_sd)) + 
  #geom_smooth(aes(x=NNMonDist, y = inputSD),  color = 'blue') +
  ggtitle('Relationship Between Distance to Nearest Monitor and \n Prediction Uncertainty') + 
  labs(x= 'Distance to Nearest Monitor (m)', y = 'Prediction Uncertainty') + 
  annotate('text', x = 100000, y = 3.5,
           label= paste0('Correlation = ',
                         round(cor(dtaInput$NNMonDist, dtaInput$pred_sd), 2 )))
dev.off()

# Another one 
png(here::here('ISEE_analysis', 'outputs', 
               paste0('corr_NNMon_inputSD_', YYYY, '.png')))
ggplot(dtaInput) + 
  geom_point(aes(x=NNMonDist, y = inputSD)) + 
  #geom_smooth(aes(x=NNMonDist, y = inputSD),  color = 'blue') +
  ggtitle('Relationship Between Distance to Nearest Monitor and \n Input Model Disagreement') + 
  labs(x= 'Distance to Nearest Monitor (m)', y = 'SD of Input Models') + 
  annotate('text', x = 100000, y = 3.5,
           label= paste0('Correlation = ',
                         round(cor(dtaInput$NNMonDist, dtaInput$inputSD), 2 )))
dev.off()

####*************************************************
#### 6: Plot Monitors and Prediction Uncertainty ####
####*************************************************


# 1B.a Create ParameterTitle 
ParameterName <- 'pred_sd'
if(ParameterName == 'w_mean'){ParameterTitle <- paste0('Mean of ', Input, ' Weight')}
if(ParameterName == 'w_std'){ParameterTitle <- paste0('SD of ', Input, ' Weight')}
if(ParameterName == 'bias_mean'){ParameterTitle <- 'Mean of Bias Term'}
if(ParameterName == 'bias_std'){ParameterTitle <- 'SD of Bias Term'}
if(ParameterName == 'y_mean'){ParameterTitle <- 'Predicted Concentration'}
if(ParameterName == 'pred_sd'){ParameterTitle <- 'Prediction Uncertainty'}
if(ParameterName == 'HighWI'){ParameterTitle <- 'Most-Weighted'}

# 1B.c Set range
if(ParameterName == 'bias_mean'){
  Parameter.min <- min(BNEout$activeVar); Parameter.midneg <- min(BNEout$activeVar)/2
  Parameter.midpos <- max(BNEout$activeVar)/2; Parameter.max <- max(BNEout$activeVar)
} 

# 1B.a Set the aesthetic schemes 
# 1B.a.i for weights' expected value
if(ParameterName == 'w_mean' | ParameterName == 'w_std' ){
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
} else if(ParameterName == 'pred_sd'){
  fillScheme <- viridis::scale_fill_viridis(
    direction = -1, option = 'magma',
    breaks = c(0, 1, 2, 3, 4, 4.76),
    limits = c(0, 4.76), trans = 'sqrt')
  
  colorScheme <- viridis::scale_color_viridis(
    direction = -1, option = 'magma',
    breaks = c(0, 1, 2, 3, 4, 4.76),
    limits = c(0, 4.76), trans = 'sqrt')
}  else {
  fillScheme <- viridis::scale_fill_viridis(
    direction = -1, option = 'magma',
    breaks = c(0, round(quantile(BNEout$activeVar, 0.25)[[1]],2), 
               round(quantile(BNEout$activeVar, 0.5)[[1]],2), 
               round(quantile(BNEout$activeVar, 0.75)[[1]],2), 
               round(max(BNEout$activeVar),2)),
    #limits = c(0, 2))
    limits = c(0, round(max(BNEout$activeVar),2)))
  
  colorScheme <- viridis::scale_color_viridis(
    direction = -1, option = 'magma',
    breaks = c(0, round(quantile(BNEout$activeVar, 0.25)[[1]],2), 
               round(quantile(BNEout$activeVar, 0.5)[[1]],2), 
               round(quantile(BNEout$activeVar, 0.75)[[1]],2), 
               round(max(BNEout$activeVar),2)),
    #limits = c(0, 2))
    limits = c(0, round(max(BNEout$activeVar),2)))
} 

# Set legend position
LegPos <- 'right'

# Make the plot
p1 <- ggplot(conus) + 
  geom_sf(fill = NA)  + 
  geom_sf(data = dtaInput, aes(fill= pred_sd, color = pred_sd), size = 0.25) + 
  geom_sf(data = aqs, fill= 'black', color = 'black', size = 0.25) + 
  
  fillScheme + colorScheme +
  guides(fill = guide_colorbar(title = ParameterTitle), 
         color = guide_colorbar(title = ParameterTitle)) + 
  ggtitle(paste0(ParameterTitle, ' for ', YYYY)) + 
  labs(x='', y='') +
  theme(plot.title = element_text(size = 18)) + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  theme_void() + 
  theme(legend.position = LegPos) 

p2 <- ggplot(conus) + 
  geom_sf(fill = NA)  + 
  geom_sf(data = dtaInput, aes(fill= bias_sd, color = bias_sd), size = 0.25) + 
  geom_sf(data = aqs, fill= 'black', color = 'black', size = 0.25) + 
  fillScheme + colorScheme +
  guides(fill = guide_colorbar(title = 'offset SD'), 
         color = guide_colorbar(title = 'offset SD')) + 
  ggtitle(paste0('Offset SD', ' for ', YYYY)) + 
  labs(x='', y='') +
  theme(plot.title = element_text(size = 18)) + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  theme_void() +
  theme(legend.position = LegPos) 

dtaInput <- dtaInput %>% mutate(Diff = abs(pred_sd - bias_sd)) %>% 
  mutate(Diff = if_else(Diff ==0, 0.0001, Diff))

p3 <- ggplot(conus) + 
  geom_sf(fill = NA)  + 
  geom_sf(data = dtaInput, aes(fill= Diff, color = Diff), size = 0.25) + 
  geom_sf(data = aqs, fill= 'black', color = 'black', size = 0.25) + 
  scale_fill_gradient(high = 'cyan', low = 'darkseagreen4', trans = 'sqrt') +
  scale_color_gradient(high = 'cyan', low = 'darkseagreen4', trans = 'sqrt') +
  guides(fill = guide_colorbar(title = 'Difference'), 
         color = guide_colorbar(title = 'Difference')) + 
  ggtitle(paste0('Pred SD Minus offset SD', ' for ', YYYY)) + 
  labs(x='', y='') +
  theme(plot.title = element_text(size = 18)) + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  theme_void() +
  theme(legend.position = LegPos) 

p4 <- ggplot(conus) + 
  geom_sf(fill = NA)  + 
  geom_sf(data = dtaInput, aes(fill= NNMonDist, color = NNMonDist), size = 0.25) + 
  geom_sf(data = aqs, fill= 'black', color = 'black', size = 0.25) + 
  viridis::scale_fill_viridis(
    direction = -1, option = 'magma', trans = 'sqrt') +
  viridis::scale_color_viridis(
    direction = -1, option = 'magma', trans = 'sqrt') +
  guides(fill = guide_colorbar(title = 'dist to Nearest  Monitor'), 
         color = guide_colorbar(title = 'dist to Nearest  Monitor')) + 
  ggtitle(paste0('Dist to Nearest  Monitor', ' for ', YYYY)) + 
  labs(x='', y='') +
  theme(plot.title = element_text(size = 18)) + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  theme_void() +
  theme(legend.position = LegPos) 

p5 <- ggplot(conus) + 
  geom_sf(fill = NA)  + 
  geom_sf(data = dtaInput, aes(fill= inputSD, color = inputSD), size = 0.25) + 
  geom_sf(data = aqs, fill= 'black', color = 'black', size = 0.25) + 
  viridis::scale_fill_viridis(
    direction = -1, option = 'magma', trans = 'sqrt') +
  viridis::scale_color_viridis(
    direction = -1, option = 'magma', trans = 'sqrt') +
  guides(fill = guide_colorbar(title = 'Input SD'), 
         color = guide_colorbar(title = 'Input SD')) + 
  ggtitle(paste0('Disagrement among input models', ' for ', YYYY)) + 
  labs(x='', y='') +
  theme(plot.title = element_text(size = 18)) + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  theme_void() +
  theme(legend.position = LegPos) 

# 3i Output plot
png(here::here('ISEE_analysis', 'outputs', 
               paste0('Monitors_ySD_', YYYY, '_yes.png')), 
    height = 800, width = 800)
cowplot::plot_grid(p1, p2,p3, p4,  p5,  nrow = 3)
dev.off()


pB <- ggplot(conus) + 
  geom_sf(fill = NA)  + 
  geom_sf(data = dtaInput, aes(fill= bias_mean, color = bias_mean), size = 0.25) + 
  geom_sf(data = aqs, fill= 'black', color = 'black', size = 0.25) + 
 # viridis::scale_fill_viridis(
  #  direction = -1, option = 'magma', trans = 'sqrt') +
  #viridis::scale_color_viridis(
   # direction = -1, option = 'magma', trans = 'sqrt') +
  guides(fill = guide_colorbar(title = 'Bias Mean'), 
         color = guide_colorbar(title = 'Bias Mean')) + 
  ggtitle(paste0('Bias Mean', ' for ', YYYY)) + 
  labs(x='', y='') +
  theme(plot.title = element_text(size = 18)) + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  theme_void() +
  theme(legend.position = LegPos)
png(here::here('ISEE_analysis', 'outputs', 
               paste0('Monitors_ySD_', YYYY, '_yesSpecial.png')), 
    height = 800, width = 800)
cowplot::plot_grid(p2, pB, nrow = 2)
dev.off()


png(here::here('ISEE_analysis', 'outputs', 
               paste0('Monitors_ySD_', YYYY, '_a.png')), 
    height = 800, width = 800)
cowplot::plot_grid(p1, p2, ncol = 2)
dev.off()

png(here::here('ISEE_analysis', 'outputs', 
               paste0('Monitors_ySD_', YYYY, '_b.png')), 
    height = 800, width = 800)
cowplot::plot_grid(p3, p4, ncol = 2)
dev.off()


p5 <- ggplot(conus) + 
  geom_sf(fill = NA)  + 
  geom_sf(data = dtaInput, aes(fill= w_sd_AV, color = w_sd_AV), size = 0.25) + 
  geom_sf(data = aqs, fill= 'black', color = 'black', size = 0.25) + 
  viridis::scale_fill_viridis(
    direction = -1, option = 'magma', trans = 'sqrt') +
  viridis::scale_color_viridis(
    direction = -1, option = 'magma', trans = 'sqrt') +
  guides(fill = guide_colorbar(title = 'dist to Nearest  Monitor'), 
         color = guide_colorbar(title = 'dist to Nearest  Monitor')) + 
  ggtitle(paste0('Dist to Nearest  Monitor', ' for ', YYYY)) + 
  labs(x='', y='') +
  theme(plot.title = element_text(size = 18)) + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  theme_void() +
  theme(legend.position = LegPos) 
p6 <- ggplot(conus) + 
  geom_sf(fill = NA)  + 
  geom_sf(data = dtaInput, aes(fill= w_sd_GS, color = w_sd_GS), size = 0.25) + 
  geom_sf(data = aqs, fill= 'black', color = 'black', size = 0.25) + 
  viridis::scale_fill_viridis(
    direction = -1, option = 'magma', trans = 'sqrt') +
  viridis::scale_color_viridis(
    direction = -1, option = 'magma', trans = 'sqrt') +
  guides(fill = guide_colorbar(title = 'dist to Nearest  Monitor'), 
         color = guide_colorbar(title = 'dist to Nearest  Monitor')) + 
  ggtitle(paste0('Dist to Nearest  Monitor', ' for ', YYYY)) + 
  labs(x='', y='') +
  theme(plot.title = element_text(size = 18)) + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  theme_void() +
  theme(legend.position = LegPos) 
p7 <- ggplot(conus) + 
  geom_sf(fill = NA)  + 
  geom_sf(data = dtaInput, aes(fill= w_sd_CM, color = w_sd_CM), size = 0.25) + 
  geom_sf(data = aqs, fill= 'black', color = 'black', size = 0.25) + 
  viridis::scale_fill_viridis(
    direction = -1, option = 'magma', trans = 'sqrt') +
  viridis::scale_color_viridis(
    direction = -1, option = 'magma', trans = 'sqrt') +
  guides(fill = guide_colorbar(title = 'dist to Nearest  Monitor'), 
         color = guide_colorbar(title = 'dist to Nearest  Monitor')) + 
  ggtitle(paste0('Dist to Nearest  Monitor', ' for ', YYYY)) + 
  labs(x='', y='') +
  theme(plot.title = element_text(size = 18)) + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  theme_void() +
  theme(legend.position = LegPos) 

p8 <- ggplot(conus) + 
  geom_sf(fill = NA)  + 
  geom_sf(data = dtaInput, aes(fill= w_sd_JS, color = w_sd_JS), size = 0.25) + 
  geom_sf(data = aqs, fill= 'black', color = 'black', size = 0.25) + 
  viridis::scale_fill_viridis(
    direction = -1, option = 'magma', trans = 'sqrt') +
  viridis::scale_color_viridis(
    direction = -1, option = 'magma', trans = 'sqrt') +
  guides(fill = guide_colorbar(title = 'dist to Nearest  Monitor'), 
         color = guide_colorbar(title = 'dist to Nearest  Monitor')) + 
  ggtitle(paste0('Dist to Nearest  Monitor', ' for ', YYYY)) + 
  labs(x='', y='') +
  theme(plot.title = element_text(size = 18)) + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  theme_void() +
  theme(legend.position = LegPos) 






png(here::here('ISEE_analysis', 'outputs', 
               paste0('Monitors_WeightsSD_', YYYY, '_b.png')), 
    height = 800, width = 800)
cowplot::plot_grid(p5, p6, p7, p8, ncol = 2)
dev.off()


data.frame(Year =YYYY, 
           CorrPredSDBiasSD = cor(dtaInput$pred_sd, dtaInput$bias_sd), 
           CorrPredSDNNMon = cor(dtaInput$pred_sd, dtaInput$NNMonDist), 
           CorrBiasSDNNMon = cor(dtaInput$bias_sd, dtaInput$NNMonDist)) %>% 
  write_csv(paste0('~/Desktop/Corrs_', YYYY, '.csv'))

cor(dtaInput$pred_sd, dtaInput$inputSD)
cor(dtaInput$Diff, dtaInput$inputSD)
cor(dtaInput$Diff, dtaInput$NNMonDist)
