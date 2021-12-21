# Plot Distribution of prediction models
# Examine CAMS
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Wrangle Predictions 
# 2: Plot CAMS Predictions 
# 3: Plot CAMS Error 
# 4: Histogram of Prediction Models
# 5: Table of Prediction Model Distribution
# 6: Table of Prediction Model Performance
# 7: Correlation of AQS and Prediction Models
# 8: Correlation Among Prediction Models

####**************
#### N: Notes ####
####**************

# Right now we are using training and avgscm prediction datasets inherited 
# from the Capstone students' project, so the code it not yet 100% reproducible

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_set_up_env.R"))
}

if(!exists("Ran_a_00_conusApp")){
  here::i_am("README.md")
  source(here::here('str_app_conus_uncert', 'scripts', 
                    "a_00_set_up_env_conusApp.R"))
}

dir.proj <- 'str_examine_cams'

####****************************
#### 1: Wrangle Predictions ####
####****************************

# 1a Define function to wrangle predictions 
read_predSet_cams <- function(YYYY, fold) {
  readr::read_csv(here::here('BNE_inputs', 'prediction_datasets', 'individual_annual', 
                                        paste0('predictions_avgscmjsccca_', YYYY, 
                                               '_', fold, '.csv')))
  
}

# 1b Readin prediction datasets
preds.conus.allYYYY <- bind_rows(
  read_predSet_cams(2010, 'all'), read_predSet_cams(2011, 'all'), 
  read_predSet_cams(2012, 'all'), read_predSet_cams(2013, 'all'),
  read_predSet_cams(2014, 'all'), read_predSet_cams(2015, 'all'))


preds.conus.allYYYY  <- preds.conus.allYYYY # %>% 
  #slice_sample(prop = 0.1)

preds.conus.2010 <- preds.conus.allYYYY %>% filter(time == 2010)
preds.conus.2011 <- preds.conus.allYYYY %>% filter(time == 2011)
preds.conus.2012 <- preds.conus.allYYYY %>% filter(time == 2012)
preds.conus.2013 <- preds.conus.allYYYY %>% filter(time == 2013)
preds.conus.2014 <- preds.conus.allYYYY %>% filter(time == 2014)
preds.conus.2015 <- preds.conus.allYYYY %>% filter(time == 2015)
# repeat for NYS
preds.nys.2010 <- read_predSet_cams(2010, 'NYS')
preds.nys.2015 <- read_predSet_cams(2015, 'NYS')
preds.nys.NYSYYYY <- bind_rows(
  read_predSet_cams(2010, 'NYS'), read_predSet_cams(2011, 'NYS'), 
  read_predSet_cams(2012, 'NYS'), read_predSet_cams(2013, 'NYS'),
  read_predSet_cams(2014, 'NYS'), read_predSet_cams(2015, 'NYS'))




####******************************
#### 2: Plot CAMS Predictions ####
####******************************
 
# 2a CONUS 
# 2a.i create vector of concentration values 
predMean.vec <- c(preds.conus.2010$cams_pred, preds.conus.2010$js_pred, 
                  preds.conus.2010$cmaq_outs_pred, preds.conus.2015$cams_pred, 
                  preds.conus.2015$js_pred, preds.conus.2015$cmaq_outs_pred)
p.min <- min(predMean.vec); p.max <- max(predMean.vec)
# manually set to account for log scale of colors
p.1 <- 25; p.2 = 50; p.3 = 100
valueVec.wSD <- c(round(p.min,2), round(p.1,2),  round(p.2,2),
                  round(p.3,2), 75, round(p.max,2))

# 2a.ii make plot
png(here::here(dir.proj, 'outputs', 'a1a_predictions_conus_2010_2015.png'), 
    height = 600, width = 1000)
cowplot::plot_grid(
  plotSpatialOneBNEParameter(dta = preds.conus.2010, parameter = 'cams_pred', 
                             mainTitle = '2010 CAMS Predictions', 
                             valueScale = valueVec.wSD, legYN = 'legY'), 
  plotSpatialOneBNEParameter(dta = preds.conus.2015, parameter = 'cams_pred', 
                             mainTitle = '2015 CAMS Predictions', 
                             valueScale = valueVec.wSD, legYN = 'legY'), 
  makeNiceHistogram(preds.conus.2010, 'cams_pred', mainTitle = '2010 CAMS ',
                    xRange = c(0, 124), binCount = 60),
  makeNiceHistogram(preds.conus.2015, 'cams_pred', mainTitle = '2015 CAMS ',
                    xRange = c(0, 124), binCount = 60),
  nrow = 2, ncol = 2, rel_heights = 4:2)
dev.off()

# 2b NYS 
# 2a.i create vector of concentration values 
predMean.vec <- c(preds.nys.2010$cams_pred, preds.nys.2010$js_pred, 
                  preds.nys.2010$cmaq_outs_pred, preds.nys.2015$cams_pred, 
                  preds.nys.2015$js_pred, preds.nys.2015$cmaq_outs_pred)
p.min <- min(predMean.vec); p.max <- max(predMean.vec)
# manually set to account for log scale of colors
p.1 <- 10; p.2 = 20; p.3 <- 25
valueVec.wSD <- c(round(p.min,2), round(p.1,2),  round(p.2,2),
                  p.3, 35)
# 2b.ii make plot
png(here::here(dir.proj, 'outputs', 'a1b_predictions_nys_2010_2015.png'), 
    height = 600, width = 1000)
cowplot::plot_grid(
  plotSpatialOneBNEParameter(dta = preds.nys.2010, parameter = 'cams_pred', 
                             mainTitle = '2010 CAMS Predictions', 
                             valueScale = valueVec.wSD, legYN = 'legY'), 
  plotSpatialOneBNEParameter(dta = preds.nys.2015, parameter = 'cams_pred', 
                             mainTitle = '2015 CAMS Predictions', 
                             valueScale = valueVec.wSD, legYN = 'legY'), 
  makeNiceHistogram(preds.nys.2010, 'cams_pred', mainTitle = '2010 CAMS ',
                    xRange = c(0, 31), binCount = 60),
  makeNiceHistogram(preds.nys.2015, 'cams_pred', mainTitle = '2015 CAMS ',
                    xRange = c(0, 31), binCount = 60),
  nrow = 2, ncol = 2, rel_heights = 4:2)
dev.off()


####************************
#### 3: Plot CAMS Error ####
####************************

# 3a Read AQS data 
# 3a.i function to readin aqs
add_pred_to_aqs <- function(YYYY) {
  aqs <- read_csv(here::here('BNE_inputs', 'training_datasets', 'individual_annual', 
                             paste0('training_avgscmjsccca_', YYYY, '_all.csv')))
  return(aqs)
}
# 3a.ii bring in all the aqs data
aqs <- map_dfr(2010:2015, add_pred_to_aqs)
# 3a.iii remove island observations 
# 2010 only has conus
aqs <- aqs %>% 
  filter(lat > min(filter(aqs, year ==2010)$lat) &  
           lat < max(filter(aqs, year ==2010)$lat) & 
           lon > min(filter(aqs, year ==2010)$lon) &  
           lon < max(filter(aqs, year ==2010)$lon))

# 3b compute error 
aqs <- aqs %>% 
  mutate(err_av = av_pred - obs_pm2_5, err_gs = gs_pred - obs_pm2_5, err_cm = cmaq_outs_pred - obs_pm2_5, 
         err_js = js_pred - obs_pm2_5, err_cc =  caces_pred - obs_pm2_5, err_ca = cams_pred - obs_pm2_5)

# remove one spot that is super high 
aqs.sm <- aqs %>% filter(cams_pred < 100)
aqs.bg <- aqs %>% filter(cams_pred > 100)

# 3c aggregate error 
aqs.agg <- aqs %>% 
  group_by(lat, lon) %>% 
  summarize(rmse_av = sqrt(mean(err_av^2)), 
            rmse_gs = sqrt(mean(err_gs^2)),
            rmse_cm = sqrt(mean(err_cm^2)),
            rmse_js = sqrt(mean(err_js^2)),
            rmse_cc = sqrt(mean(err_cc^2)),
            rmse_ca = sqrt(mean(err_ca^2)), 
            me_av = mean(err_av),
            me_gs = mean(err_gs),
            me_cm = mean(err_cm),
            me_js = mean(err_js),
            me_cc = mean(err_cc),
            me_ca = mean(err_ca))

aqs.agg.sm <- aqs.sm %>% 
  group_by(lat, lon) %>% 
  summarize(rmse_av = sqrt(mean(err_av^2)), 
            rmse_gs = sqrt(mean(err_gs^2)),
            rmse_cm = sqrt(mean(err_cm^2)),
            rmse_js = sqrt(mean(err_js^2)),
            rmse_cc = sqrt(mean(err_cc^2)),
            rmse_ca = sqrt(mean(err_ca^2)), 
            me_av = mean(err_av),
            me_gs = mean(err_gs),
            me_cm = mean(err_cm),
            me_js = mean(err_js),
            me_cc = mean(err_cc),
            me_ca = mean(err_ca))
# 3e create error scale
errorScale <- c(round(min(aqs$err_ca),2), 0, 10, 20, 30, 40)

# 3f bring in conus outline shapefile
conus <- st_read(here::here('data_ancillary', 'formatted', 'spatial_outlines', 
                            'conus.shp'))

# 3g create conus plot
png(here::here(dir.proj, 'outputs', 'a2b_annual_error_conus.png'), 
    height = 600, width = 1500)
cowplot::plot_grid(
  plotSpatialOneBNEParameter(dta = filter(aqs.sm, year == 2010), parameter = 'err_ca', 
                             mainTitle = '2010 CAMS Error', plotOutline = conus,
                             valueScale = errorScale, legYN = 'legY'),
  plotSpatialOneBNEParameter(dta = filter(aqs.sm, year == 2015), parameter = 'err_ca', 
                             mainTitle = '2015 CAMS Error', plotOutline = conus,
                             valueScale = errorScale, legYN = 'legY'),
  plotSpatialOneBNEParameter(dta = aqs.agg.sm, parameter = 'me_ca', 
                             mainTitle = 'Ave CAMS Error', plotOutline = conus,
                             valueScale = errorScale, legYN = 'legY'),
  makeNiceHistogram(filter(aqs.sm, year == 2010), 'err_ca', mainTitle = '2010 CAMS Error',
                    xRange = c(-15, 50), binCount = 60),
  makeNiceHistogram(filter(aqs.sm, year == 2015), 'err_ca', mainTitle = '2015 CAMS Error',
                    xRange = c(-15, 50), binCount = 60),
  makeNiceHistogram(aqs.agg.sm, 'me_ca', mainTitle = '2015 CAMS Error',
                    xRange = c(-15, 50), binCount = 60),
  nrow = 2, ncol = 3, rel_heights = 4:2)
dev.off()

# 3h create nys plot
# 3h.i restrict data to NYS bounding box
aqs.nys <- aqs %>% 
  filter(lat > min(preds.nys.2010$lat) & 
           lat < max(preds.nys.2010$lat) &
           lon > min(preds.nys.2010$lon) & 
           lon < max(preds.nys.2010$lon) )
aqs.agg.nys <- aqs.agg %>% 
  filter(lat > min(preds.nys.2010$lat) & 
           lat < max(preds.nys.2010$lat) &
           lon > min(preds.nys.2010$lon) & 
           lon < max(preds.nys.2010$lon) )
# 3h.ii make NYS outline
usa <- st_read(here::here('data_ancillary', 'raw', 'Census', 'cb_2015_us_state_500k', 
                          'cb_2015_us_state_500k.shp'))
nys <- usa %>% filter(NAME == 'New York')

# 3h.iii make plot
png(here::here(dir.proj, 'outputs', 'a2b_annual_error_nys.png'), 
    height = 600, width = 1500)
cowplot::plot_grid(
  plotSpatialOneBNEParameter(dta = filter(aqs.nys, year == 2010), parameter = 'err_ca', 
                             mainTitle = '2010 CAMS Error', plotOutline = nys,
                             valueScale = errorScale, legYN = 'legY', pointSize = 2),
  plotSpatialOneBNEParameter(dta = filter(aqs.nys, year == 2015), parameter = 'err_ca', 
                             mainTitle = '2015 CAMS Error', plotOutline = nys,
                             valueScale = errorScale, legYN = 'legY', pointSize = 2),
  plotSpatialOneBNEParameter(dta = aqs.agg.nys, parameter = 'me_ca', 
                             mainTitle = 'Ave CAMS Error', plotOutline = nys,
                             valueScale = errorScale, legYN = 'legY', pointSize = 2),
  makeNiceHistogram(filter(aqs.nys, year == 2010), 'err_ca', mainTitle = '2010 CAMS Error',
                    xRange = c(-15, 50), binCount = 60),
  makeNiceHistogram(filter(aqs.nys, year == 2015), 'err_ca', mainTitle = '2015 CAMS Error',
                    xRange = c(-15, 50), binCount = 60),
  makeNiceHistogram(aqs.agg.nys, 'me_ca', mainTitle = '2015 CAMS Error',
                    xRange = c(-15, 50), binCount = 60),
  nrow = 2, ncol = 3, rel_heights = 4:2)
dev.off()

####***************************************
#### 4: Histogram of Prediction Models ####
####***************************************

png(here::here(dir.proj, 'outputs', 'a3_histogram.png'), 
    height = 400, width = 1000)
cowplot::plot_grid(
  makeNiceHistogram(preds.conus.2010, 'cams_pred', mainTitle = '2010 CAMS ',
                    xRange = c(0, 124), binCount = 60),
  makeNiceHistogram(preds.conus.2015, 'cams_pred', mainTitle = '2015 CAMS',
                    xRange = c(0, 124), binCount = 60),
  makeNiceHistogram(preds.conus.2010, 'cmaq_outs_pred', mainTitle = '2010 CMAQ Fusion',
                    xRange = c(0, 124), binCount = 60),
  makeNiceHistogram(preds.conus.2015, 'cmaq_outs_pred', mainTitle = '2015 CMAQ Fusion',
                    xRange = c(0, 124), binCount = 60),
  makeNiceHistogram(preds.conus.2010, 'js_pred', mainTitle = '2010 Schwartz Model',
                    xRange = c(0, 124), binCount = 60),
  makeNiceHistogram(preds.conus.2015, 'js_pred', mainTitle = '2015 Schwartz Model',
                    xRange = c(0, 124), binCount = 60),
  
  nrow = 3, ncol = 2)
dev.off()

####***********************************************
#### 5: Table of Prediction Model Distribution ####
####***********************************************

meanPM <- data.frame(mean= c(mean(preds.conus.2010$cams_pred), 
                             mean(preds.conus.2010$av_pred),
                             mean(preds.conus.2010$gs_pred),
                             mean(preds.conus.2010$cmaq_outs_pred),
                             mean(preds.conus.2010$js_pred),
                             mean(preds.conus.2010$caces_pred)), 
                     sd= c(sd(preds.conus.2010$cams_pred), 
                           sd(preds.conus.2010$av_pred),
                           sd(preds.conus.2010$gs_pred),
                           sd(preds.conus.2010$cmaq_outs_pred),
                           sd(preds.conus.2010$js_pred),
                           sd(preds.conus.2010$caces_pred)), 
                     min= c(min(preds.conus.2010$cams_pred), 
                            min(preds.conus.2010$av_pred),
                            min(preds.conus.2010$gs_pred),
                            min(preds.conus.2010$cmaq_outs_pred),
                            min(preds.conus.2010$js_pred),
                            min(preds.conus.2010$caces_pred)), 
                     q1= c(quantile(preds.conus.2010$cams_pred, 0.25), 
                           quantile(preds.conus.2010$av_pred, 0.25),
                           quantile(preds.conus.2010$gs_pred, 0.25),
                           quantile(preds.conus.2010$cmaq_outs_pred, 0.25),
                           quantile(preds.conus.2010$js_pred, 0.25),
                           quantile(preds.conus.2010$caces_pred, 0.25)),
                     median= c(median(preds.conus.2010$cams_pred), 
                               median(preds.conus.2010$av_pred),
                               median(preds.conus.2010$gs_pred),
                               median(preds.conus.2010$cmaq_outs_pred),
                               median(preds.conus.2010$js_pred),
                               median(preds.conus.2010$caces_pred)),
                     q3= c(quantile(preds.conus.2010$cams_pred, 0.75), 
                           quantile(preds.conus.2010$av_pred, 0.75),
                           quantile(preds.conus.2010$gs_pred, 0.75),
                           quantile(preds.conus.2010$cmaq_outs_pred, 0.75),
                           quantile(preds.conus.2010$js_pred, 0.75),
                           quantile(preds.conus.2010$caces_pred, 0.75)),
                     max= c(max(preds.conus.2010$cams_pred), 
                            max(preds.conus.2010$av_pred),
                            max(preds.conus.2010$gs_pred),
                            max(preds.conus.2010$cmaq_outs_pred),
                            max(preds.conus.2010$js_pred),
                            max(preds.conus.2010$caces_pred)))

####**********************************************
#### 6: Table of Prediction Model Performance ####
####**********************************************

meanPerformance <- data.frame(
  meanRMSE = c(mean(aqs.agg$rmse_ca), 
                             mean(aqs.agg$rmse_av),
                             mean(aqs.agg$rmse_gs),
                             mean(aqs.agg$rmse_cm),
                             mean(aqs.agg$rmse_js),
                             mean(aqs.agg$rmse_cc)), 
                     sdRMSE = c(sd(aqs.agg$rmse_ca), 
                                sd(aqs.agg$rmse_av),
                                sd(aqs.agg$rmse_gs),
                                sd(aqs.agg$rmse_cm),
                                sd(aqs.agg$rmse_js),
                                sd(aqs.agg$rmse_cc)), 
                     meanME = c(mean(aqs.agg$me_ca), 
                                  mean(aqs.agg$me_av),
                                  mean(aqs.agg$me_gs),
                                  mean(aqs.agg$me_cm),
                                  mean(aqs.agg$me_js),
                                  mean(aqs.agg$me_cc)), 
                     sdME = c(sd(aqs.agg$me_ca), 
                                sd(aqs.agg$me_av),
                                sd(aqs.agg$me_gs),
                                sd(aqs.agg$me_cm),
                                sd(aqs.agg$me_js),
                                sd(aqs.agg$me_cc))) %>% 
  mutate(meanRMSE = round(meanRMSE, 2), 
         sdRMSE = round(sdRMSE, 2),
         meanME = round(meanME, 2), 
         sdME = round(sdME, 2))

####*************************************************
#### 7: Correlation of AQS and Prediction Models ####
####*************************************************

# 7a define function to get spatial corr
calc_corr_aqs <- function(YYYY) {
  aqsYYYY <- aqs %>% filter(year == YYYY)
  varName = paste0('Y', YYYY)
  data.frame(corr = c(cor(aqsYYYY$obs_pm2_5, aqsYYYY$cams_pred), 
                      cor(aqsYYYY$obs_pm2_5, aqsYYYY$av_pred),
                      cor(aqsYYYY$obs_pm2_5, aqsYYYY$gs_pred),
                      cor(aqsYYYY$obs_pm2_5, aqsYYYY$cmaq_outs_pred),
                      cor(aqsYYYY$obs_pm2_5, aqsYYYY$js_pred),
                      cor(aqsYYYY$obs_pm2_5, aqsYYYY$caces_pred))) %>% 
    mutate(!!varName := round(corr, 2)) %>% 
    dplyr::select(-corr)
}
# 7b fill in table
corTable <- map(2010:2015, calc_corr_aqs) %>%
  bind_cols()

 ####********************************************
#### 8: Correlation Among Prediction Models ####
####********************************************

# 4a Create the year-specific correlations 
preds.preds.2010 <- preds.conus.2010 %>% 
  dplyr::select(contains('pred')) %>% 
  rename('vonDonkelaar' = av_pred, GBD = gs_pred, 'CMAQ Fusion' = cmaq_outs_pred, 
         'Schwartz Model' = js_pred, CACES = caces_pred, CAMS = cams_pred)
preds.cors.2010 <- cor(preds.preds.2010)

preds.preds.2015 <- preds.conus.2015 %>% 
  dplyr::select(contains('pred')) %>% 
  rename('vonDonkelaar' = av_pred, GBD = gs_pred, 'CMAQ Fusion' = cmaq_outs_pred, 
         'Schwartz Model' = js_pred, CACES = caces_pred, CAMS = cams_pred)
preds.cors.2015 <- cor(preds.preds.2015)
# 4b Average across years
# 4c Plot 
png(here::here(dir.proj, 'outputs', 'a4i_corrs.png'), 
    height = 400, width = 400)
corrplot::corrplot(preds.cors.2010, type = 'lower', addCoef.col = 'black')
dev.off()

png(here::here(dir.proj, 'outputs', 'a4ii_corrs.png'), 
    height = 400, width = 400)
corrplot::corrplot(preds.cors.2015, type = 'lower', addCoef.col = 'black')
dev.off()


#__________________old code --------------------------#


####**************************************
#### 2: Plot Maps of CAMS Predictions ####
####**************************************

# 2a Create scale we can use across years 
# no common scale because 2010 has extremely high values
# well, we could make the color scale log. 
# 2a Make maps 
predMean.vec <- c(preds.conus.2010$cams_pred, preds.conus.2010$js_pred, 
                  preds.conus.2010$cmaq_outs_pred, preds.conus.2015$cams_pred, 
                  preds.conus.2015$js_pred, preds.conus.2015$cmaq_outs_pred)
p.min <- min(predMean.vec); p.max <- max(predMean.vec)
p.1 <- 3; p.2 = 10; p.3 = 50
valueVec.wSD <- c(round(p.min,2), round(p.1,2),  round(p.2,2), round(p.3,2), round(p.max,2))


png(here::here(dir.proj, 'outputs', 'a1_prediction_maps.png'), 
    height = 400, width = 1000)
cowplot::plot_grid(
  plotSpatialOneBNEParameter(dta = preds.conus.2010, parameter = 'cams_pred', 
                             mainTitle = '2010 CAMS Predictions', 
                             valueScale = valueVec.wSD, legYN = 'legN'), 
  plotSpatialOneBNEParameter(dta = preds.conus.2015, parameter = 'cams_pred', 
                             mainTitle = '2015 CAMS Predictions', 
                             valueScale = valueVec.wSD, legYN = 'legN'), 
  plotSpatialOneBNEParameter(dta = preds.conus.2010, parameter = 'cmaq_outs_pred', 
                             mainTitle = '2010 CAMQ Fusion Predictions', 
                             valueScale = valueVec.wSD, legYN = 'legN'), 
  plotSpatialOneBNEParameter(dta = preds.conus.2015, parameter = 'cmaq_outs_pred', 
                             mainTitle = '2015 CAMQ Fusion Predictions', 
                             valueScale = valueVec.wSD, legYN = 'legN'),
  plotSpatialOneBNEParameter(dta = preds.conus.2010, parameter = 'js_pred', 
                             mainTitle = '2010 Schwartz Model Predictions', 
                             valueScale = valueVec.wSD), 
  plotSpatialOneBNEParameter(dta = preds.conus.2015, parameter = 'js_pred', 
                             mainTitle = '2015 Schwartz Model Predictions', 
                             valueScale = valueVec.wSD),
  nrow = 2, ncol = 3)
dev.off()
  
  
  


# 2b Make maps for NYS
pdf(here::here(dir.proj, 'outputs', 'a2_prediction_maps_NYS.pdf'))
cowplot::plot_grid(
  plotSpatialOneBNEParameter(dta = preds.nys.2010, parameter = 'cams_pred', 
                             mainTitle = '2010 CAMS Predictions'), 
  plotSpatialOneBNEParameter(dta = preds.nys.2011, parameter = 'cams_pred', 
                             mainTitle = '2011 CAMS Predictions'), 
  plotSpatialOneBNEParameter(dta = preds.nys.2012, parameter = 'cams_pred', 
                             mainTitle = '2012 CAMS Predictions'), 
  plotSpatialOneBNEParameter(dta = preds.nys.2013, parameter = 'cams_pred', 
                             mainTitle = '2013 CAMS Predictions'), 
  plotSpatialOneBNEParameter(dta = preds.nys.2014, parameter = 'cams_pred', 
                             mainTitle = '2014 CAMS Predictions'), 
  plotSpatialOneBNEParameter(dta = preds.nys.2015, parameter = 'cams_pred', 
                             mainTitle = '2015 CAMS Predictions'), 
  nrow = 3, ncol = 2
)
####********************************************
#### 3: Plot Histograms of CAMS Predictions ####
####********************************************

# 3a Make histograms 


png(here::here(dir.proj, 'outputs', 'a2_histogram.png'), 
    height = 400, width = 1000)
cowplot::plot_grid(
  makeNiceHistogram(preds.conus.2010, 'cams_pred', mainTitle = '2010 CAMS ',
                    xRange = c(0, 124), binCount = 60),
  makeNiceHistogram(preds.conus.2015, 'cams_pred', mainTitle = '2010 CAMS',
                    xRange = c(0, 124), binCount = 60),
  makeNiceHistogram(preds.conus.2010, 'cmaq_outs_pred', mainTitle = '2010 CMAQ Fusion',
                    xRange = c(0, 124), binCount = 60),
  makeNiceHistogram(preds.conus.2015, 'cmaq_outs_pred', mainTitle = '2015 CMAQ Fusion',
                    xRange = c(0, 124), binCount = 60),
  makeNiceHistogram(preds.conus.2010, 'js_pred', mainTitle = '2010 Schwartz Model',
                    xRange = c(0, 124), binCount = 60),
  makeNiceHistogram(preds.conus.2015, 'js_pred', mainTitle = '2015 Schwartz Model',
                    xRange = c(0, 124), binCount = 60),
  
   nrow = 3, ncol = 2)
dev.off()

####*****************************************************
#### 4: Plot Correlations Among Prediction Variables ####
####*****************************************************

# 4a Create the year-specific correlations 
preds.preds.2010 <- preds.conus.2010 %>% 
  dplyr::select(contains('pred')) %>% 
  rename('vonDonkelaar' = av_pred, GBD = gs_pred, 'CMAQ Fusion' = cmaq_outs_pred, 
         'Schwartz Model' = js_pred, CACES = caces_pred, CAMS = cams_pred)
preds.cors.2010 <- cor(preds.preds.2010)

preds.preds.2015 <- preds.conus.2015 %>% 
  dplyr::select(contains('pred')) %>% 
  rename('vonDonkelaar' = av_pred, GBD = gs_pred, 'CMAQ Fusion' = cmaq_outs_pred, 
         'Schwartz Model' = js_pred, CACES = caces_pred, CAMS = cams_pred)
preds.cors.2015 <- cor(preds.preds.2015)
# 4b Average across years
# 4c Plot 
png(here::here(dir.proj, 'outputs', 'a3i_corrs.png'), 
    height = 400, width = 400)
  corrplot::corrplot(preds.cors.2010, type = 'lower', addCoef.col = 'black')
dev.off()

png(here::here(dir.proj, 'outputs', 'a3ii_corrs.png'), 
    height = 400, width = 400)
  corrplot::corrplot(preds.cors.2015, type = 'lower', addCoef.col = 'black')
dev.off()

####**************************
#### # 5: Plot CAMS Error ####
####**************************

# 5a read AQS data 
add_pred_to_aqs <- function(YYYY) {
aqs <- read_csv(here::here('BNE_inputs', 'training_datasets', 'individual_annual', 
                           paste0('training_avgscmjsccca_', YYYY, '_all.csv')))



return(aqs)
}

aqs <- map_dfr(2010:2015, add_pred_to_aqs)

aqs <- aqs %>% filter(lat > min(filter(aqs, year ==2010)$lat) &  
                        lat < max(filter(aqs, year ==2010)$lat) & 
                        lon > min(filter(aqs, year ==2010)$lon) &  
                        lon < max(filter(aqs, year ==2010)$lon))
# remove one spot that is super high 
aqs <- aqs %>% filter(cams_pred < 100)
# 5b compute error 
aqs <- aqs %>% 
  mutate(err_av = av_pred - obs_pm2_5, err_gs = gs_pred - obs_pm2_5, err_cm = cmaq_outs_pred - obs_pm2_5, 
         err_js = js_pred - obs_pm2_5, err_cc =  caces_pred - obs_pm2_5, err_ca = cams_pred - obs_pm2_5)
# 5c aggregate error 
aqs.agg <- aqs %>% 
  group_by(lat, lon) %>% 
  summarize(rmse_av = sqrt(mean(err_av^2)), 
            rmse_gs = sqrt(mean(err_gs^2)),
            rmse_cm = sqrt(mean(err_cm^2)),
            rmse_js = sqrt(mean(err_js^2)),
            rmse_cc = sqrt(mean(err_cc^2)),
            rmse_ca = sqrt(mean(err_ca^2)), 
            me_av = mean(err_av),
            me_gs = mean(err_gs),
            me_cm = mean(err_cm),
            me_js = mean(err_js),
            me_cc = mean(err_cc),
            me_ca = mean(err_ca))
# 5d create year-specific plot 
# combine


errorScale <- c(round(min(aqs$err_ca),2), 10, 35, round(max(aqs$err_ca),2))
  
conus <- st_read(here::here('data_ancillary', 'formatted', 'spatial_outlines', 
                            'conus.shp'))



png(here::here(dir.proj, 'outputs', 'a4_annual_error.png'), 
    height = 600, width = 1200)
cowplot::plot_grid(
  plotSpatialOneBNEParameter(dta = filter(aqs, year == 2010), parameter = 'err_ca', 
                             mainTitle = '2010 CAMS Error', plotOutline = 'plotOutline',
                             valueScale = errorScale, legYN = 'legN'),
  plotSpatialOneBNEParameter(dta = filter(aqs, year == 2011), parameter = 'err_ca', 
                             mainTitle = '2011 CAMS Error', plotOutline = 'plotOutline',
                             valueScale = errorScale, legYN = 'legN'), 
  plotSpatialOneBNEParameter(dta = filter(aqs, year == 2012), parameter = 'err_ca', 
                             mainTitle = '2012 CAMS Error', plotOutline = 'plotOutline',
                             valueScale = errorScale, legYN = 'legN'), 
  plotSpatialOneBNEParameter(dta = filter(aqs, year == 2013), parameter = 'err_ca', 
                             mainTitle = '2013 CAMS Error', plotOutline = 'plotOutline',
                             valueScale = errorScale, legYN = 'legN'),
  plotSpatialOneBNEParameter(dta = filter(aqs, year == 2014), parameter = 'err_ca', 
                             mainTitle = '2014 CAMS Error', plotOutline = 'plotOutline',
                             valueScale = errorScale, legYN = 'legN'), 
  plotSpatialOneBNEParameter(dta = filter(aqs, year == 2015), parameter = 'err_ca', 
                             mainTitle = '2015 CAMS Error', plotOutline = 'plotOutline',
                             valueScale = errorScale, legYN = 'legN'),
  nrow = 2, ncol = 3)
dev.off()
# 5e create plots of average performance
aqs.agg.sf <- aqs.agg %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326")) %>% 
  sf::st_transform(crs=sf::st_crs(projString))

png(here::here(dir.proj, 'outputs', 'a5_agg_error.png'), 
    height = 300, width = 800)
cowplot::plot_grid( 
  plotSpatialOneBNEParameter(dta = aqs.agg, parameter = 'rmse_ca', 
                             mainTitle = 'CAMS RMSE', plotOutline = 'plotOutline',
                              legYN = 'legY'),
  plotSpatialOneBNEParameter(dta = aqs.agg, parameter = 'me_ca', 
                             mainTitle = 'CAMS Mean Error', plotOutline = 'plotOutline',
                             legYN = 'legY'),
  nrow = 1, ncol = 2)
dev.off()

# and a table of the mean SD RMSE by model 


####**************************
#### # 6: Back up slides ####
####**************************

# this will be a backup slide
pdf(here::here(dir.proj, 'outputs', 'a1_prediction_maps.pdf'))
cowplot::plot_grid(
  plotSpatialOneBNEParameter(dta = preds.conus.2010, parameter = 'cams_pred', 
                             mainTitle = '2010 CAMS Predictions'), 
  plotSpatialOneBNEParameter(dta = preds.conus.2011, parameter = 'cams_pred', 
                             mainTitle = '2011 CAMS Predictions'), 
  plotSpatialOneBNEParameter(dta = preds.conus.2012, parameter = 'cams_pred', 
                             mainTitle = '2012 CAMS Predictions'), 
  plotSpatialOneBNEParameter(dta = preds.conus.2013, parameter = 'cams_pred', 
                             mainTitle = '2013 CAMS Predictions'), 
  plotSpatialOneBNEParameter(dta = preds.conus.2014, parameter = 'cams_pred', 
                             mainTitle = '2014 CAMS Predictions'), 
  plotSpatialOneBNEParameter(dta = preds.conus.2015, parameter = 'cams_pred', 
                             mainTitle = '2015 CAMS Predictions'), 
  nrow = 3, ncol = 2
)
dev.off()



##-------------------

YYYY <- 2011
# 1a Set Year
bneOut <- fst::read_fst(here::here(dir.proj, 'BNE_outputs',
                          'BNEoutputs_combined.fst'))

# 1b. restrict to 2011
bneOut <- bneOut %>% 
  dplyr::filter(stringr::str_detect(run_id, as.character(YYYY)))

# 1c Wrangle predictions
# 1c.i Convert to simple features
bneOut <- bneOut %>% 
  sf::st_as_sf(., coords = c("lon", "lat"), crs=st_crs('epsg:4326')) %>% 
  sf::st_transform(crs=st_crs(projString))

####*************************
#### 2: Plot Predictions ####
####*************************

png(here::here(dir.proj, 'outputs', 'describe_annual_bne',
               paste0('Input_distribution_', YYYY, '.png')))
plotSpatialOneBNEParameter(bneOut, 'w_mean_av')
dev.off()
#cowplot::plot_grid(
  #plotSpatialOneBNEParameter(bneOut, 'w_mean_av'),
  #nrow = 3)



bneOut <- readBNEoutput(2010, inputSet, 3.5, 'all')

pred.in <- readr::read_csv(here::here('BNE_inputs', 'prediction_datasets', 'individual_annual', 
                                      paste0('predictions_avgscmjscc_', 2010, '_all.csv')))

bneOut <- bind_cols(bneOut, pred.in)

bneOut <- bneOut %>% 
  mutate(pred_mean = (1/5) *(av_pred + gs_pred + cmaq_outs_pred + js_pred + caces_pred)) %>% 
  mutate(diffsq = (av_pred-pred_mean)^2 + 
           (gs_pred-pred_mean)^2 +
           (cmaq_outs_pred-pred_mean)^2 +
           (js_pred-pred_mean)^2 +
           (caces_pred-pred_mean)^2) %>% 
  mutate(sd_inputs = sqrt((1/5)*diffsq))
