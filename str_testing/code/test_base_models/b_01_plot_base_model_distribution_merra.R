# Plot Distribution of prediction models
# Examine CAMS
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Wrangle Predictions 
# 2: Plot MERRA Predictions 
# 3: Plot MERRA Error 
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
  source(here::here('scripts', 'a_set_up', "a_00_config_env.R"))
}

if(!exists("Ran_a_00_uncert")){
  here::i_am("README.md")
  source(here::here('str_uncert_analysis', 'code', 
                    "0_00_config_env_uncert_analysis.R"))
}

dir.proj <- 'str_testing'

####****************************
#### 1: Wrangle Predictions ####
####****************************

# 1a Define function to wrangle predictions 
read_predSet_merra <- function(YYYY, fold) {
  readr::read_csv(here::here('BNE_inputs', 'prediction_datasets', 'individual_annual', 
                                        paste0('predictions_avgscmjsccme_', YYYY, 
                                               '_', fold, '.csv')))
  
}

# 1b Readin prediction datasets
preds.conus.allYYYY <- bind_rows(
  read_predSet_merra(2010, 'all'))


preds.conus.allYYYY  <- preds.conus.allYYYY # %>% 
  #slice_sample(prop = 0.1)

preds.conus.2010 <- preds.conus.allYYYY %>% filter(time == 2010)

# repeat for NYS
preds.nys.2010 <- read_predSet_merra(2010, 'NYS')
preds.nys.NYSYYYY <- bind_rows(
  read_predSet_merra(2010, 'NYS'))




####******************************
#### 2: Plot MERRA Predictions ####
####******************************
 
# 2a CONUS 
# 2a.i create vector of concentration values 
predMean.vec <- c(preds.conus.2010$merra_pred, preds.conus.2010$js_pred, 
                  preds.conus.2010$cmaq_outs_pred)
p.min <- min(predMean.vec); p.max <- max(predMean.vec)
# manually set to account for log scale of colors
p.1 <- 6; p.2 = 12; p.3 = 18
valueVec.wSD <- c(round(p.min,2), round(p.1,2),  round(p.2,2),
                  round(p.3,2), round(p.max,2))

# 2a.ii make plot
png(here::here(dir.proj, 'outputs_merra', 'a1a_predictions_conus_2010_2015.png'), 
    height = 600, width = 600)
cowplot::plot_grid(
 plotOneParameterSpatial(dta = preds.conus.2010, parameter = 'merra_pred', 
                             mainTitle = '2010 MERRA Predictions', 
                             valueScale = valueVec.wSD, legYN = 'legY'),  
  plotOneParameterHist(preds.conus.2010, 'merra_pred', mainTitle = '2010 MERRA ',
                    xRange = c(0, 25), binCount = 60),
  nrow = 2, ncol = 1, rel_heights = 4:2)
dev.off()

# 2b NYS 
# 2a.i create vector of concentration values 
predMean.vec <- c(preds.nys.2010$cmerra_pred, preds.nys.2010$js_pred, 
                  preds.nys.2010$cmaq_outs_pred)
p.min <- min(predMean.vec); p.max <- max(predMean.vec)
# manually set to account for log scale of colors
p.1 <- 10; p.2 = 20; p.3 <- 25
valueVec.wSD <- c(round(p.min,2), round(p.1,2),  round(p.2,2),
                  p.3, 35)
# 2b.ii make plot
png(here::here(dir.proj, 'outputs_merra', 'a1b_predictions_nys_2010_2015.png'), 
    height = 600, width = 600)
cowplot::plot_grid(
 plotOneParameterSpatial(dta = preds.nys.2010, parameter = 'merra_pred', 
                             mainTitle = '2010 MERRA Predictions'), 
  plotOneParameterHist(preds.nys.2010, 'merra_pred', mainTitle = '2010 MERRA ',
                    xRange = c(0, 25), binCount = 60),
  nrow = 2, ncol = 1, rel_heights = 4:2)
dev.off()


####************************
#### 3: Plot MERRA Error ####
####************************

# 3a Read AQS data 
# 3a.i function to readin aqs
add_pred_to_aqs <- function(YYYY) {
  aqs <- read_csv(here::here('BNE_inputs', 'training_datasets', 'individual_annual', 
                             paste0('training_avgscmjsccme_', YYYY, '_all.csv')))
  return(aqs)
}
# 3a.ii bring in all the aqs data
aqs <- map_dfr(2010, add_pred_to_aqs)
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
         err_js = js_pred - obs_pm2_5, err_cc =  caces_pred - obs_pm2_5, err_me = merra_pred - obs_pm2_5)

# 3c aggregate error 
aqs.agg <- aqs %>% 
  group_by(lat, lon) %>% 
  summarize(rmse_av = sqrt(mean(err_av^2)), 
            rmse_gs = sqrt(mean(err_gs^2)),
            rmse_cm = sqrt(mean(err_cm^2)),
            rmse_js = sqrt(mean(err_js^2)),
            rmse_cc = sqrt(mean(err_cc^2)),
            rmse_me = sqrt(mean(err_me^2)), 
            me_av = mean(err_av),
            me_gs = mean(err_gs),
            me_cm = mean(err_cm),
            me_js = mean(err_js),
            me_cc = mean(err_cc),
            me_me = mean(err_me))

# 3e create error scale
errorScale <- c(round(min(aqs$err_me),2), 0, 5, 10, 15)

# 3f bring in conus outline shapefile
conus <- st_read(here::here('data_ancillary', 'formatted', 'spatial_outlines', 
                            'conus.shp'))

# 3g create conus plot
png(here::here(dir.proj, 'outputs', 'test_annual_merra','a2b_annual_error_conus.png'), 
    height = 600, width = 600)
cowplot::plot_grid(
 plotOneParameterSpatial(dta = filter(aqs, year == 2010), parameterName = 'err_me', 
                             mainTitle = '2010 MERRA Error', borderObj = conus,
                             valueScale = errorScale, legYN = 'legY'),
  plotOneParameterHist(filter(aqs, year == 2010), 'err_me', mainTitle = '2010 MERRA Error',
                    valueRange = c(-15, 20), binWidth = 1),
  nrow = 2, ncol = 1, rel_heights = 4:2)
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
png(here::here(dir.proj, 'outputs_merra', 'a2b_annual_error_nys.png'), 
    height = 600, width = 600)
cowplot::plot_grid(
 plotOneParameterSpatial(dta = filter(aqs.nys, year == 2010), parameter = 'err_me', 
                             mainTitle = '2010 MERRA Error', plotOutline = nys,
                             valueScale = errorScale, legYN = 'legY', pointSize = 2),
  plotOneParameterHist(filter(aqs.nys, year == 2010), 'err_me', mainTitle = '2010 MERRA Error',
                    xRange = c(-15, 20), binCount = 60),
  nrow = 2, ncol = 1, rel_heights = 4:2)
dev.off()


# Look at relative ranking of models. 
# if a model isn't in the top half of ranking (by accuracy) for at least 5% of observations 
# we would throw it out. We use median for ranking; mean is too dependent on 
# which models are included; an extreme model can throw off the mean absolute error
aqs <- aqs %>% 
  rename(obs_pm25 = obs_pm2_5, pred_me = merra_pred, pred_av = av_pred, pred_gs = gs_pred, 
         pred_cm = cmaq_outs_pred, pred_js = js_pred, pred_cc = caces_pred) %>%
  dplyr::select(-starts_with('ae')) %>%
  mutate(ae_me = abs(obs_pm25 - pred_me), ae_av = abs(obs_pm25 - pred_av), 
         ae_gs = abs(obs_pm25 - pred_gs), ae_cm = abs(obs_pm25 - pred_cm), 
         ae_js = abs(obs_pm25 - pred_js), ae_cc = abs(obs_pm25 - pred_cc)) 
aqs$ae_median <- apply(dplyr::select(aqs,starts_with('ae')), 1, median)
aqs <- aqs %>% 
  mutate(below_median_ae_me = if_else(ae_me <= ae_median, 1, 0),
         below_median_ae_av = if_else(ae_av <= ae_median, 1, 0),
         below_median_ae_gs = if_else(ae_gs <= ae_median, 1, 0),
         below_median_ae_cm = if_else(ae_cm <= ae_median, 1, 0), 
         below_median_ae_js = if_else(ae_js <= ae_median, 1, 0), 
         below_median_ae_cc = if_else(ae_cc <= ae_median, 1, 0))

# compute percentage adding info, full and by season 
percentMEaddInfo.annual <- 100*round(mean(aqs$below_median_ae_me), 2)
percentAVaddInfo.annual <- 100*round(mean(aqs$below_median_ae_av), 2)
percentGSaddInfo.annual <- 100*round(mean(aqs$below_median_ae_gs), 2)
percentCMaddInfo.annual <- 100*round(mean(aqs$below_median_ae_cm), 2)
percentJSaddInfo.annual <- 100*round(mean(aqs$below_median_ae_js), 2)
percentCCaddInfo.annual <- 100*round(mean(aqs$below_median_ae_cc), 2)






####***************************************
#### 4: Histogram of Prediction Models ####
####***************************************

png(here::here(dir.proj, 'outputs_merra', 'a3_histogram.png'), 
    height = 400, width = 500)
cowplot::plot_grid(
  plotOneParameterHist(preds.conus.2010, 'merra_pred', mainTitle = '2010 MERRA ',
                    xRange = c(0, 25), binCount = 60),
  plotOneParameterHist(preds.conus.2010, 'cmaq_outs_pred', mainTitle = '2010 CMAQ Fusion',
                    xRange = c(0, 25), binCount = 60),
  plotOneParameterHist(preds.conus.2010, 'js_pred', mainTitle = '2010 Schwartz Model',
                    xRange = c(0, 25), binCount = 60),

  
  nrow = 3, ncol = 1)
dev.off()

####***********************************************
#### 5: Table of Prediction Model Distribution ####
####***********************************************

meanPM <- data.frame(mean= c(mean(preds.conus.2010$merra_pred), 
                             mean(preds.conus.2010$av_pred),
                             mean(preds.conus.2010$gs_pred),
                             mean(preds.conus.2010$cmaq_outs_pred),
                             mean(preds.conus.2010$js_pred),
                             mean(preds.conus.2010$caces_pred)), 
                     sd= c(sd(preds.conus.2010$merra_pred), 
                           sd(preds.conus.2010$av_pred),
                           sd(preds.conus.2010$gs_pred),
                           sd(preds.conus.2010$cmaq_outs_pred),
                           sd(preds.conus.2010$js_pred),
                           sd(preds.conus.2010$caces_pred)), 
                     min= c(min(preds.conus.2010$merra_pred), 
                            min(preds.conus.2010$av_pred),
                            min(preds.conus.2010$gs_pred),
                            min(preds.conus.2010$cmaq_outs_pred),
                            min(preds.conus.2010$js_pred),
                            min(preds.conus.2010$caces_pred)), 
                     q1= c(quantile(preds.conus.2010$merra_pred, 0.25), 
                           quantile(preds.conus.2010$av_pred, 0.25),
                           quantile(preds.conus.2010$gs_pred, 0.25),
                           quantile(preds.conus.2010$cmaq_outs_pred, 0.25),
                           quantile(preds.conus.2010$js_pred, 0.25),
                           quantile(preds.conus.2010$caces_pred, 0.25)),
                     median= c(median(preds.conus.2010$merra_pred), 
                               median(preds.conus.2010$av_pred),
                               median(preds.conus.2010$gs_pred),
                               median(preds.conus.2010$cmaq_outs_pred),
                               median(preds.conus.2010$js_pred),
                               median(preds.conus.2010$caces_pred)),
                     q3= c(quantile(preds.conus.2010$merra_pred, 0.75), 
                           quantile(preds.conus.2010$av_pred, 0.75),
                           quantile(preds.conus.2010$gs_pred, 0.75),
                           quantile(preds.conus.2010$cmaq_outs_pred, 0.75),
                           quantile(preds.conus.2010$js_pred, 0.75),
                           quantile(preds.conus.2010$caces_pred, 0.75)),
                     max= c(max(preds.conus.2010$merra_pred), 
                            max(preds.conus.2010$av_pred),
                            max(preds.conus.2010$gs_pred),
                            max(preds.conus.2010$cmaq_outs_pred),
                            max(preds.conus.2010$js_pred),
                            max(preds.conus.2010$caces_pred)))

####**********************************************
#### 6: Table of Prediction Model Performance ####
####**********************************************

meanPerformance <- data.frame(
  meanRMSE = c(mean(aqs.agg$rmse_me), 
                             mean(aqs.agg$rmse_av),
                             mean(aqs.agg$rmse_gs),
                             mean(aqs.agg$rmse_cm),
                             mean(aqs.agg$rmse_js),
                             mean(aqs.agg$rmse_cc)), 
                     sdRMSE = c(sd(aqs.agg$rmse_me), 
                                sd(aqs.agg$rmse_av),
                                sd(aqs.agg$rmse_gs),
                                sd(aqs.agg$rmse_cm),
                                sd(aqs.agg$rmse_js),
                                sd(aqs.agg$rmse_cc)), 
                     meanME = c(mean(aqs.agg$me_me), 
                                  mean(aqs.agg$me_av),
                                  mean(aqs.agg$me_gs),
                                  mean(aqs.agg$me_cm),
                                  mean(aqs.agg$me_js),
                                  mean(aqs.agg$me_cc)), 
                     sdME = c(sd(aqs.agg$me_me), 
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
  data.frame(corr = c(cor(aqsYYYY$obs_pm2_5, aqsYYYY$merra_pred), 
                      cor(aqsYYYY$obs_pm2_5, aqsYYYY$av_pred),
                      cor(aqsYYYY$obs_pm2_5, aqsYYYY$gs_pred),
                      cor(aqsYYYY$obs_pm2_5, aqsYYYY$cmaq_outs_pred),
                      cor(aqsYYYY$obs_pm2_5, aqsYYYY$js_pred),
                      cor(aqsYYYY$obs_pm2_5, aqsYYYY$caces_pred))) %>% 
    mutate(!!varName := round(corr, 2)) %>% 
    dplyr::select(-corr)
}
# 7b fill in table
corTable <- map(2010, calc_corr_aqs) %>%
  bind_cols()

 ####********************************************
#### 8: Correlation Among Prediction Models ####
####********************************************

# 4a Create the year-specific correlations 
preds.preds.2010 <- preds.conus.2010 %>% 
  dplyr::select(contains('pred')) %>% 
  rename('vonDonkelaar' = av_pred, GBD = gs_pred, 'CMAQ Fusion' = cmaq_outs_pred, 
         'Schwartz Model' = js_pred, CACES = caces_pred, MERRA = merra_pred)
preds.cors.2010 <- cor(preds.preds.2010)

# 4b Average across years
# 4c Plot 
png(here::here(dir.proj, 'outputs_merra', 'a4i_corrs.png'), 
    height = 400, width = 400)
corrplot::corrplot(preds.cors.2010, type = 'lower', addCoef.col = 'black')
dev.off()


