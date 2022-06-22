# File: STR_1_summarizeBNErun.R
# Author: Sebastian T. Rowland <sr3463@cumc.columbia.edu>
# Date: 12/21/2021

# this function can accept BNE run from any BNE model 
# note that the legend scales will be specific to this run, but 
# similar parameters (e.g., the weights) will follow the same scale

#' \code{summarizeBNErun} creates a pdf with many plots to summarize the BNE results 
#' Useful for reviewing model for main findings and coherent results
#' 
#' @param bneOut a dataframe of the BNE outputs
#' @param location string determining the location to plot. For now used to plot
#' specific cities 
#' @param targetDir string of directory to save the results
#' @param targetFile string of file name, 'default targetFile' uses the runID
#' @param dropData string determining if you drop 75% of the data to quickly generate the plot
#' helpful for conus-wide results which can take ~ 3 minutes to print
#' 
#' @return a printed pdf with many plots showing the bne results
#' 
#' @export
#' @importFrom magrittr %>%
#' 

summarizeBNErun <- function(bneOut,
                          location = 'conus', 
                          targetDir, 
                          targetFile = 'default targetFile', 
                          dropData = FALSE){
  
  #-------------------------------#
  #### 1. wrangle BNE outputs: ####
  #-------------------------------#
  
  # 1a. drop 50% of observations to speed up plotting 
  if(dataDrop) {
    bneOut <- bneOut %>% 
     dplyr::slice_sample(prop = 0.25)
  }

  # 1b. restict to city, if relevant
  # 1b.i. first, identify which city each point belongs to 
  bneOut <- bneOut %>% 
    dplyr::mutate(city = case_when(
      lon < -100 ~ 'LA', 
      lon > -100 & lon < -85 ~ 'Chicago', 
      lat > 41.4 & lon > -73 ~ 'Boston',
      lon > -80 ~ 'NYC', 
      TRUE ~ 'no city')) 
  # 1b.ii now restrict
  if (location %in% bneOut$city) {bneOut <- bneOut %>% dplyr::filter(city == !!location)}
    
  # 1c. compute distance of weights from mean 
  # relevant for testing, but will not be relevant once BNE is stabilized
  bneOut <- bneOut %>% 
    dplyr::mutate(w_dist_av = abs(w_mean_av - 0.2), 
           w_dist_gs = abs(w_mean_gs - 0.2),
           w_dist_cm = abs(w_mean_cm - 0.2),
           w_dist_js = abs(w_mean_js - 0.2),
           w_dist_cc = abs(w_mean_cc - 0.2))
  
  # 1d. read AQS monitors 
  # we restrict them to the year and area we are plotting
  aqs <- readr::read_csv(here::here('BNE_inputs', 'training_datasets', 'combined_annual', 
                             'training_avgscmjscc_all.csv')) %>% 
    dplyr::filter(year == YYYY) %>% 
    dplyr::filter(lat > min(bneOut$lat) & lat < max(bneOut$lat) & 
             lon > min(bneOut$lon) & lon < max(bneOut$lon))
  
  #----------------------------------------#
  #### 2. create BNE-summarizing plots: ####
  #----------------------------------------#
  
  # 2a Begin PDF
  if(targetFile == 'default targetFile') {targetFile <- bneOut$run_id[1]}
  
  pdf(here::here(targetDir,
                 paste0(targetFile, '.pdf')))
  
  # 2b weights
  cowplot::plot_grid(
    plotOneParameterSpatial(dta = bneOut, parameter = 'w_mean_av',
                               mainTitle = 'Weight of Aaron Von Donkelaar', pointSize = 2.0,
                               plotAQS = aqs), 
    plotOneParameterSpatial(dta = bneOut, parameter = 'w_mean_gs', 
                               mainTitle = 'Weight of Global Burden of Disease', pointSize = 2.0,
                               plotAQS = aqs),
    plotOneParameterSpatial(dta = bneOut, parameter = 'w_mean_cm',
                               mainTitle = 'Weight of CMAQ-Fusion', pointSize = 2.0,
                               plotAQS = aqs),
    plotOneParameterSpatial(dta = bneOut, parameter = 'w_mean_js', 
                               mainTitle = 'Weight of Joel Schwartz 2019', pointSize = 2.0,
                               plotAQS = aqs),
    plotOneParameterSpatial(dta = bneOut, parameter = 'w_mean_cc', 
                               mainTitle = 'Weight of CACES', pointSize = 2.0,
                               plotAQS = aqs),
    ncol = 2, nrow = 3)
  
  cowplot::plot_grid(
    plotOneParameterSpatial(dta = bneOut, parameter = 'w_dist_av',
                               mainTitle = 'Weight of Aaron Von Donkelaar Relative to Prior', pointSize = 2.0,
                               plotAQS = aqs, valueScale = seq(0.1,0.8, 0.1)), 
    plotOneParameterSpatial(dta = bneOut, parameter = 'w_dist_gs', 
                               mainTitle = 'Weight of Global Burden of Disease Relative to Prior', pointSize = 2.0,
                               plotAQS = aqs, valueScale = seq(0.1,0.8, 0.1)),
    plotOneParameterSpatial(dta = bneOut, parameter = 'w_dist_cm',
                               mainTitle = 'Weight of CMAQ-Fusion Relative to Prior', pointSize = 2.0,
                               plotAQS = aqs, valueScale = seq(0.1,0.8, 0.1)),
    plotOneParameterSpatial(dta = bneOut, parameter = 'w_dist_js', 
                               mainTitle = 'Weight of Joel Schwartz 2019 Relative to Prior', pointSize = 2.0,
                               plotAQS = aqs, valueScale = seq(0.1,0.8, 0.1)),
    plotOneParameterSpatial(dta = bneOut, parameter = 'w_dist_cc', 
                               mainTitle = 'Weight of CACES Relative to Prior', pointSize = 2.0,
                               plotAQS = aqs, valueScale = seq(0.1,0.8, 0.1)),
    ncol = 2, nrow = 3)
  
  vec.wSD <- c(bne.out$w_sd_av, bne.out$w_sd_gs, bne.out$w_sd_cm, bne.out$w_sd_js, bne.out$w_sd.cc)
  p.min <- round(min(dta$p), 2); p.max <- round(max(dta$p), 2)
  valueVec.wSD  <- c(p.min, 
                     round(p.min + 0.25*(p.max - p.min), 2), 
                     round(p.min + 0.50*(p.max - p.min), 2), 
                     round(p.min + 0.75*(p.max - p.min), 2), 
                     p.max)
  
  cowplot::plot_grid(
    plotOneParameterSpatial(dta = bneOut, parameter = 'w_sd_av',
                               mainTitle = 'Uncertainty of Weight of Aaron Von Donkelaar', 
                               valueScale = valueVec.wSD, pointSize = 2.0,
                               plotAQS = aqs), 
    plotOneParameterSpatial(dta = bneOut, parameter = 'w_sd_gs', 
                               mainTitle = 'Uncertainty of Weight of Global Burden of Disease', 
                               valueScale = valueVec.wSD, pointSize = 2.0,
                               plotAQS = aqs),
    plotOneParameterSpatial(dta = bneOut, parameter = 'w_sd_cm',
                               mainTitle = 'Uncertainty of Weight of CMAQ-Fusion', 
                               valueScale = valueVec.wSD, pointSize = 2.0,
                               plotAQS = aqs),
    plotOneParameterSpatial(dta = bneOut, parameter = 'w_sd_js', 
                               mainTitle = 'Uncertainty of Weight of Joel Schwartz 2019', 
                               valueScale = valueVec.wSD, pointSize = 2.0,
                               plotAQS = aqs),
    plotOneParameterSpatial(dta = bneOut, parameter = 'w_sd_cc', 
                               mainTitle = 'Uncertainty of Weight of CACES', 
                               valueScale = valueVec.wSD, pointSize = 2.0,
                               plotAQS = aqs),
    ncol = 2, nrow = 3)
  
  cowplot::plot_grid(
    plotOneParameterSpatial(dta = bneOut, parameter = 'ens_mean', 
                               mainTitle = 'Model Combination', pointSize = 2.0,
                               plotAQS = aqs),
    plotOneParameterSpatial(dta = bneOut, parameter = 'ens_sd',
                               mainTitle = 'Uncertainty of Model Combination', pointSize = 2.0,
                               plotAQS = aqs), 
    ncol = 1, nrow = 2)
  
  cowplot::plot_grid(
    plotOneParameterSpatial(dta = bneOut, parameter = 'res_mean', 
                               mainTitle = 'Estimated Residual Process', pointSize = 2.0,
                               plotAQS = aqs),
    plotOneParameterSpatial(dta = bneOut, parameter = 'res_sd',
                               mainTitle = 'Uncertainty of Residual Process', pointSize = 2.0,
                               plotAQS = aqs), 
    ncol = 1, nrow = 2)
  
  print(cowplot::plot_grid(
    plotOneParameterSpatial(dta = bneOut, parameter = 'pred_mean', pointSize = 2.0),
    plotOneParameterSpatial(dta = bneOut, parameter = 'pred_sd', pointSize = 2.0, plotAQS = aqs), 
    ncol = 1, nrow = 2))
  dev.off()
}
