# Compare Aditya and Lawrence Location Data
# BNE Crude Error Assessment 
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Readin Annual Training Data from Aditya 
# 2: Readin EPA Data from Lawrence
# 3: Compare LatLon

####********************
#### 0: Preparation ####
####********************

# 0a Load packages
library(tidyverse)
library(purrr)
library(rgdal)
library(tmap)

####************************************************
#### 1: Readin Annual Training Data from Aditya ####
####************************************************

# 1a Readin data
dta <-  read_fst(here::here('Data', 'processed', 
                            'AVGSCMJSCCM14_3.5_explanatory.fst'))

dta <- dta %>%   
  st_as_sf(., coords = c("lon", "lat"), crs=st_crs('epsg:4326')) %>% 
  st_transform(., crs=st_crs(projString))

dta <- dta %>% 
  left_join(BG1, by = 'gridID') %>% 
  mutate(MonitorCount = if_else(is.na(MonitorCount), 0, MonitorCount))


print_expDist <- function(VarName){
pdf(here::here('Results_Outputs', 'uncertainty_analysis',
               paste0('explaninDist_', VarName, '.pdf')))
cowplot::plot_grid(
 plotSpatial_oneParameter(dta, VarName, ' ', 'LegN'),
 plotHist_oneParameter(dta, VarName), 
 ncol = 1)
dev.off()
}

VarList <- c('summerTemp', 'winterTemp', 'popD', 'monitorOverlap', 
             'pred_mean', 'pred_uncert')


for (i in 1:length(VarList)){
 VarName <- VarList[i]
  
  png(here::here('Results_Outputs', 'uncertainty_analysis',
                 paste0('explaninDist_', VarName, '.png')))
  cowplot::plot_grid(
    plotSpatial_oneParameter(dta, VarName, ' ', 'LegN'),
    plotHist_oneParameter(dta, VarName), 
    ncol = 1)
  dev.off()
  
}


VarName <- VarList[1]
png(here::here('Results_Outputs', 'uncertainty_analysis',
               paste0('explaninDist_', VarName, '.png')))
cowplot::plot_grid(
  plotSpatial_oneParameter(dta, VarName, ' ', 'LegY'),
  plotHist_oneParameter(dta, VarName), 
  ncol = 1)
dev.off()


VarName <- VarList[2]
png(here::here('Results_Outputs', 'uncertainty_analysis',
               paste0('explaninDist_', VarName, '.png')))
cowplot::plot_grid(
  plotSpatial_oneParameter(dta, VarName, ' ', 'LegY'),
  plotHist_oneParameter(dta, VarName), 
  ncol = 1)
dev.off()


VarName <- VarList[3]
png(here::here('Results_Outputs', 'uncertainty_analysis',
               paste0('explaninDist_', VarName, '.png')))
cowplot::plot_grid(
  plotSpatial_oneParameter(dta, VarName, ' ', 'LegY'),
  plotHist_oneParameter(dta, VarName), 
  ncol = 1)
dev.off()

VarName <- VarList[4]
png(here::here('Results_Outputs', 'uncertainty_analysis',
               paste0('explaninDist_', VarName, '.png')))
cowplot::plot_grid(
  plotSpatial_oneParameter(dta, VarName, ' ', 'LegY'),
  plotHist_oneParameter(dta, VarName), 
  ncol = 1)
dev.off()

VarName <- VarList[5]
png(here::here('Results_Outputs', 'uncertainty_analysis',
               paste0('explaninDist_', VarName, '.png')))
cowplot::plot_grid(
  plotSpatial_oneParameter(dta, VarName, ' ', 'LegY'),
  plotHist_oneParameter(dta, VarName), 
  ncol = 1)
dev.off()

VarName <- VarList[6]
png(here::here('Results_Outputs', 'uncertainty_analysis',
               paste0('explaninDist_', VarName, '.png')))
cowplot::plot_grid(
  plotSpatial_oneParameter(dta, VarName, ' ', 'LegY'),
  plotHist_oneParameter(dta, VarName), 
  ncol = 1)
dev.off()

VarName <- 'MonitorCount'
png(here::here('Results_Outputs', 'uncertainty_analysis',
               paste0('explaninDist_', VarName, '.png')))
cowplot::plot_grid(
  plotSpatial_oneParameter(dta, VarName, ' ', 'LegY'),
  plotHist_oneParameter(dta, VarName), 
  ncol = 1)
dev.off()
