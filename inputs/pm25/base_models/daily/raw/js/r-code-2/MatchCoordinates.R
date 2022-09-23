#####################################################################################
# Code: match lat/lon coordinates with predictions in RDS files                                           
# Author: Yaguang Wei1                                    
#  1 Harvard T.H. Chan School of Public Health, 
#    Harvard University
#####################################################################################

### For the data in RDS format, the latitude and longitude of each 1-km grid cell were specified in USGridSite.rds. 
### MatchCoordinates.R: sample R code to match lat/lon coordinates to PM2.5 prediction of each 1-km grid cell.

# Each RDS file is a vector with 11218022 elements, and the USGridSite.rds is a data frame with 
# 11218022 rows and 3 variables.

# The elements in the vector and the rows in the data frame have the same length and are in the 
# same order, meaning that you can simply bind them by columns.

# sample code
sitecode <- readRDS('~/USGridSite.rds')
pm_20000101 <- readRDS('~/PredictionStep2_PM25_USGrid_20000101_20000101.rds')
pm_20000101 <- data.frame(t(pm_20000101))
pm_20000101 <- cbind(sitecode,pm_20000101)