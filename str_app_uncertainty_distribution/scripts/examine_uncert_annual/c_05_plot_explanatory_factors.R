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

# 0a Load package required for this script
if(!exists("Ran_0_00")){
  # change this to something .md when I can 
  #here::i_am("README.rtf")
  source(here::here('Scripts', 'fastUncertaintyAnalysis_ISEE',
                    "0_00_setUp_env.R"))
}

####*******************************************
#### 1: Fit Simple Linear Regression Model ####
####*******************************************

# 1a Readin Data
dta <- read_fst(here::here('Data', 'Processed', 'CombinedDataforISEE.fst'))

dta.sf <- baseGrid %>% 
  inner_join(dta, by = 'gridID')


# 1b Readin Conus shapefile
conus <- st_read(here::here('Data', 'Processed', 'Conus', 'conus.shp'))

# 1c plot

fillScheme <- viridis::scale_fill_viridis(direction = 1, 
                                          guide = "colorbar")
colorScheme <- viridis::scale_color_viridis(direction = 1, 
                                            guide = "colorbar")
png(here::here('Outputs', 'ISEE2021', 'uncert_plot.png'))
ggplot(conus) + 
  geom_sf()  + 
  geom_sf(data = dta.sf, aes(fill= pred_uncert, 
                              color = pred_uncert)) +
  fillScheme + colorScheme +
  theme(plot.title = element_text(size = 18)) + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  ggtitle("2015 Uncertainty")
dev.off()


