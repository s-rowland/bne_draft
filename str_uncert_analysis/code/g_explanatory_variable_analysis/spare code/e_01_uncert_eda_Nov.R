# Plot BNE Outputs
# Examine BNE Outputs
# BNE Uncertainty Analysis 
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 
# Last updated Oct 24, 2021

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Wrangle BNE Outputs
# 2: Uncertainty by Region
# 3: Uncertainty by State
# 4: Time Trends 
# 5: Scaled Uncertainty

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_set_up_env.R"))
}

if(!exists("Ran_a_00_uncert")){
  here::i_am("README.md")
  source(here::here('str_uncert_analysis', 'code', 
                    "0_00_set_up_env_uncert_analysis.R"))
}

####*****************************
#### 1: Wrangle BNE Outputs ####
####****************************

# 1c Read EPA state 
readCombinedData <- function(YYYY){
  read_fst(here::here(dir.proj, 'data', 'processed', 
                      paste0('AVGSCMJSCC_3.5_explanatory_', YYYY, '_025deg.fst'))) %>% 
    mutate(YYYY = YYYY)
}

bne.out <- map_dfr(2010:2015, readCombinedData) 
rm(readCombinedData)

bne.out <- bne.out %>% distinct()

bne.out <- bne.out %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs(projString)) %>%
  sf::st_transform(crs=sf::st_crs("epsg:4326"))

bne.out <- bne.out %>% 
  mutate(lon = st_coordinates(bne.out)[,1], 
         lat = st_coordinates(bne.out)[,2]) %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

####******************************
#### 2: Add 2.5 predictions ####
####******************************

  
addPred <- function(YYYY){
  
  pred <- readBNEoutput(YYYY, lenScaleSpace = 2.5)

  pred <- pred %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326")) %>%
    sf::st_transform(crs=sf::st_crs(projString)) %>%
    sf::st_transform(crs=sf::st_crs("epsg:4326"))
  
  pred <- pred %>% 
    mutate(lon = st_coordinates(pred)[,1], 
           lat = st_coordinates(pred)[,2]) %>% 
    as.data.frame() %>% 
    dplyr::select(-geometry)
  
  pred <- pred %>% 
    mutate(lat = as.character(lat), 
           lon = as.character(lon)) %>% 
    mutate(TEST = 'test')
  
  dta.yyyy <- bne.out %>% 
    dplyr::select(-pred_mean, -pred_sd) %>%
    mutate(lat = as.character(lat), 
           lon = as.character(lon)) %>%
    filter(YYYY == !!YYYY) %>% 
    inner_join(pred, by = c('lat', 'lon'))
  
  return(dta.yyyy)
}

bne.out <- map(2010:2015, addPred) %>% bind_rows()

ls(bne.out)


####*********************************
#### 2: Add State and EPA Region ####
####*********************************

# 2a Read state data 
states <- read_sf(here::here('data_ancillary', 'raw', 'Census','cb_2015_us_state_500k', 
                             'cb_2015_us_state_500k.shp'))

# 2b Clean up state data 
states <- states %>% 
  rename(state = STUSPS) %>%
  mutate(state_num = as.numeric(STATEFP)) %>% 
  dplyr::select(-AFFGEOID, -ALAND, -AWATER, -GEOID, -LSAD, -NAME, 
                -STATEFP, -STATENS)

# 2c Read EPA region table
epaRegion <- read_csv(here::here('data_ancillary', 'generated', 'epa_regions.csv'))

# 2d Combine 
states  <- states %>% 
  inner_join(epaRegion, by = 'state')

# 2e Convert projection 
states <- states %>% 
  sf::st_transform(crs=st_crs(projString))

# 2f make table for region 
uncert.region <- bne.out %>% 
  group_by(region) %>% 
  summarize(meanUncert_region = mean(pred_sd), 
            sdUncert_region = sd(pred_sd)) %>% 
  arrange(desc(meanUncert_region))

# 2g make table for state 
uncert.state <- bne.out %>% 
  group_by(state) %>% 
  summarize(meanUncert_state = mean(pred_sd), 
            sdUncert_state = sd(pred_sd)) %>% 
  arrange(desc(meanUncert_state))

# 2g combine
states <- states %>% 
  inner_join(uncert.region, by = 'region') %>% 
  inner_join(uncert.state, by = 'state')

# 2h make categorical variables 
states <- states %>% 
  arrange(desc(meanUncert_state)) %>% 
  mutate(Category = if_else(row_number() <= 5, 'Top 5', 'Other'))

png(here::here(dir.proj, 'outputs', 'comm_mtg',
               paste0('e1_mean_uncert_state.png')), 
    height = 300, width = 400)
ggplot() + 
  geom_sf(data = states, aes(fill = meanUncert_state, colour = Category)) + 
  scale_color_manual(values = c(NA, 'white')) + 
  viridis::scale_fill_viridis(direction = -1, option = 'magma') + 
  ggplot2::theme_void() + 
  ggplot2::labs(x='', y='') +
  ggplot2::guides(fill = guide_colorbar(title = 'Mean \n Uncertainty'), 
                  color = 'none')  
dev.off()

png(here::here(dir.proj, 'outputs', 'comm_mtg',
               paste0('e2_mean_uncert_region.png')), 
    height = 300, width = 400)
ggplot() + 
  geom_sf(data = states, aes(fill = meanUncert_region), colour = NA) + 
  #scale_color_manual(values = c(NA, 'white')) + 
  viridis::scale_fill_viridis(direction = -1, option = 'magma') + 
  ggplot2::theme_void() + 
  ggplot2::labs(x='', y='') +
  ggplot2::guides(fill = guide_colorbar(title = 'Mean \n Uncertainty'), 
                  color = 'none')  
dev.off()
####********************
#### 4: Time Trends ####
####********************

# 2f make table for region by year 
uncert.region.yyyy <- bne.out %>% 
  group_by(region, YYYY) %>% 
  summarize(meanUncert_region = mean(pred_sd), 
            sdUncert_region = sd(pred_sd)) %>% 
  arrange(desc(meanUncert_region))

getTimeSlope <- function(areaOfInterest, activeR){
  lm(pred_sd~YYYY, data = filter(bne.out, get(areaOfInterest) == activeR))$coefficients[[2]]
}

getTimeSlopeCI <- function(areaOfInterest, activeR){
  mod <- summary(lm(data = filter(bne.out, get(areaOfInterest) == activeR), 
                    pred_sd~YYYY))
  fit <- mod$coefficients[2,1]
  se <- mod$coefficients[2,2]
  CI <- paste0(round((fit - 1.96*se), 4), ', ', round((fit + 1.96*se), 4))
}

regionList <- unique(bne.out$region)
regionSlope <- data.frame(region = regionList, 
                          slope_region = map2(rep('region', 10), regionList, getTimeSlope) %>% unlist(), 
                          slopeCI_region = map2(rep('region', 10), regionList, getTimeSlopeCI) %>% unlist())

stateList <- unique(bne.out$state)
stateSlope <- data.frame(state = stateList, 
                          slope_state = map2(rep('state', length(stateList)), stateList, getTimeSlope) %>% unlist(), 
                          slopeCI_state = map2(rep('state', length(stateList)), stateList, getTimeSlopeCI) %>% unlist())


states <- states %>% 
  inner_join(stateSlope, by = 'state') %>%
  inner_join(regionSlope, by = 'region') 


png(here::here(dir.proj, 'outputs', 'comm_mtg',
               paste0('e4_slope_uncert_region.png')), 
    height = 300, width = 400)
ggplot() + 
  geom_sf(data = states, aes(fill = slope_region), colour = NA) + 
  #scale_color_manual(values = c(NA, 'white')) + 
  scale_fill_gradientn(colors = c('blue', 'white', 'red'), 
                       values = c(0, 
                                  scales::rescale(c(0, states$slope_region))[1], 
                                  1)) + 
  ggplot2::theme_void() + 
  ggplot2::labs(x='', y='') +
  ggplot2::guides(fill = guide_colorbar(title = 'Linear Time Trend'), 
                  color = 'none') 
dev.off()

png(here::here(dir.proj, 'outputs', 'comm_mtg',
               paste0('e3_slope_uncert_state.png')), 
    height = 300, width = 400)
ggplot() + 
  geom_sf(data = states, aes(fill = slope_state), colour = NA) + 
  #scale_color_manual(values = c(NA, 'white')) + 
  scale_fill_gradientn(colors = c('blue', 'white', 'red'), 
                       values = c(0, 
                                  scales::rescale(c(0, states$slope_state))[1], 
                                  1)) +   ggplot2::theme_void() + 
  ggplot2::labs(x='', y='') +
  ggplot2::guides(fill = guide_colorbar(title = 'Linear Time Trend'), 
                  color = 'none') 
dev.off()

####******************************
#### 5: Scaled Uncertainty ####
####******************************

bne.out <- bne.out %>% 
  mutate(pred_sd_scaled = 100 * (pred_sd / pred_mean))

bne.out.2013 <- bne.out %>% 
  filter(YYYY == 2013)




png(here::here(dir.proj, 'outputs', 'comm_mtg',
               paste0('e5_uncert_scaled.png')), 
    height = 800, width = 400)
cowplot::plot_grid(
  plotSpatialOneBNEParameter(dta = bne.out.2013, parameter = 'pred_sd_scaled'), 
  makeNiceHistogram(dta = bne.out, parameter = 'pred_sd_scaled'),
  ncol = 1, nrow = 2)
dev.off()
