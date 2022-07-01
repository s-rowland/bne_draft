# File: j_20_eFgi1_AQS_availability_trends.R
# BNE Annual Uncertainty Analysis
# Authors:
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 10/04/2022
#
# Contents:
#  N. notes
#  0. preparation
#  1. bring in data
#  2. plot overall availabilitiy
#  3. plot regional trends of availability
#  4. save plot 

#### ------------------ ####
####       N. notes     ####
#### ------------------ ####


#### ---------------- ####
####  0. preparation  ####
#### ---------------- ####

# 0.a. load packages, etc
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

# 0.b. load objects and packages specific for this work
if(!exists('ran_a_00_uncert')){
  here::i_am('README.md')
  source(here::here('str_uncert_analysis', 'code', 
                    '0_00_config_env_uncert_analysis.R'))
}

# 0.c. loac objects for generating plots
if(!exists('ran_j_00')){
  here::i_am('README.md')
  source(here::here('str_uncert_analysis', 'code', 'j_generate_results_for_manuscript',
                    'j_00_set_plotting_features.R'))
}

#### ------------------ ####
####  1. bring in data  ####
#### ------------------ ####

# 1.a bring in aqs training data 
aqs <- read_csv(here::here('inputs', 'pm25', 'training_datasets','annual_combined', 
                           'training_cvfolds.csv'))

# 1.b bring in states polygon 
# 1.b.i set excluded areas 
excludedAreas <- c('Alaska', 'Hawaii', 'Puerto Rico', 
                     'Commonwealth of the Northern Mariana Islands', 'Guam', 
                     'American Samoa', 'United States Virgin Islands')

# 1.b.ii load the base map from our shapefile
states <- st_read(here::here('ancillary_data', 'raw', 'census', 'cb_2015_us_state_500k', 
                          'cb_2015_us_state_500k.shp'))%>%
  sf::st_transform(crs=sf::st_crs(projCRS))

# 1.b.iii remove regions that will most likely not be included in the 
# contiguous nationwide application 
states <- states[!states$NAME%in%excludedAreas,]

#### ------------------------------- ####
####  2. plot overall availabilitiy  ####
#### ------------------------------- ####

# 2.a aggreagte counts of observations 
aqs.agg <- aqs %>% 
  group_by(lat, lon) %>% 
  summarize(num_obs = n())

aqs.agg.sf <- aqs.agg %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs=sf::st_crs("epsg:4326")) %>%
  sf::st_transform(crs=sf::st_crs(projCRS))

aqs.agg.sf1 <- aqs.agg.sf %>% 
  filter(num_obs ==1 )
aqs.agg.sf2 <- aqs.agg.sf %>% 
  filter(num_obs ==2 )
aqs.agg.sf3 <- aqs.agg.sf %>% 
  filter(num_obs ==3 )
aqs.agg.sf4 <- aqs.agg.sf %>% 
  filter(num_obs ==4 )
aqs.agg.sf5 <- aqs.agg.sf %>% 
  filter(num_obs ==5 )
aqs.agg.sf6 <- aqs.agg.sf %>% 
  filter(num_obs ==6 )

colSet <- RColorBrewer::brewer.pal(7, 'BuPu')[2:7]
aqs.agg.sf <- aqs.agg.sf %>% 
  mutate(num_obs = as.factor(num_obs))

# 2.b create plot 
TP.a <- ggplot(states) + 
  geom_sf(fill = 'white') +
  geom_sf(data = aqs.agg.sf, aes(fill = num_obs, color = num_obs)) + 
  scale_fill_manual(values = colSet) +
  scale_color_manual(values = colSet) +
  labs(fill = 'Count', color = 'Count') + 
  geom_sf(data = aqs.agg.sf1, fill = colSet[1], color = colSet[1]) + 
  geom_sf(data = aqs.agg.sf2, fill = colSet[2], color = colSet[2]) + 
  geom_sf(data = aqs.agg.sf3, fill = colSet[3], color = colSet[3]) + 
  geom_sf(data = aqs.agg.sf4, fill = colSet[4], color = colSet[4]) + 
  geom_sf(data = aqs.agg.sf5, fill = colSet[5], color = colSet[5]) + 
  geom_sf(data = aqs.agg.sf6, fill = colSet[6], color = colSet[6]) + 
  tema + 
  theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        legend.box.background = element_rect(color = 'white'), 
        legend.key.size = unit(1, 'line'), 
        legend.key = element_rect(fill = "white"))


#### ----------------------------------------- ####
####  3. plot regional trends of availability  ####
#### ----------------------------------------- ####

# 3.a aggregate by region
aqs.time.regional <- aqs %>% 
  inner_join(key.state.region, by = 'state') %>% 
  inner_join(key.regionNames, by = 'region') %>% 
  group_by(yyyy, region_name) %>% 
  summarize(Count = n()) %>% 
  orderRegionNames() %>% ungroup()


# 2.d. plot by region
TP.b <- ggplot(aqs.time.regional) + 
  geom_line(aes(x = yyyy, y= Count, color = region_name)) + 
 scale_color_manual(values = col.Region) +
  labs(x = 'Year', y= 'Annual Count of Observations', color = 'Region') + 
  theme_minimal() + 
  scale_x_continuous(breaks = c(2010, 2013, 2015)) +
  theme(axis.text = element_text(size = 15), 
        axis.title = element_text(size = 18), 
        title = element_text(size = 20), 
        legend.text = element_text(size = 13), 
        legend.title = element_text(size = 15)) 

#### -------------- ####
####  4. save plot  ####
#### -------------- ####

png(here::here(dir.proj, 'manuscript', 'eFigure1_AQS_availability.png'), 
    height = 400, width =800)
cowplot::plot_grid(TP.a, TP.b, nrow = 1, labels = c('A', 'B'), 
                   rel_heights = c(1, 0.25))
dev.off()
