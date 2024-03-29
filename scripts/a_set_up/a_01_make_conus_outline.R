# File: a_01_make_conus_outline.R
# Authors:
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 01/14/21
#
# Contents:
#  N. Notes
#  0. Import Packages and Set Global Objects
#  1. Make CONUS Outline

#### ------------------- ####
####       N. NOTES      ####
#### ------------------- ####

# N.1. goal 
# for restricting data and visually improving plots, we need a shapefile of the 
# oultine of CONUS. 

#### ------------------------------------------- ####
####  0. IMPORT PACKAGES AND SET GLOBAL OBJECTS  ####
#### ------------------------------------------- ####

# 0.a. import packages and global objects, if you haven't already done so
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

#### ----------------------- ####
####  1. MAKE CONUS OUTLINE  ####
#### ----------------------- ####

# 1.a. set excluded areas 
excludedAreas <- c('Alaska', 'Hawaii', 'Puerto Rico', 
                   'Commonwealth of the Northern Mariana Islands', 'Guam', 
                   'American Samoa', 'United States Virgin Islands')

# 1.b. load the base map from our shapefile
# Robbie: File missing for line below
usa <- st_read(here::here('ancillary_data', 'raw', 'census', 'cb_2015_us_state_500k', 
                          'cb_2015_us_state_500k.shp'))

# 1.c. remove regions that will most likely not be included in the 
# contiguous nationwide application 
conusStates <- usa[!usa$NAME%in%excludedAreas,]

# 1.d. merge the states into a single polygon
conus <- conusStates %>% 
  mutate(g = 'conus', q = 5) %>%
  group_by(g) %>% 
  summarise(m = mean(q)) %>% 
  st_cast()   

# 1.e. transform geographical coordinates to US National Atlas Equal Area Projection
# Robbie: Referred to in a_00 but might be worth explaining why you used this one very briefly (or at least to a link
# which explains why)

# Sebastian: The CRS is just used for visualization, and I chose it somewhat arbitrarily. 
# Solution: I added a little description.

# National Equal Atlas Projection was chosen based on author preference; 
# if you prefer a different projection, you can set the projCRS in a_00.
conus <- st_transform(conus, crs=st_crs(projCRS))

# 1.f. save conus shapefile 
conus %>% 
  st_write(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                      'conus.shp'))

# 1.g. remove objects we used - they are not globally-used objects
rm(usa, conusStates, conus)
