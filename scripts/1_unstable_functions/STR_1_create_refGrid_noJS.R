# File: STR_1_create_refGrid_noJS.R
# Author: Sebastian T. Rowland <sr3463@cumc.columbia.edu>
# Date: 12/22/21

#' \code{create_refGrid_noJS} creates a reference grid with a user-specified density
#' 
#' @param AOI a string describing the area of interest that the refGrid should cover. 
#' currently allowed values: 'conus', 'NYS', 'cities'    
#' @param makePlot a logical determining whether to create a test plot 
#' @param targetDir directory for the testplot
#' 
#' @return saves a fst with columns for lat and lon of centroids; optionally prints a 
#' an image of the grid for review. 
#' 
#' @export
#' @importFrom magrittr %>%
#' 

create_refGrid_noJS <- function(AOI, 
                                makePlot = TRUE, 
                                targetDir = 'Desktop') {
  
  #----------------------------------#
  #### 1. CREATE SHAPEFILE OF AOI ####
  #----------------------------------#
  
  allowedAOI <- c('conus', 'NYS', 'cities' )
  if (!AOI %in% allowedAOI) stop("The AOI specified was not recognized. See documentation.")
  
  
  # 1a restrict to area of interest
  if (AOI == 'conus') {
    # bring in shapefile
    states <- sf::st_read(here::here('data_ancillary', 'raw', 'Census', 'cb_2015_us_state_500k', 
                                     'cb_2015_us_state_500k.shp')) %>% 
      st_transform(crs=sf::st_crs("epsg:4326"))
    # name locations of interest (here the locations to exclude)
    aoiMembers <- 'conus'
    excludedAreas <- c("Alaska", "Hawaii", "Puerto Rico", 
                       "Commonwealth of the Northern Mariana Islands", "Guam", 
                       "American Samoa", "United States Virgin Islands")
    # extract the area of interest from the simple feature
    aoi.sf <- states %>% 
      dplyr::filter(!NAME %in% excludedAreas) %>% 
      dplyr::mutate(member = 'conus')
  }
  
  if (AOI == 'NYS') {
    # bring in shapefile
    states <- sf::st_read(here::here('data_ancillary', 'raw', 'Census', 'cb_2015_us_state_500k', 
                                     'cb_2015_us_state_500k.shp')) %>% 
      st_transform(crs=sf::st_crs("epsg:4326"))
    # name locations of interest (here the locations to exclude)
    aoiMembers <- c('NY')
    # extract the area of interest from the simple feature
    aoi.sf <- states %>% 
      dplyr::filter(STUSPS %in% aoiMembers) %>% 
      dplyr::rename(member = STUSPS)
  }
  
  if (AOI == 'cities') {
    # bring in shapefile
    cbsa <- sf::st_read(here::here('data_ancillary', 'raw', 'Census', 'tl_2015_us_cbsa', 
                                   'tl_2015_us_cbsa.shp')) %>% 
      st_transform(crs=sf::st_crs("epsg:4326"))
    # name locations of interest (here the locations to exclude)
    aoiMembers <- c('Boston-Cambridge-Newton, MA-NH Metro Area',
                   'New York-Newark-Jersey City, NY-NJ-PA Metro Area',
                   'Los Angeles-Long Beach-Anaheim, CA Metro Area',
                   'Chicago-Naperville-Elgin, IL-IN-WI Metro Area')
    # extract the area of interest from the simple feature
    aoi.sf <- cbsa %>% 
      dplyr::filter(NAMELSAD %in% aoiMembers) %>% 
      dplyr::rename(member = NAMELSAD)
  }
  
  #-----------------------------------------------------------------#
  #### 2. IDENTIFY GRID CENTROIDS WITHIN AOI-INSCRIBED RECTANGLE ####
  #-----------------------------------------------------------------#
  
  # 2a. determine grid density 
  # we need less density for the nationwide grid, though when we make the final 
  # product we will need to get a higher density. 
  if (AOI == 'conus') {gridDiameter <- 0.125}
  if (AOI == 'NYS') {gridDiameter <- 0.025}
  if (AOI == 'cities') {gridDiameter <- 0.025}
  
  # 2b. mini function
  create_rectangle_grid <- function(aoi.sf, member, gridDiameter) {
    # restrict to the member of interest
    aoi.sf.member <- aoi.sf %>% 
      dplyr::filter(member == !!member)
    # get min and max coordinates
    min.lat <- sf::st_bbox(aoi.sf)$ymin
    max.lat <- sf::st_bbox(aoi.sf)$ymax
    min.lon <- sf::st_bbox(aoi.sf)$xmin
    max.lon <- sf::st_bbox(aoi.sf)$xmax
    # create a dataframe of grid centroids
    dta <- expand_grid(lat = seq(min.lat, max.lat, by = gridDiameter), 
                       lon = seq(min.lon, max.lon, by = gridDiameter))
    # return resulting rectangular grid
    return(dta)
  }
  
  # 2c. create rectangular grids
  refGrid.rect <- purrr::pmap(list(list(aoi.sf), aoiMembers, gridDiameter), 
              create_rectangle_grid) %>% 
    dplyr::bind_rows()
  
  #---------------------------------------------#
  #### 3. restrict centroids to shape of AOI ####
  #---------------------------------------------#
  
  # remember, the original aoi is not a rectangle
  
  # 3a intersect with original shape of aoi
  refGrid.aoi <- refGrid.rect %>% 
      sf::st_as_sf(coords = c("lon", "lat"), crs=st_crs("epsg:4326")) %>% 
      sf::st_join(aoi.sf, join = st_within) %>% 
      dplyr::filter(!is.na(member)) 
  
  #-----------------------#
  #### 4. save refGrid ####
  #-----------------------#
  
  # 4a. convert refGrid back to dataframe
  # at the end of the day, we only need the lat and lon
  refGrid.aoi <- refGrid.aoi %>%
    dplyr::mutate(lon = st_coordinates(.)[,1], 
           lat = st_coordinates(.)[,2]) %>%
    as.data.frame() %>% 
    dplyr::select(lat, lon) 
  
  # 4b. save the centroids
  refGrid.aoi %>% 
    fst::write_fst(here::here('data_ancillary', 'generated',  
                              paste0('refGrid_', AOI, '.fst')))
  
  #-----------------------------#
  #### 5. make optional plot ####
  #-----------------------------#
  
  if (makePlot) {
    # 5a make refGrid spatial again 
    refGrid.aoi <- refGrid.aoi %>% 
      sf::st_as_sf(coords = c("lon", "lat"), crs=st_crs("epsg:4326")) %>% 
      sf::st_transform(crs=st_crs(projString))
    
    # 5b save image
    png(paste0(targetDir, '/refGridTest_', AOI, '.png'))
    plot(refGrid.aoi)
    dev.off()
  }

}

#---------------------------------------------#
#### 4. bonus code ####
#---------------------------------------------#

# when we do our final conus version, we will need a much finer grid 
# we can use this code to efficiently clip the fine-scale grid
# mini function 
#clip_to_aoi_by_section <- function(grid.rect, aoi.sf, section, nSections) {

# determine the rows for this section
#  nCentroids <- nrow(grid.rect)
#  sectionStart <- 1 + ceiling(nCentroids * (section-1)/nSections)
#  sectionEnd <- ceiling(nCentroids * (section)/nSections)
# do the clipping 
#  grid.rect.clipped <- grid.rect %>% 
#    dplyr::slice(sectionStart:sectionEnd) %>% 
#    sf::st_as_sf(coords = c("lon", "lat"), crs=st_crs("epsg:4326")) %>%
#    sf::st_join(aoi.sf, join = st_within) %>% 
#    dplyr::filter(!is.na(member)) 
# return result 
#  return(grid.rect.clipped)
#}
#if (AOI == 'conus') {

# 1a. separate the center of the us from the borders
# since the center does not need to be clipped to the border
# grid.rect <- grid.rect %>% 
#    dplyr::mutate(lat_border = if_else(lat < quantile(grid.rect$lat, 0.4)[1] | 
#                                       lat > quantile(grid.rect$lat, 0.65)[1], 1, 0), 
#                  lon_border = if_else(lon < quantile(grid.rect$lon, 0.3)[1] | 
#                                       lon > quantile(grid.rect$lon, 0.7)[1], 1, 0)) 
#  grid.rect.clipped <- grid.rect %>% 
#    filter(lat_border + lon_border == 0) 

#  grid.rect.border <- grid.rect %>% 
#    filter(lat_border + lon_border > 0) 

# 1b. convert to sf
# set number of sections 
#  nSections <- 10

#  for (section in 1:nSections) {
#    grid.rect.clipped <- clip_to_aoi_by_section(grid.rect = grid.rect.border, 
#                                                aoi.sf = aoi.sf, 
#                                                section = section, 
#                                                nSections = nSections) %>% 
#      bind_rows(grid.rect.clipped)
#  }
#}

# confirm that it looks good. 
#refGrid <-
 # fst::read_fst(here::here('data_ancillary', 'generated',  
  #                          'refCities.fst'))
# 3d. transform geographical coordinates to Lambert Azimuth Equal Area Projection
#refGrid <- refGrid %>% 
#  sf::st_as_sf(coords = c("lon", "lat"), crs=st_crs("epsg:4326")) %>% 
#  sf::st_transform(crs=st_crs(projString))

#plot(refGrid)
