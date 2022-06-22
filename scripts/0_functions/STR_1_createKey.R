# File: STR_1_createKey.R
# Author: Sebastian T. Rowland <sr3463@cumc.columbia.edu>
# Date: 12/22/21

#' \code{createKey} creates a reference grid with a user-specified density
#' 
#' @param ref.df dataframe of the reference locations. Must have lat, lon, not be a sf. 
#' currently we assume that it has crs of 4326. 
#' @param refName string of the short name of the reference dataset, 
#' used when naming the key 
#' @param baseModel.df dataframe of the reference locations. Must have lat, lon, not be a sf. 
#' currently we assume that it has crs of 4326. Could be something other than a base model
#' but baseModel is the most typical use case
#' @param baseModelName string of the name of the base model, 
#' used when naming the key 
#' @param timescale string. annual, monthly, or daily? 
#' 
#' @return saves a fst with columns for lat and lon of centroids; optionally prints a 
#' an image of the grid for review. 
#' 
#' @export
#' @importFrom magrittr %>%
#' 


# note: make sure there are no duplicate locations in your baseModel.df!!! 

createKey <- function(ref.df, refName, baseModel.df, baseModelName) {

  #### --------------------------------------- ####
  #### 0. example arguments and error catching ####
  #### --------------------------------------- ####
  
  if (!stringr::str_detect(paste(names(ref.df), collapse = ''), 'lat') | 
      !stringr::str_detect(paste(names(ref.df), collapse = ''), 'lon') | 
      !stringr::str_detect(paste(names(baseModel.df), collapse = ''), 'lat') | 
      !stringr::str_detect(paste(names(baseModel.df), collapse = ''), 'lon')) {
    stop("At least one dataset is missing location data. See documentation.")
  }

  #### ------------------ ####
  #### 1. wrangle ref.df ####
  #### ------------------ ####
  
  # 1a. rename the variables so easier to keep track of during the join
  # and keep only unique locations
  ref.df <- ref.df %>% 
    dplyr::rename(ref_lat = lat, ref_lon = lon)
  
  
  if (stringr::str_detect(refName, 'aqs')) {
    ref.df <- ref.df %>% 
      mutate(ref_id = ref_id)
  } else { 
   ref.df <- ref.df %>%
      dplyr::mutate(ref_id = row_number()) }
    
  # 1.b. wrangle dates
  if (stringr::str_detect(paste(names(ref.df), collapse = ''), 'day') & 
     stringr::str_detect(paste(names(ref.df), collapse = ''), 'month') & 
     stringr::str_detect(paste(names(ref.df), collapse = ''), 'year')) {
    ref.df <- ref.df %>% 
     dplyr::select(ref_lat, ref_lon, ref_id, day, month, year)
  } else if (stringr::str_detect(paste(names(ref.df), collapse = ''), 'month') & 
     stringr::str_detect(paste(names(ref.df), collapse = ''), 'year')) {
    ref.df <- ref.df %>% 
      dplyr::select(ref_lat, ref_lon, ref_id, month, year)
  } else if (stringr::str_detect(paste(names(ref.df), collapse = ''), 'year')) {
    ref.df <- ref.df %>% 
      dplyr::select(ref_lat, ref_lon, ref_id, year)
  }
  
  # 1.c. get the unique ref locations 
  refLoc.df <- ref.df %>% 
    dplyr::select(ref_lat, ref_lon, ref_id) %>% 
    distinct()
  
  # 1.d. prepare the base model for joining
  baseModelLoc.df <- baseModel.df %>% 
    dplyr::mutate(baseModel_id = row_number()) %>% 
    dplyr::rename(baseModel_lat = lat, baseModel_lon = lon) %>%
    dplyr::select(baseModel_id, baseModel_lat, baseModel_lon) 
  
  #### ---------------------------- ####
  ####   3. join ref and base model ####
  #### ---------------------------- ####
  
  # this spatial join identifies the nearest base model centroid to each point 
  # in the ref.df
  
  # 3a. convert data to simple features, in the same projection 
  # recall that the crs of the epa is fixed in the loadData command
  refLoc.sf <- refLoc.df %>%
    sf::st_as_sf(coords = c("ref_lon", "ref_lat"), crs = sf::st_crs("epsg:4326")) %>%
    sf::st_transform(crs = sf::st_crs("epsg:2163"))
  
  # this step should take ~3min
  baseModelLoc.sf <- baseModelLoc.df %>%
    sf::st_as_sf(coords = c("baseModel_lon", "baseModel_lat"), 
                 crs = sf::st_crs("epsg:4326")) %>%
    sf::st_transform(crs = sf::st_crs("epsg:2163"))
  
  # 3b. identify the indicies of the relevant locations of the base model
  # this step should take ~20 seconds
  tictoc::tic()
  baseModelIndices <- unlist(nngeo::st_nn(refLoc.sf, baseModelLoc.sf, k = 1))
  tictoc::toc()
  
  # 3c. create table linking the aqs and the base model locations 
  # we can subsequently use this key for filtering 
  key_ref_baseModel <- baseModelLoc.df[baseModelIndices, ] %>%
    cbind(refLoc.df, .) %>%
    tibble::as_tibble() %>%
    dplyr::arrange(baseModel_id) 
  
  # 3d. throw a warning if the number of entries in the key is not equal to the 
  # number of locations in the refence grid
  if (nrow(key_ref_baseModel) != nrow(ref.df)) stop("There are duplicate locations or a similar error")
  
  
  # 3d. save results
  key_ref_baseModel %>% 
    fst::write_fst(here::here('inputs', 'pm25', 'keys', 
                                paste0('key_nn_', refName, '_', baseModelName, '.fst')))
}
