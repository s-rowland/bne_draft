# File: LGC_1_loadData.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 07/26/21

#' \code{loadData} loads and pre-processes PM2.5 data,
#' given the path to the .csv file containg the data and the dataset's name.
#' 
#' @param path A character giving the file path to a .csv file corresponding to the specified \code{dataset}.
#' @param dataset A character giving the name of the dataset being loaded. 
#' Must be one of \code{{"EPA", "CMAQINS", "CMAQOUTS", "AV", "GS", "CACES", "JSSITES", "JSEPAKEY", "JSREF"}}.
#' 
#' @return A tibble (in long format) containing the loaded and pre-processed dataset. 
#' If \code{dataset = "EPA"} then the tibble will be comprised of the following columns:
#' \describe{
#'    \item{\code{ref_id} a character identifying the reference ID of the EPA AQS monitor.}
#'    \item{\code{lat} a double giving the latitude of the EPA AQS monitor.}
#'    \item{\code{lon} a double giving the longitude of the EPA AQS monitor.}
#'    \item{\code{year}} a character giving the year of the observation.}
#'    \item{\code{month} a character giving the month of the observation.}
#'    \item{\code{day} a character giving the day of the observation.}
#'    \item{\code{obs_pm2_5} a double giving the PM2.5 observation recorded at the EPA AQS monitor on the specified date.}
#' }
#' If \code{dataset} is any of the other acceptable strings, then the tibble will be comprised of the following columns:
#' \describe{
#'    \item{\code{lat} a double giving the latitude of the model prediction.}
#'    \item{\code{lon} a double giving the longitude of the model prediction.}
#'    \item{\code{year}} a character giving the year of the model prediction.}
#'    \item{\code{month}} a character giving the month of the model prediction. Only returned if month of observation is available.}
#'    \item{\code{day}} a character giving the day of the model prediction. Only returned if day of observation is available.}
#'    \item{\code{fips}} a character giving the fips associated with the model prediction. Only returned if lat & lon reference FIPS centroids.}
#'    \item{\code{pred} a double giving the predicted PM2.5 for that location and date.}
#' }
#' 
#' @seealso \code{\link{makeTrainingData}}, \code{\link{saveData}}
#' 
#' @export
#' @importFrom magrittr %>%
loadData <- function(path, dataset) {

  #--------------------------#
  #### 0. error handling: ####
  #--------------------------#
  
  # 0a. soft-check on the dataset being asked for. 
  datasets <- c("EPA", "CMAQINS", "CMAQOUTS", "AV", "GS", "CACES", "JSSITES", 
                "JSEPAKEY", "JSREF", "CMAQOUTS_annual", "JS_annual", "AQS_annual",
                'CAMS', 'MERRA_annual', 'MERRA_daily')
  if (!dataset %in% datasets) stop("The dataset specified was not recognized. See documentation.")

  #-------------------------#
  ####  1. load & clean: ####
  #-------------------------#
  
  # 1a AQS data from EPA 
  # ground-truth data CONUS application and other US-based applications
  # str: added annual option which does not have Required.Day.Count column
  
  if (dataset == "AQS_annual"){
    dta <- readr::read_csv(path, col_types = "cddcccdd")
    dInit <- dta %>% 
      dplyr::select(!c("State.Name", "Arithmetic.Mean")) %>%
      na.omit() %>%
      dplyr::rename(ref_id = Monitor.ID, lat = Latitude, lon = Longitude, 
                    datum = Datum,  year = Year, 
                    obs_pm2_5 = Arithmetic.Mean.Seasonal) %>%
      dplyr::group_by(datum) %>%
      dplyr::group_split(.keep = FALSE)
    
    # reproject the datum and combine
    d <- dInit[[1]] %>% 
      sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs("epsg:4269")) %>%
      sf::st_transform(crs = sf::st_crs("epsg:4326")) %>%
      cbind(., sf::st_coordinates(.)) %>%
      dplyr::rename(lon = X, lat = Y) %>% 
      tibble::as_tibble() %>%
      dplyr::select(-geometry) %>% 
      rbind(dInit[[2]], .) %>% # combined with points already recorded in epsg:4326
      dplyr::select(ref_id, lat, lon, year, obs_pm2_5)
    
  }
    if (dataset == "EPA") {

      # we first keep columns of interest, standardize their names. 
      # Some of the monitor locations are in the NAD83 datum; to project these to 
      # WGS84 we first split the dataset by datum
      # split by datum to reproject to epsg:4326 
    # need to split by datum here and standardize lon / lat coords...
      dta <- readr::read_csv(path, col_types = "cddccccddd")

      dInit <- dta %>% 
        dplyr::select(!c("State.Name", "Arithmetic.Mean", "Required.Day.Count")) %>%
        na.omit() %>%
        dplyr::rename(ref_id = Monitor.ID, lat = Latitude, lon = Longitude, 
                    datum = Datum, date = Date.Local.Formatted, year = Year, 
                    obs_pm2_5 = Arithmetic.Mean.Seasonal) %>%
        dplyr::mutate(month = stringr::str_sub(date, 6, 7), day = stringr::str_sub(date, 9, 10)) %>% 
        dplyr::select(-date) %>%
        dplyr::group_by(datum) %>%
        dplyr::group_split(.keep = FALSE)
    
    # reproject the datum and combine
    d <- dInit[[1]] %>% 
      sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs("epsg:4269")) %>%
      sf::st_transform(crs = sf::st_crs("epsg:4326")) %>%
      cbind(., sf::st_coordinates(.)) %>%
      dplyr::rename(lon = X, lat = Y) %>% 
      tibble::as_tibble() %>%
      dplyr::select(-geometry) %>% 
      rbind(dInit[[2]], .) %>% # combined with points already recorded in epsg:4326
      dplyr::select(ref_id, lat, lon, year, month, day, obs_pm2_5)

  } else if (dataset == "CMAQINS") {

    d <- readr::read_csv(path, col_types = "ddcd") %>%
      dplyr::rename(lat = Lat, lon = Lon, date = Date, pred = Conc) %>%
      dplyr::mutate(year = stringr::str_sub(date, 1, 4), month = stringr::str_sub(date, 6, 7), day = stringr::str_sub(date, 9, 10)) %>%
      dplyr::select(lat, lon, year, month, day, pred) %>%
      na.omit()

  } else if (dataset == "CMAQOUTS") {

    d <- readr::read_csv(path, col_types = "cddcdd") %>%
      dplyr::rename(fips = FIPS, lat = Latitude, lon = Longitude, date = Date, pred = Prediction, stderr = SEpred) %>%
      dplyr::select(-stderr) %>%
      na.omit() %>%
      dplyr::mutate(year = stringr::str_sub(date, 1, 4), 
                    month = stringr::str_sub(date, 6, 7), 
                    day = stringr::str_sub(date, 9, 10)) %>%
      dplyr::select(-date)
  } else if (dataset == "CMAQOUTS_annual") {
    capture_date <- stringr::str_split(stringr::str_split(path, "annual_")[[1]][3], '_')[[1]][1]
    
    d <- fst::read_fst(path) %>%
      dplyr::mutate(year = capture_date)
    
  } else if (dataset == "AV") {
    
    # read in the raster
    rasterObj <- raster::raster(path)
    
    # split data into conus and alaska components
    # renamed usa to conus bc alaska is within usa
    conusExtent <- raster::extent(-124.8, -66.9, 24.4, 49.5) # CONUS
    alaskaExtent <- raster::extent(-179.2, -129.9, 51.1, 71.5) # ALASKA

    conus <- raster::crop(rasterObj, conusExtent)
    alaska <- raster::crop(rasterObj, alaskaExtent)
    conus.coords <- raster::coordinates(conus)
    alaska.coords <- raster::coordinates(alaska)
    conus.pm <- raster::extract(conus, conus.coords)
    alaska.pm <- raster::extract(alaska, alaska.coords)
  
    # extract date from the path name 
    # renamed date because date is a function in base R 
    capture_date <- stringr::str_split(stringr::str_split(path, "V4NA03_PM25_NA_")[[1]][2], "_")[[1]][1]
    
    # create dataframe with conus and Alaska predictions
    d <- rbind(tibble::tibble(lon = conus.coords[,"x"], lat = conus.coords[,"y"], pred = conus.pm), 
               tibble::tibble(lon = alaska.coords[,"x"], lat = alaska.coords[,"y"], pred = alaska.pm)) %>% 
          na.omit() %>%
          dplyr::mutate(year = stringr::str_sub(capture_date, 1, 4), 
                        month = stringr::str_sub(capture_date, 5, 6)) %>%
          dplyr::select(lat, lon, year, month, pred)

  } else if (dataset == "GS") {

    load(path) # mydata2 is loaded into the environment...
    # note that GS has extra columns and allows us to explicitly filter by country
    d <- mydata2 %>%
      tibble::as_tibble() %>%
      dplyr::filter(Country == "USA") %>%
      dplyr::select(!(tidyselect::contains("log") | tidyselect::contains("pop"))) %>%
      tidyr::pivot_longer(cols = tidyselect::contains("_PM2.5_"), 
                          names_to = c("measurement", "year"),
                          names_pattern = "(.*)_PM2.5_(.*)",
                          values_to = "pred") %>%
      dplyr::rename(lat = Latitude, lon = Longitude) %>%
      dplyr::select(lat, lon, measurement, year, pred) %>%
      dplyr::filter(measurement == "Mean") %>%
      dplyr::select(-measurement)

  } else if (dataset == "CACES") {

    d <- readr::read_csv(path, col_types = "cccdcdd") %>%
      dplyr::rename(pred = pred_wght) %>%
      dplyr::select(lat, lon, fips, year, pred)

  } else if (dataset == "CAMS") {
    rasterObj <- raster::raster(path)
    
    # double check that you grab the right area
    #ad <- calc(rasterObj, fun=function(x){log(x)})
    #plot(ad)
    
    conusExtent <- raster::extent(365-134.8, 365-66.9, 24.4, 49.5) # CONUS
    conus <- raster::crop(rasterObj, conusExtent)
    conus.coords <- raster::coordinates(conus)
    conus.pm <- raster::extract(conus, conus.coords)
    capture_date <- stringr::str_split(stringr::str_split(path, "_raw/")[[1]][2], "adj")[[1]][1]
    
    d <- tibble::tibble(lon = conus.coords[,"x"], lat = conus.coords[,"y"], pred = conus.pm) %>% 
      na.omit() %>%
      dplyr::mutate(year = stringr::str_sub(capture_date, 1, 4)) %>%
      dplyr::select(lat, lon, year, pred)
    
  } else if (dataset == "MERRA_annual") {
    
    # read in the raster
    rasterObj <- raster::raster(path)
    
    # split data into conus and alaska components
    # renamed usa to conus bc alaska is within usa
    conusExtent <- raster::extent(-124.8, -66.9, 24.4, 49.5) # CONUS
    alaskaExtent <- raster::extent(-179.2, -129.9, 51.1, 71.5) # ALASKA
    
    conus <- raster::crop(rasterObj, conusExtent)
    alaska <- raster::crop(rasterObj, alaskaExtent)
    conus.coords <- raster::coordinates(conus)
    alaska.coords <- raster::coordinates(alaska)
    conus.pm <- raster::extract(conus, conus.coords)
    alaska.pm <- raster::extract(alaska, alaska.coords)
    
    # extract date from the path name 
    # renamed date because date is a function in base R 
    capture_date <- stringr::str_sub(stringr::str_split(path, "adjPM25sum")[[1]][1],-4, -1)
    
    # create dataframe with conus and Alaska predictions
    d <- rbind(tibble::tibble(lon = conus.coords[,"x"], lat = conus.coords[,"y"], pred = conus.pm), 
               tibble::tibble(lon = alaska.coords[,"x"], lat = alaska.coords[,"y"], pred = alaska.pm)) %>% 
      na.omit() %>%
      dplyr::mutate(year = stringr::str_sub(capture_date, 1, 4)) %>%
      dplyr::select(lat, lon, year, pred)
    
    # Non- prediction datasets
  } else if (dataset == "JSSITES") {

    d <- readRDS(path) %>% 
      tibble::as_tibble() %>%
      dplyr::rename(js_lat = Lat, js_lon = Lon) %>%
      dplyr::select(js_lat, js_lon) %>%
      dplyr::mutate(js_index = dplyr::row_number())
    
  } else if (dataset == "JS_annual") {
    
    capture_date <- stringr::str_split(stringr::str_split(path, "js_annual_")[[1]][3], "_")[[1]][1]
    
    d <- fst::read.fst(path) %>%
      dplyr::mutate(year = capture_date)
    
  } else if (dataset == "JSEPAKEY") {

    d <- readr::read_csv(path)

  } else if (dataset == "JSREF") {

    d <- readr::read_csv(path)

  }

  return(d)

}