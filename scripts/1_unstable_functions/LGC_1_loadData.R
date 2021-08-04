# File: LGC_1_loadData.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 07/26/21
# To Do: need to add code to handle GS, Caces, and JS..!

#' \code{loadData} loads and pre-processes PM2.5 data,
#' given the path to the .csv file containg the data and the dataset's name.
#' 
#' @param path A character giving the file path to a .csv file corresponding to the specified \code{dataset}.
#' @param dataset A character giving the name of the dataset being loaded. 
#' Must be one of \code{{"EPA", "CMAQINS", "CMAQOUTS", "AV"}}.
#' 
#' @return A tibble (in long format) containing the loaded and pre-processed dataset. 
#' If \code{dataset = "EPA"} then the tibble will be comprised of the following columns:
#' \describe{
#'    \item{\code{ref_id} a character identifying the reference ID of the EPA AQS monitor.}
#'    \item{\code{lat} a double giving the latitude of the EPA AQS monitor.}
#'    \item{\code{lon} a double giving the longitude of the EPA AQS monitor.}
#'    \item{\code{date} a character giving the date of the observation.}
#'    \item{\code{year}} a character giving the year of the observation.}
#'    \item{\code{obs_pm2_5} a double giving the PM2.5 observation recorded at the EPA AQS monitor on the specified date.}
#' }
#' If \code{dataset} is any of the other acceptable strings, then the tibble will be comprised of the following columns:
#' \describe{
#'    \item{\code{lat} a double giving the latitude of the model prediction.}
#'    \item{\code{lon} a double giving the longitude of the model prediction.}
#'    \item{\code{date} a character giving the date of the model prediction.}
#'    \item{\code{year}} a character giving the year of the model prediction.}
#'    \item{\code{pred} a double giving the predicted PM2.5 for that location and date.}
#' }
#' 
#' @seealso \code{\link{makeTrainingData}}
#' 
#' @export
#' @importFrom magrittr %>%
loadData <- function(path, dataset) {

  #### ------------------ ####
  #### 0. error handling: ####
  #### ------------------ ####
  # 0a. soft-check on the dataset being asked for. 
  datasets <- c("EPA", "CMAQINS", "CMAQOUTS", "AV")
  if (!dataset %in% datasets) stop("The dataset specified was not recognized. See documentation.")

  #### ------------------ ####
  ####  1. load & clean: ####
  #### ------------------ ####
  if (dataset == "EPA") {

    # need to split by datum here and standardize lon / lat coords...
    dInit <- readr::read_csv(path, col_types = "cddccccddd") %>%
      dplyr::select(!c("State.Name", "Arithmetic.Mean", "Required.Day.Count")) %>% 
      na.omit() %>%
      dplyr::rename(ref_id = Monitor.ID, lat = Latitude, lon = Longitude, datum = Datum, date = Date.Local.Formatted, year = Year, obs_pm2_5 = Arithmetic.Mean.Seasonal) %>%
      dplyr::mutate(month = stringr::str_sub(date, 6, 7), day = stringr::str_sub(date, 9, 10)) %>%
      dplyr::select(-date) %>%
      dplyr::group_by(datum) %>%
      dplyr::group_split(.keep = FALSE)

    d <- dInit[[1]] %>% 
      sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs("epsg:4269")) %>%
      sf::st_transform(crs = sf::st_crs("epsg:4326")) %>%
      cbind(., sf::st_coordinates(.)) %>%
      dplyr::rename(lon = X, lat = Y) %>% 
      tibble::as_tibble() %>%
      dplyr::select(-geometry) %>% 
      rbind(dInit[[2]], .)

  } else if (dataset == "CMAQINS") {

    d <- readr::read_csv(path, col_types = "ddcd") %>%
      dplyr::rename(lat = Lat, lon = Lon, date = Date, pred = Conc) %>%
      dplyr::mutate(year = stringr::str_sub(date, 1, 4), month = stringr::str_sub(date, 6, 7), day = stringr::str_sub(date, 9, 10)) %>%
      dplyr::select(-date) %>%
      na.omit()

  } else if (dataset == "CMAQOUTS") {

    d <- readr::read_csv(path, col_types = "cddcdd") %>%
      dplyr::rename(fips = FIPS, lat = Latitude, lon = Longitude, date = Date, pred = Prediction, stderr = SEpred) %>%
      dplyr::select(-stderr) %>%
      na.omit() %>%
      mutate(year = stringr::str_sub(date, 1, 4), month = stringr::str_sub(date, 6, 7), day = stringr::str_sub(date, 9, 10)) %>%
      dplyr::select(-date)

  } else if (dataset == "AV") {

    date <- stringr::str_split(stringr::str_split(path, "V4NA03_PM25_NA_")[[1]][2], "_")[[1]][1]
    usaExtent <- raster::extent(-124.8, -66.9, 24.4, 49.5) # USA
    alaskaExtent <- raster::extent(-179.2, -129.9, 51.1, 71.5) # ALASKA

    rasterObj <- raster::raster(path)
  
    usa <- raster::crop(rasterObj, usaExtent)
    alaska <- raster::crop(rasterObj, alaskaExtent)
    usa.coords <- raster::coordinates(usa)
    alaska.coords <- raster::coordinates(alaska)
    usa.pm <- raster::extract(usa, usa.coords)
    alaska.pm <- raster::extract(alaska, alaska.coords)
  
    d <- rbind(tibble::tibble(lon = usa.coords[,"x"], lat = usa.coords[,"y"], pred = usa.pm), 
               tibble::tibble(lon = alaska.coords[,"x"], lat = alaska.coords[,"y"], pred = alaska.pm)) %>% 
          na.omit() %>%
          dplyr::mutate(year = stringr::str_sub(date, 1, 4), month = stringr::str_sub(date, 5, 6))

  }

  # need to add code for GS, Caces, and JS..!

  return(d)

}