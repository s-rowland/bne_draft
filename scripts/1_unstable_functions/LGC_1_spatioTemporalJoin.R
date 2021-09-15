# File: LGC_1_spatioTemporalJoin.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 07/27/21

#' \code{spatioTemporalJoin} cleans and formats PM2.5 data for training BNE.
#' Briefly, given a reference dataset, R, (e.g. EPA AQS PM2.5 observations from EPA monitors) 
#' and PM2.5 predictions from a model, M, \code{spatioTemporalJoin} will use 
#' nearest neighbours to merge the two datasets, returning a single training dataset with 
#' the model predictions from M that correspond closest (spatially) to R's points, 
#' for each date (temporally) R has PM2.5 values.
#' 
#' @param refData A tibble of reference PM2.5 observations in long format. 
#' Must include the following columns (in the following datatypes):
#' \code{ref_id (chr), lat (dbl), lon (dbl), year (chr), obs_pm2_5 (dbl)}. 
#' Additional optional columns can include: \code{month (chr), day (chr)}, 
#' depending on the temporal resolution of the reference data, 
#' and \code{fips} depending on the spatial resolution.
#' \code{day}, \code{month}, and \code{year} should be formatted as "DD", "MM", and "YYYY", respectively.
#' @param modelData A tibble of PM2.5 predictions from a model M in long format.  
#' Must include the following columns (in the following datatypes):
#' \code{lat (dbl), lon (dbl), year (chr), pred (dbl)}. 
#' Additional optional columns can include: \code{month (chr), day (chr)}, 
#' depending on the temporal resolution of the model's data,
#' and \code{fips} depending on the spatial resolution.
#' \code{day}, \code{month}, and \code{year} should be formatted as "DD", "MM", and "YYYY", respectively.
#' @param modelName A character providing the name of the model associated with \code{modelData}.
#' @param override (Optional) A logical indicating if you would like to override \code{spatioTemporalJoin}'s default behaviour of
#' parsing the data in month-size chunks (if monthly data is available). By default \code{override = FALSE}, but when 
#' set as \code{TRUE}, \code{spatioTemporalJoin} will always parse the data in year-size chunks, i.e. one year at a time.
#' @param censusTractFile (Optional) A character vector providing the path to a census tract directory or file 
#' to read in when either \code{refData} or \code{modelData} include FIPS information in an optional \code{fips} column.
#' Ignored when neither \code{refData} nor \code{modelData} include a \code{fips} column. By default, \code{censusTractFile = NULL}. 
#' @param refCRS (Optional) A character providing the coordinate reference system used 
#' for \code{refData}'s \code{lat} and \code{lon} columns. By default, \code{refCRS = "epsg:4326"}.
#' @param modelCRS (Optional) A character providing the coordinate reference system used 
#' for \code{modelData}'s \code{lat} and \code{lon} columns. By default, \code{modelCRS = "epsg:4326"}.
#' @param outputCRS (Optional) A character providing the coordinate reference system to be used
#' for the output \code{lat} and \code{lon} columns in the returned tibble. By default, \code{outputCRS = "epsg:2163"}.
#' @param verbose (Optional) A logical indicating if verbose output should be printed as the function executes. 
#' By default, \code{verbose = TRUE}. 
#' 
#' @return A tibble containing the model predictions from \code{modelData} that correspond 
#' closest (spatially) to the reference observations from \code{refData}, for each date we have reference observations.
#' The tibble will be in long format, comprised of the following columns:
#' \describe{
#'    \item{\code{ref_id} a character identifying the reference ID of the \code{(lat, lon)} pair.}
#'    \item{\code{lat} a double giving the latitude of the ref_id.}
#'    \item{\code{lon} a double giving the longitude of the ref_id.}
#'    \item{\code{year} a character giving the year of the measurements.}
#'    \item{\code{month} (provided both \code{refData} and \code{modelData} have \code{month} columns) a character giving the month of the measurements.}
#'    \item{\code{day} (provided both \code{refData} and \code{modelData} have \code{day} columns) a character giving the day of the measurements.}
#'    \item{\code{obs_pm2_5} a double giving the PM2.5 observation recorded at the ref_id on the specified date.}
#'    \item{\code{[modelName]_pred} a double giving the PM2.5 prediction made by \code{modelName} on the specified date.}
#' }
#' 
#' @seealso \code{\link{loadData}}
#' 
#' @export
#' @importFrom magrittr %>%
#' @importFrom foreach foreach %do%
spatioTemporalJoin <- function(
  refData, 
  modelData, 
  modelName, 
  override = FALSE,
  censusTractFile = NULL,
  refCRS = "epsg:4326", 
  modelCRS = "epsg:4326", 
  outputCRS = "epsg:2163", 
  verbose = TRUE
) {

  #--------------------------#
  #### 0. error handling: ####
  #--------------------------#
  
  # 0a. soft-check on the refData column names
  # the reason we check for "year" in a separate if-statement outside 
  # of the for-loop is to make calculations in 1b easier (same applies to 0b).
  refCols <- colnames(refData)
  expRefCols <- c("ref_id", "lat", "lon", "obs_pm2_5")
  for (c in expRefCols) if (!c %in% refCols) stop("refData does not have the proper column names (ref_id, lat, lon, obs_pm2_5). See the documentation & rename.")
  if (!"year" %in% refCols) stop("refData does not have the 'year' column. See the documentation & rename.")
  
  # 0b. soft-check on the modelData column names
  modelCols <- colnames(modelData)
  expModelCols <- c("lat", "lon", "pred")
  for (c in expModelCols) if (!c %in% modelCols) stop("modelData does not have the proper column names (lat, lon, pred). See the documentation & rename.")
  if (!"year" %in% modelCols) stop("modelData does not have the 'year' column. See the documentation & rename.")
  
  # 0c. check for censusTractFile if 'fips' shows up in either dataset
  if (("fips" %in% union(refCols, modelCols)) && is.null(censusTractFile)) stop("FIPS codes were provided in one of input dataframes under the column 'fips', however no censusTractFile was provided.")
  
  #### ------------------ ####
  ####     1. setup:      ####
  #### ------------------ ####
  # 1a. general setup:
  model_pred <- paste(modelName, "pred", sep = "_")
  
  # 1b. time-step calculations:
  refDateVars <- setdiff(refCols, expRefCols)
  modelDateVars <- setdiff(modelCols, expModelCols)
  # the extra call to intersect on the following line looks like its superfluous but 
  # its to prevent a column like 'fips' from sneaking into dateVarsForJoin.
  # dateVarsForJoin keeps track of how we will be temporally joining the data,
  # i.e. via day-month-year? or via month-year? or just via year? to handle 
  # daily, monthly, or annual data respectively. 
  dateVarsForJoin <- sort(intersect(intersect(refDateVars, modelDateVars), c("day", "month", "year"))) 
  # mode just keeps track of how we should process refData & modelData;
  # we can parse the dataframes one month at a time, or one year at a time.
  # for large datasets, computationally we need to break them up into smaller
  # chunks so operations like nngeo::st_nn don't scale badly. so we parse them
  # one month at a time. for smaller datasets, parsing data one year at a time is ok.
  # if mode >= 2, then we parse things one month at a time, unless override was set to TRUE.
  # else, we parse things one year at a time.
  mode <- length(dateVarsForJoin)

  if (mode >= 2 && !override) { # could change to (mode == 3)...
    refTimeSteps <- refData %>% 
      dplyr::distinct(month, year) %>%
      dplyr::select(month, year) %>%
      tidyr::unite("time_step") 
    
    modelTimeSteps <- modelData %>%
      dplyr::distinct(month, year) %>%
      dplyr::select(month, year) %>%
      tidyr::unite("time_step")

    timeSteps <- intersect(refTimeSteps$time_step, modelTimeSteps$time_step) %>%
      tibble::tibble(time_step = .) %>%
      tidyr::separate(time_step, into = c("month", "year"), sep = "_")

    n <- nrow(timeSteps)
  } else {
    timeSteps <- intersect(unique(refData$year), unique(modelData$year))
    n <- length(timeSteps)
  } 

  # 1c. progress bar:
  if (verbose) {
    progressBar <- txtProgressBar(min=0, max=n*5, width=50, style=3)
    counter <- 0
    pb <- function(p) { setTxtProgressBar(progressBar, p); return(p+1) }
    counter <- pb(counter)
  }

  # 1d. extra columns we want to keep:
  extraCols <- setdiff(refCols, c(expRefCols, "year", "month", "day", "fips"))

  #--------------------------#
  ####  2. data cleaning: ####
  #--------------------------#
  
  # here we will use a foreach loop to parse the data one timeStep at a time
  # so that the sf, nngeo, and join operations don't scale badly...
  df <- foreach(i = iterators::icount(n), .combine = rbind) %do% {

    # 2a. process ref data
    # 2a.i restrict to current timeStep
    if (mode >= 2 && !override) {
      # parse one month at a time:
      refData.timeStep <- refData %>%
        dplyr::rename(ref_lat = lat, ref_lon = lon) %>%
        dplyr::filter(year == timeSteps$year[i], month == timeSteps$month[i])
    } else {
      # parse one year at a time:
      refData.timeStep <- refData %>%
        dplyr::rename(ref_lat = lat, ref_lon = lon) %>%
        dplyr::filter(year == timeSteps[i])
    }
    
    # 2a.ii extract locations
    refLocations <- refData.timeStep %>%
      dplyr::select(ref_id, ref_lat, ref_lon) %>%
      dplyr::distinct()
    
    # 2a.iii process ref data spatially (convert to simple features)
    # here we take fips into account if refData's lon and lat coords
    # reference the centroids of fips / census tracts. 
    # Aka - by default we treat data as points, which would almost always yield 
    # the same results as treating it as regular rectangles. 
    # However, if the data represents average of an irregular area, such as 
    # Census tracts, we want to capture that irregular area
    if ("fips" %in% refCols) {
      refLocations.sf <- sf::read_sf(censusTractFile) %>%
        dplyr::inner_join(., refData.timeStep, by = c("GEOID" = "fips")) %>%
        sf::st_transform(crs = sf::st_crs(outputCRS))
    } else {
      refLocations.sf <- refLocations %>%
        sf::st_as_sf(coords = c("ref_lon", "ref_lat"), crs = sf::st_crs(refCRS)) %>%
        sf::st_transform(crs = sf::st_crs(outputCRS))
    }

    if (verbose) counter <- pb(counter)

    # 2b. process model data
    # 2b.i restrict to current timeStep
    if (mode >= 2 && !override) {
      # parse one month at a time:
      modelData.timeStep <- modelData %>%
        dplyr::filter(year == timeSteps$year[i], month == timeSteps$month[i]) %>%
        dplyr::rename(!!model_pred := pred, model_lat = lat, model_lon = lon)
    } else {
      # parse one year at a time:
      modelData.timeStep <- modelData %>%
        dplyr::filter(year == timeSteps[i]) %>%
        dplyr::rename(!!model_pred := pred, model_lat = lat, model_lon = lon)
    }
    
    # 2b.iii process model data spatially (convert to simple features)
    # here we take fips into account if modelData's lon and lat coords
    # reference the centroids of fips / census tracts. 
    # add . timeStep to modelLocations.sf
    if ("fips" %in% modelCols) {
      modelLocations.sf <- sf::read_sf(censusTractFile) %>%
        dplyr::inner_join(., modelData.timeStep, by = c("GEOID" = "fips")) %>%
        sf::st_transform(crs = sf::st_crs(outputCRS))
    } else {
      modelLocations.sf <- modelData.timeStep %>%
        dplyr::select(model_lat, model_lon) %>% 
        dplyr::distinct() %>%
        sf::st_as_sf(coords = c("model_lon", "model_lat"), crs = sf::st_crs(modelCRS)) %>%
        sf::st_transform(crs = sf::st_crs(outputCRS))
    }
    
    # add ticker if requested
    if (verbose) counter <- pb(counter)

    # 2c. run nearest neighbours to get spatial mapping:
    # for each point in refData, modelIndicies has a row containing the index 
    # (row_number) of the nearest model point. 
    modelIndices <- unlist(
      suppressMessages(
        nngeo::st_nn(refLocations.sf, modelLocations.sf, k = 1)
      )
    )
    
    # add ticker if requested
    if (verbose) counter <- pb(counter)
    
    # 2d. create a a table of the refData locations and lat and lon of nearest point 
    # from the model
    nnTable <- modelData.timeStep[modelIndices, ] %>%
      dplyr::select(model_lat, model_lon) %>%
      cbind(refLocations, .) %>%
      tibble::as_tibble()

    # 2e. spatial join: merge the model data for the timeStep 
    # with the nearest neighbour ref locations for the timeStep 
    # by lon and lat (spatially)
    modelData.timeStep.nn <- dplyr::inner_join(x = modelData.timeStep, y = nnTable, 
                                               by = c("model_lon", "model_lat")) %>%
      dplyr::select(ref_id, !!dateVarsForJoin, !!model_pred)

    if (verbose) counter <- pb(counter)

    # 2f. spatio-temporal join: merge the ref data and model data for the timeStep
    # by ref_id and date (temporally)
    refModel.timeStep <- dplyr::inner_join(x = refData.timeStep, y = modelData.timeStep.nn, 
                                           by = c("ref_id", dateVarsForJoin)) %>%
      dplyr::select(!!c("ref_id", "ref_lat", "ref_lon", 
                        intersect(refDateVars, c("day", "month", "year")), 
                        "obs_pm2_5", extraCols, model_pred)) %>%
      dplyr::rename(lat = ref_lat, lon = ref_lon)

    if (verbose) counter <- pb(counter)

    # 2g. Return joined dataset
    refModel.timeStep

  }

  return(df)

}