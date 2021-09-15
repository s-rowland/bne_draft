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

computeAreaWeightedAverage <- function(AggregationLayer, FeatureLayer, FeatureVar){
  #AggregationLayer <- baseGrid;  FeatureLayer <- uncert.sf; FeatureVar <- 'std'

  # 1a Assign Gridid 
 # AggregationLayer <- AggregationLayer %>% 
   # mutate(gridID = row_number())
  
  # 1b Create Intersections
  # not too bad, takes less than 5 min to run. 
  intersec <- st_intersection(AggregationLayer, FeatureLayer)
  
  # 1c Calculate Area
  intersec <- intersec %>% 
    mutate(area = st_area(intersec)) %>% 
    mutate(area = as.numeric(as.character(area)))
  
  # 1d now make it not spatial, we not longer need geometry 
  intersec <- as.data.frame(intersec)
  
  # 1e fix variable names for the Feature layer, do the 
  intersec <- intersec %>% 
    rename(var = !!FeatureVar)
  
  # 1f Calculate Weighted average
  # 1f.i calculate total area
  intersec.grouped <- intersec %>%
    group_by(gridID) %>% 
    summarize(areaTot = sum(area))
  # 1f.ii add total area column to dataset
  intersec <- intersec %>% 
    inner_join(intersec.grouped, by = 'gridID') 
  # compute area-weighted average
  intersec.mean <- intersec %>% 
    group_by(gridID) %>% 
    summarize(varMean = sum(var * area / areaTot)) %>% 
      rename(!!FeatureVar := varMean)
  # return dataframe 
  intersec.mean
}

# this takes over 30 minutes when I just od it... maybe better if it is outside 
# of function 
compute_areaWeightedAverage_Raster <- function(AggregationLayer, FeatureLayer){
  AggregationLayer <- baseGrid;  FeatureLayer <- prism; 
  
  # 1b Assign Gridid 
  AggregationLayer <- AggregationLayer %>% 
    mutate(gridID = row_number())
  # extract
  intersec.mean <- raster::extract(FeatureLayer, AggregationLayer, 
                               df = TRUE, factors = TRUE, fun = mean) 

  # Add grid id's 
  intersec.mean$gridID <- AggregationLayer$gridID
    # return dataframe 
  intersec.mean
}
