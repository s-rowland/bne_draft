# File: STR_1_getAnnualAverageCMAQ.R
# Author: Sebastian T. Rowland <sr3463@cumc.columbia.edu>
# Date: 02/02/2022

#' \code{getAnnualAverageCMAQ} creates a pdf with many plots to summarize the BNE results 
#' Useful for reviewing model for main findings and coherent results
#' 
#' @param insOut string. Whether average original CMAQ (CMAQ INS) or CMAQ 
#' fused with AQs (CMAQ OUT). Possible values are 'ins' and 'out
#' @param yyyy string/numeric. the year over which to average
#' 
#' @return nothing; results are stored in folder
#' 
#' @export
#' @importFrom magrittr %>%
#' 

getAnnualAverageCMAQ <- function(insOut, yyyy) {
  
  # 1.a. read the cmaq dataset
  cmq <- readr::read_csv(here::here('inputs', 'pm25', 'base_models', 'raw', 
                                    paste0('cm', insOut, '_daily_raw'), 
                                    paste0(yyyy, '_pm25_daily_average.txt'))) %>% 
    janitor::clean_names()
  
  # 1.b. rename the variables according to the year
  if (yyyy < 2015){
    cmq <- cmq %>% 
      dplyr::rename(lat = latitude, lon = longitude, pred = pm25_daily_average_ug_m3) 
  } else if (yyyy >= 2015) {
    cmq <- cmq %>% 
      dplyr::rename(lat = latitude, lon = longitude, pred = prediction)
  }
  
  # 1.c. get the annual average at each location
  cmq.fips <- cmq %>%
    dplyr::group_by(lat, lon) %>% 
    dplyr::summarize(pred = mean(as.numeric(pred)))
  
  # 1.d. save as an fst
  cmq.fips %>% 
    fst::write_fst(
      here::here('inputs', 'pm25', 'base_models', 'formatted', 
                 paste0('cm', insOut, '_annual_formatted'),
                 paste0('cm', insOut, '_', yyyy, '_formatted.fst')))
}