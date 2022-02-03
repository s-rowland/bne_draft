# File: STR_1_joinCB.R
# Author: Sebastian T. Rowland <sr3463@cumc.columbia.edu>
# Date: 12/21/2021

# this function can accept BNE run from any BNE model 
# note that the legend scales will be specific to this run, but 
# similar parameters (e.g., the weights) will follow the same scale

#' \code{summarizeBNErun} creates a pdf with many plots to summarize the BNE results 
#' Useful for reviewing model for main findings and coherent results
#' 
#' @param bneOut a dataframe of the BNE outputs
#' @param location string determining the location to plot. For now used to plot
#' specific cities 
#' @param targetDir string of directory to save the results
#' @param targetFile string of file name, 'default targetFile' uses the runID
#' @param dropData string determining if you drop 75% of the data to quickly generate the plot
#' helpful for conus-wide results which can take ~ 3 minutes to print
#' 
#' @return a printed pdf with many plots showing the bne results
#' 
#' @export
#' @importFrom magrittr %>%
#' 

# 1. define function to assign one h3_3 hexagon of cb data
assignCB <- function(pred.oneH3) {
  
  read_fst(here::here('inputs', 'pm25', 'base_models', 'formatted', 
                      'cb_annual_formatted',  pred.oneH3$file_name[1])) %>% 
    inner_join(pred.oneHex, by = 'h3_3')
}

# 2. main function
joinCB<- function(dataset, temporalResolution) {
  
  # 2.a. create safe hex look up object 
  # this identifies some hexagons with funny names
  safe_hex_lookup <- read_rds(here::here('ancillary_data', 'raw', 'brokamp_objects', 
                                         'safe_hex_lookup.rds'))
  # okay next drop training locations without a safe hex 
  # 2.b. get list of safe hex 
  cbHexList <- list.files(here::here('inputs', 'pm25', 'base_models', 'raw', 'pm25-brokamp'))
  
  # 2.c. wrangle the data; rename variables and filter
  # for each point we need to determine the level 8 h3 (h3_8), and the parent H3 
  preds <- preds %>% 
    mutate(h3 = suppressMessages(h3jsr::point_to_h3(dplyr::select(., lon, lat), res = 8))) %>%
    mutate(h3_3 = h3jsr::get_parent(h3, res = 3)) %>%
    mutate(year = 2010) %>%
    dplyr::left_join(safe_hex_lookup, by = 'h3_3') %>%
    dplyr::mutate(h3_3 = ifelse(!is.na(safe_hex), safe_hex, h3_3)) %>%
    dplyr::select(-safe_hex) %>% 
    mutate(file_name = paste0(h3_3, "_", 2010, "_h3pm.fst")) 
  
  # 2.d. keep only training locations within a safe hex
  preds.safeHex <- preds %>% 
    filter(file_name %in% cbHexList)
  pred.list <- split(preds.safeHex, preds.safeHex$file_name)
  pred.oneHex <- pred.list[[1]]
  
  # 2.d map it 
  pred.cb <- purrr::map_dfr(pred.list, assignCB)
  
}