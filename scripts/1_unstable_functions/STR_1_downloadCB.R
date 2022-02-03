# File: STR_1_downloadCB.R
# Author: Sebastian T. Rowland <sr3463@cumc.columbia.edu>
# Date: 02/02/2021

#' \code{downloadCB} downloads daily predictions for 1 year for 1 h3_3 from 
#' Cole Brokamp's model
#' 
#' @param h3_3 string. the level 3 hex to read in 
#' @param yyyy string or numeric of the year to read in  
#' 
#' @return either the prediction dataset or the name of the missing file. 
#' 
#' @export
#' @importFrom magrittr %>%
#' 

downloadCB <- function(h3_3, yyyy) {
  # make the file name 
  cb_file_name <- paste0(h3_3, '_', yyyy, 'h3pm.fst')
  out <- tryCatch(
    {

      # here we download the data for a specific location 
      dta.pm <- s3::s3_get_files(cb_file_name, 
                                 confirm = FALSE, 
                                 download_folder =
                                   here::here('inputs', 'pm25', 'base_models', 
                                              'raw'))
    },
    # error condition for missing URLs
    error=function(cond) {
      message(paste("URL for predictions at that site does not seem to exist:"))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(cb_file_name)
    },
    
    finally={
    }
  )    
  return(out)
}