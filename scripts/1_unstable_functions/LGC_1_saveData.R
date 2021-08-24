# File: LGC_1_saveData.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 08/23/21

#' \code{saveData} saves a given data frame / tibble / table to some number of .csv files. 
#' Depending on what the user specifies, the data will be saved via one .csv file 
#' per day, per month, per year, or by some other category defined by the user. 
#'
#' @param df A data.frame, tibble, or data.table containing PM2.5 observations / predictions. 
#' @param dir A character giving the path to the directory you would like \code{df} saved to.
#' @param saveBy (Optional) A character vector of column names from \code{df} detailling how to group 
#' \code{df}, i.e. what to pass to \code{\link[dplyr]{group_by}}.
#'
#' @return A character vector giving the names of the .csv files created. 
#'
#' @seealso \code{\link{loadData}}
#'
#' @export
#' @importFrom magrittr %>%
saveData <- function(df, dir, saveBy = c("year", "month", "day")) {
  
  if (!endsWith(dir, "/")) dir <- paste0(dir, "/")
  dir.create(dir)

  paths <- tibble::as_tibble(df) %>%
    dplyr::group_by(dplyr::across({{saveBy}})) %>%
    dplyr::group_split() %>%
    purrr::map_chr(function(x) {
      filename <- x[, saveBy] %>% dplyr::slice_head() %>% paste(collapse = "-")
      path <- paste0(dir, filename, ".csv")
      readr::write_csv(x %>% dplyr::select(-!!saveBy), file = path)
      return(path)
    })

  paths

}