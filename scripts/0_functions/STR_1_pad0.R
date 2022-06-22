# File: STR_1_pad0.R
# Author: Sebastian T. Rowland <sr3463@cumc.columbia.edu>
# Date: 12/21/2021

#' \code{pad0} takes a number and adds a left 0 pad if needed. Used to add leading 
#' zeros to months and days for consistent formatting.
#' 
#' @param x the number to pad
#' 
#' @return a string with a leading zero, if needed
#' 
#' @export
#' 

pad0 <- function(x){ stringr::str_pad(x, 2, 'left', '0')}
  