# File: process_JS.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 8/19/21
#
# https://beta.sedac.ciesin.columbia.edu/data/set/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016/data-download#close


years <- c(2010:2016)
months <- stringr::str_pad(1:12, 2, "left", "0")
prefix <- "https://beta.sedac.ciesin.columbia.edu/downloads/data/aqdh/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016-"
suffix <- "-rds.zip"
wd <- "~/Downloads/"

#### --------------- ####
#### 1. DOWNLOAD ZIP ####
#### --------------- ####

infix <- paste0(years[1], months[1])
url <- paste0(prefix, infix, suffix)
zipfile <- paste0(wd, infix, ".zip")

download <- paste0("curl -o ", zipfile, " -b ~/.urs_cookies -c ~/.urs_cookies -L -n ", url)

system(download)

#### --------------- ####
####  2. UNZIP FILES ####
#### --------------- ####

unzip(zipfile, exdir = paste0(wd, infix))
