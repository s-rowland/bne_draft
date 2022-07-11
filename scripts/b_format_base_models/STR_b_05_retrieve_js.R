# Robbie: A brief description of the script would go well here

pad0 <- function(x){ stringr::str_pad(x, 2, 'left', '0')}

library(foreach)

# Robbie: Having seen this routine a couple of times in these scripts might it be worth having a source file for the below and elsewhere throughout?
#yyyy <- 2016
# 0.e set up parallelization
# 0.e.i get the number of cores
# we subtract one to reserve a core for non-lbic tasks
n.cores <- parallel::detectCores() - 1
# 0.e.ii create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "FORK"
)
#check cluster definition (optional)
print(my.cluster)

# 0.e.iii register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
#check if it is registered (optional)
foreach::getDoParRegistered()

# Robbie: A brief description here and for all of the below would be useful
prefix <- "https://beta.sedac.ciesin.columbia.edu/downloads/data/aqdh/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016-"
suffix <- "-rds.zip"
wd <- paste0(here::here('inputs', 'pm25', 'base_models', 'daily', 'raw', 'js'), '/')


library(magrittr)
time.tab <- tidyr::expand_grid(yyyy = c(2005, 2004), 
                        mm= pad0(1:12)) %>%
 dplyr:: mutate(infix = paste0(yyyy, mm))


Sys.time()
ee <- foreach(
  infix = time.tab$infix, 
  .combine = 'rbind'
) %do% {
  #infix <- paste0(yyyy, mm)
  list.files(wd)
  # 3E.a. download js if needed
  if  ( !(infix %in% list.files(wd))) {
    
    # 3E.a.i. name of monthly file
    url <- paste0(prefix, infix, suffix)
    zipfile <- paste0(wd, infix, ".zip")
    download <- paste0("curl -o ", zipfile, " -b ~/.urs_cookies -c ~/.urs_cookies -L -n ", url)
    # 3E.a.ii. do the download
    system(download)
    # 3E.a.iii unzip the file
    unzip(zipfile, exdir = paste0(wd, infix))
  }
  
  g =1 
  g
}

