# File: b_06_conduct_external_validation.R
# Authors:
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 03/17/22
#
# Contents:
#  N. notes
#  0. preparation
#  1. download PRISM data
#  2. calculate seasonal averages

#### -------------- ####
#### 0. preparation ####
#### -------------- ####

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_set_up_env.R"))
}

if(!exists("Ran_a_00_ISEE")){
  here::i_am("README.md")
  source(here::here('uncertainty_factor_analysis_ISEE', 'scripts', 
                    "a_00_set_up_env_ISEE.R"))
}


  
#### ------------------------ ####
####  1. download PRISM data  ####
#### ------------------------ ####

# 1.a begin loop
for (YYYY in 2010:2015){
  
# 1.b Set the folder path for the prism data 
prism_set_dl_dir(here::here(dir.proj,'data', 'explanatory_variables', 'raw', 
                            'PRISM'))

# 1.c Download the annual data 
get_prism_annual(type = "tmean", 
                 years = YYYY, 
                 keepZip = FALSE)

get_prism_annual(type = 'ppt', 
                 years = YYYY, 
                 keepZip = FALSE)

# 1.d Download the monthly data 
get_prism_monthlys(type = "tmean", 
                 years = YYYY, 
                 mon = c(1:12),
                 keepZip = FALSE)

#### -------------------------------- ####
####  2. calculate seasonal averages  ####
#### -------------------------------- ####

# Winter = Jan, Feb, Dec 
# Summer = Jun, july, aug

# 2.a define function to readin PRISM data 
# 2.a.i Set the folder path for the prism data 
readin_monthly_prism <- function(ActiveMon){
  # 2.a.ii Set the name for the file we will readin
  prismFilePath <- prism_archive_subset("tmean", "monthly", years = YYYY, 
                                        mon =ActiveMon)
  # 2.a.iii Make it a formal rather than descriptive file name
  prismFilePath <- pd_to_file(prismFilePath)
  # 2.a.iv Actually read in the data
  prism <- raster(prismFilePath)
}

# 2.b Read in the monthly prism data
prism_01 <- readin_monthly_prism(1)
prism_02 <- readin_monthly_prism(2)
prism_12 <- readin_monthly_prism(12)

prism_06 <- readin_monthly_prism(6)
prism_07 <- readin_monthly_prism(7)
prism_08 <- readin_monthly_prism(8)

# 2.c Put into lists
prism.winter.ls <- list(prism_01, prism_02, prism_12)
prism.summer.ls <- list(prism_06, prism_07, prism_08)

# 2.d Sum them 
prism.winter <- Reduce('+', prism.winter.ls, accumulate = TRUE)
prism.summer <- Reduce('+', prism.summer.ls, accumulate = TRUE)

# 2.e Get monthly average 
prism.winter <- prism.winter[[3]] / 3
prism.summer <- prism.summer[[3]] / 3

# 2.f Save 
writeRaster(prism.winter, here::here('uncertainty_factor_analysis_ISEE','data', 
                                     'intermediate', 'PRISM_seasonal', 
                                     paste0('winter_meanT_', YYYY, '.raster')))
writeRaster(prism.summer, here::here('uncertainty_factor_analysis_ISEE', 'data', 
                                     'intermediate', 'PRISM_seasonal', 
                                     paste0('summer_meanT_', YYYY, '.raster')))

# 2.g clean environment
rm(prism_01, prism_02, prism_12, prism_06, prism_07, prism_08, prism.winter.ls, 
   prism.summer.ls)
}

# 2.h Clean environment 
rm(list=ls(pattern='prism'))
