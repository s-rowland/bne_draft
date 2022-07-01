# Combine BNE Outputs
# Uncertainty Analysis for ISEE
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Combine BNE Outputs

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_set_up_env.R"))
}

if(!exists("Ran_a_00_conusApp")){
  here::i_am("README.md")
  source(here::here('str_app_conus_uncert', 'scripts', 
                    "a_00_set_up_env_conusApp.R"))
}

#------------------------------#
#### 1: Combine BNE Outputs ####
#------------------------------#

# 1a. combine
inputSet <- c('av', 'gs', 'cm', 'js', 'cc')
inputSetList <- list(inputSet, inputSet, inputSet, inputSet, inputSet, inputSet)
kernel_sp <- 3.5

bneOut <- purrr::pmap(list(2010:2015, 
                    inputSetList, 
                    rep(kernel_sp, 6),
                    rep('all', 6)), 
               readBNEoutput)  
  
# 1b. remove 60% of the data to shrink things down 
rm_bne <- function(dta){
  set.seed(250) 
  dta <- dta %>% 
    dplyr::slice_sample(prop=0.4)
}

bneOut <- purrr::map(bneOut, rm_bne) %>%
  bind_rows() 

# 1b. save 
bneOut %>% 
  fst::write_fst(here::here(dir.proj, 'BNE_outputs',
                       'BNEoutputs_combined.fst'))

# 1c. clean environment
rm(bneOut, rm_bne)
