# File: j_07_eTable1.R
# BNE Annual Uncertainty Analysis
# Authors:
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 10/04/2022
#
# Contents:
#  N. notes
#  0. preparation
#  1. bring in data
#  2. create table

#### ------------------ ####
####       N. notes     ####
#### ------------------ ####


#### ---------------- ####
####  0. preparation  ####
#### ---------------- ####

# 0.a. load packages, etc
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

# 0.b. load objects and packages specific for this work
if(!exists('ran_a_00_uncert')){
  here::i_am('README.md')
  source(here::here('str_uncert_analysis', 'code', 
                    '0_00_config_env_uncert_analysis.R'))
}

# 0.c. loac objects for generating plots
if(!exists('ran_j_00')){
  here::i_am('README.md')
  source(here::here('str_uncert_analysis', 'code', 'j_generate_results_for_manuscript',
                    'j_00_set_plotting_features.R'))
}

#### ------------------ ####
####  1. bring in data  ####
#### ------------------ ####

# 1.a. function to read in the data
readAssignedPPD <- function(yyyy) {
  bne.ppd <- fst::read_fst(here::here(dir.proj, 'data', 'ppd_assigned', ppdPath, 
                                      paste0('bnePPD_expVar_', yyyy, '.fst'))) %>% 
    mutate(time = yyyy, cell_id = row_number()) 
}

# 1.b. bring in all the years of assigned ppd
bne.ppd <- map_dfr(2010:2015, readAssignedPPD)

bne.ppd.time <- bne.ppd
# rename variables 
bne.ppd <- bne.ppd %>% 
  dplyr::select(starts_with('pred_')) %>%
  renameBaseM()

#### ----------------------- ####
####  2. overall correlation ####
#### ----------------------- ####

# 2.a calculate correlations 
bne.corr <- cor(bne.ppd, method = 'spearman')

#### ------------------------ ####
####  3. spatial correlation  ####
#### ------------------------ ####

# 3.a make function for spearman correlation 
cor.spearman <- function(x) {
  cor(x, method = 'spearmean')
}

# 3.b get spatial correlations
bne.ppd.sp <- split(bne.ppd, bne.ppd.time$time) %>% 
  map(., cor) 

# 3.c get the average
bne.corr.sp <- (bne.ppd.sp[[1]] + bne.ppd.sp[[2]] + bne.ppd.sp[[3]] + 
  bne.ppd.sp[[4]] + bne.ppd.sp[[5]] + bne.ppd.sp[[6]])/6

#### ------------------------ ####
####  4. temporal correlation ####
#### ------------------------ ####

# 4.a get spatial correlations
# 4.a.i create location oclumn 
bne.ppd.time <- bne.ppd.time %>% 
  mutate(lat_lon = paste0(lat, lon)) 
# 4.a.ii create list of locations we will keep 
loc <- bne.ppd.time %>% 
  dplyr::select(lat_lon) %>% 
  distinct() 

# 4a.iii stratify data by location
bne.loc <- split(bne.ppd, bne.ppd.time$lat_lon)

# 4.b get the correlations
bne.corr.tmp <- cor(bne.loc[[1]]) / nrow(loc)
for (i in 2: nrow(loc)) {
  bne.corr.tmp <- bne.corr.tmp + cor(bne.loc[[i]], method = 'spearman') / nrow(loc)
  print(i)
}

#### --------------- ####
####  5. save plots  ####
#### --------------- ####

png(here::here(dir.proj, 'manuscript', 'eFig2a_cbaseM_corr_all.png'))
corrplot::corrplot(bne.corr, addCoef.col = 'black')
dev.off()
png(here::here(dir.proj, 'manuscript', 'eFig2b_cbaseM_corr_spatial.png'))
corrplot::corrplot(bne.corr.sp, addCoef.col = 'black')
dev.off()
png(here::here(dir.proj, 'manuscript', 'eFig2c_cbaseM_corr_temporal.png'))
corrplot::corrplot(bne.corr.tmp,  addCoef.col = 'black')
dev.off()
