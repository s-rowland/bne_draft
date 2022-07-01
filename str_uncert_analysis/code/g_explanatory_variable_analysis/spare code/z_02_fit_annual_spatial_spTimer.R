# spBayes Tutorial
# Nationwide Application
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 
# Sept 12, 2021

####-----------------------
#### Table of Contents ####
####-----------------------

# 0: Preparation 
# 1: Read BNE-factor Data
# 2: Set Ingredients of Model
# 3: Fit Model 
# 4: Examine Model

####--------------------
#### 0: Preparation ####
####--------------------

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

# 0b Load spatial package 
p_load(spTimer)

# 0d Set seed 
# from tutorial
set.seed(1)

####-----------------------------
#### 1: Read BNE-factor Data ####
####-----------------------------

# 1a Read Data
readCombinedData <- function(YYYY){
  read_fst(here::here(dir.proj, 'data', 'processed', 
                      paste0('AVGSCMJSCC_3.5_explanatory_', YYYY, '_025deg.fst'))) %>% 
    mutate(YYYY = YYYY)
}
dta <- map(2010:2015, readCombinedData) %>% 
  bind_rows()
rm(readCombinedData)

dta <- dta %>% 
  dplyr::select(-cell_id) %>% distinct()

# 1b Convert units of population density to thousand ppl per km 
dta <- dta %>% 
  mutate(pop_density_1kper1000km2 = pop_density*1000000 / 1000) %>% 
  mutate(lat = as.numeric(as.character(lat)), 
         lon = as.numeric(as.character(lon))) 

# remove rows with missing variables
dta <- dta %>% 
  filter(complete.cases(dta))

# Calculate distance to each near monitor
dta <- dta %>% 
  mutate(mon_dist1 = monDist1.mean, 
         mon_dist2 = 2*monDist2.mean-monDist1.mean, 
         mon_dist3 = 3*monDist3.mean-2*monDist2.mean, 
         mon_dist4 = 4*monDist4.mean-3*monDist3.mean, 
         mon_dist5 = 5*monDist5.mean-4*monDist4.mean)


# fit gam model 
# takes about 1 minutes
mod.allPsp <- gam(pred_sd ~ s(pred_mean) + s(mon_dist1)  +  s(winter_temp) +
                    s(summer_temp) + s(pop_density_1kper1000km2) + s(cloud_cover) + 
                    s(elev) , data = dta)
summary(mod.allPsp)$s.table[,1]

# population density is still linear

#-----------------------------------------------------#
#### 2: check for autocorrelation of the residuals #### 
#-----------------------------------------------------#

# okay, now we want to check for autocorrelation of the residuals 
dta$residuals <- residuals(mod.allPsp)
# okay, copy the code from page 137 of the spatiotemporal book 

# Moran's i for spatial' Durbin-Watson for temporal

#-----------------------------------#
#### 3: fit spatiotemporal model #### 
#-----------------------------------#

dta2 <- dta %>% 
  dplyr::filter(state == 'TX')
dta2 <- dta %>% 
  filter(YYYY == 2010 | YYYY == 2011) %>% 
  filter(region %in% c(1,2,3))

dta2 <- dta2 %>% 
  arrange(lat) %>% 
  arrange(lon)

time.part <- spT.time(t.series =2, segments = 1)

dta.coords <-  dta2 %>% 
  dplyr::select(lat, lon) %>% 
  distinct()

# takes about 3 minutes for just colordao
mod <- spT.Gibbs(pred_sd ~ mon_dist1  +   pred_mean + winter_temp ,
                 data = dta2, 
                 model = "AR", 
                 time.data = time.part,
          coords = dta.coords) 

# additional options 
         # knots.coords, newcoords = NULL, newdata = NULL, priors = NULL,
        #  initials = NULL, nItr = 5000, nBurn = 1000, report = 1, tol.dist = 0.05,
        #  distance.method = "geodetic:km", cov.fnc = "exponential",
        #  scale.transform = "NONE", annual.aggrn = "NONE",
        #  spatial.decay = spT.decay(distribution = "FIXED"))
