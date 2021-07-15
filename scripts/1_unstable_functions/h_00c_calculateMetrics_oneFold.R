# Join Input Models 
# Prepare BNE Inputs
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Define Function
# 2: Calculate Metrics 

####**************
#### N: Notes ####
####**************

# Right now we are using training and avgscm prediction datasets inherited 
# from the Capstone students' project, so the code it not yet 100% reproducible

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.rtf")
  source(here::here('scripts', 'a_set_up', "a_00_setUp_env.R"))
}

####******************************************
#### 1: Define Function to Create Parcels ####
####******************************************

# 1a Name function
calculateMetrics_oneFold <- function(YYYY, kScale, activeFold){
  #YYYY <- 2010; kScale <- 3.5; activeFold <- 'fold01'
    
  # 1b First, readin the BNE outputs 
  InputSet <- c('AV', 'GS', 'CM', 'JS', 'CC')
  ColNames <- c('lat', 'lon',paste0('w_mean', '_', InputSet),
                paste0('w_sd', '_', InputSet), 'bias_mean', 'bias_sd', 
                'pred_mean', 'pred_sd', 'pred_05CI', 'pred_95CI', 
                'pred_min', 'pred_max', 'pred_median')
  
  
  # 1c Readin the output of interest
  RunID <- paste0(YYYY, '_avgscmjscc_', kScale, '_', activeFold )
  output <- read_csv(here::here('BNE_Outputs/annual',
                                paste0(RunID, '.csv')), 
                     col_names = ColNames) %>%
    mutate(RunID = RunID, fold = activeFold)
  
  # 1d Then readin the AQS test dataset (called pred in this case. )
  truth <- read_csv(here::here('data_input_models', 'combined', 'annual',
                               paste0('Predictions_', YYYY, '_' , 'avgscmjscc', '_', activeFold, '.csv')))
  
  # 1e Combine predictions and truth
  # since some rounding can occur, we can't just use inner_join() 
  # 1e.i Convert to simple features
  output <- output %>%
    st_as_sf(., coords = c("lon", "lat"), crs=st_crs('epsg:4326')) %>% 
    st_transform(., crs=st_crs(projString)) 
  truth <- truth %>%
    dplyr::select(lat, lon, aqs) %>% 
    st_as_sf(., coords = c("lon", "lat"), crs=st_crs('epsg:4326')) %>% 
    st_transform(., crs=st_crs(projString)) 
  # 1e.ii Join via nearest neighbor
  truth$cellID  <- unlist(st_nn(truth, output, k = 1, returnDist = FALSE))
  output <- output %>% 
    as.data.frame() %>%
    mutate(cellID = row_number())
  dta <- output %>% 
    inner_join(truth, by = 'cellID')
  
  # 1f Fit regression
  mod <- lm(aqs~pred_mean, data = dta)
  
  # 1g Calculate metrics 
  # we store as a dataframe like this so that, if the test datasets have difference sizes 
  # we can weight the metrics from each test set according to its size 
  dta <- dta %>% 
    mutate(E = pred_mean - aqs) %>%
    mutate(SE = E^2) %>% 
    mutate(corr =  cor(dta$aqs, dta$pred_mean), 
           slope = summary(mod)$coef[2,1], 
           Rsq = summary(mod)$r.squared, 
           cover = if_else(aqs > pred_05CI & aqs < pred_95CI, 1, 0)) %>% 
    dplyr::select(E, SE, corr, slope, Rsq, cover, 
                  aqs, pred_mean, pred_median, pred_05CI, pred_95CI, pred_sd, 
                  pred_min, pred_max)
  
  # 1h Return result 
  return(dta)

}
