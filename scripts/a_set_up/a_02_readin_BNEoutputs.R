# Spatial Join of Data 
# BNE Crude Error Assessment 
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# N: Notes
# 0: Preparation 
# 1: Make CONUS Outline

####********************
#### 0: Preparation ####
####********************

# 0a Load package required for this script
if(!exists("Ran_0_00")){
  # change this to something .md when I can 
  here::i_am("README.rtf")
  source(here::here('Scripts', '0_set_up',
                    "0_00_setUp_env.R"))
}

Ran_b_01 <- "Ran_b_01"

####***************************
#### 1: Make CONUS Outline ####
####***************************

# 1a Begin loop over years 


# 1b Identify the runs / outputs within that year 
myRuns <- list.files(
  path = here::here('BNE_Outputs/annual'))

  # 1c run the readin function of each of them
#for (i in 1:length(myRuns)){
readin_BNEoutput <- function(RunFile){ 
  # RunFile <- myRuns[1]
  RunID <- str_sub(RunFile, start = 0, end= -5) 
  
    YYYY  <- str_split_fixed(RunID, '_', 5)[1,1]
    InputStr <- str_split_fixed(RunID, '_', 5)[1,2]
    ScaleK <- str_split_fixed(RunID, '_', 5)[1,3] 
    foldMethod <- str_split_fixed(RunID, '_', 5)[1,4]
    fold <- str_split_fixed(RunID, '_', 5)[1,4] 
    
    # Make input set
    InputSet <- {}
    for (j in seq(1,str_length(InputStr), 2)){
      InputSet <- c(InputSet, str_sub(InputStr,j, j+ 1))
    }
    
    # 1c Set columns Names 

    ColNames <- c('lat', 'lon',paste0('w_mean', '_', InputSet),
                  paste0('w_sd', '_', InputSet), 'bias_mean', 'bias_sd', 
                  'pred_mean', 'pred_sd')
    
    # 1c Readin the output of interest
    output <- read_csv(here::here('BNE_Outputs/annual',
                                  paste0(RunID, '.csv')), 
                       col_names = ColNames) %>%
      mutate(RunID = RunID, foldMethod = foldMethod, fold = fold)
    return(output)
}


BNEout <- map(myRuns, readin_BNEoutput) %>%
  bind_rows()

# Clean up environment 
#rm(myRuns, i, InputStr, AllInputSet, dta)

# calcualate highest weight
max2 <- function(x){
  max(x, na.rm = TRUE)
}

BNEout$MaxW <- apply(dplyr::select(BNEout, contains('w_mean')),
                     1, max2 )


BNEout <- BNEout %>% 
  mutate(HighWI = case_when(
    MaxW == w_mean_AV ~ 'AV', 
    MaxW == w_mean_GS ~ 'GS', 
    MaxW == w_mean_CM ~ 'CM', 
    MaxW == w_mean_JS ~ 'JS', 
    MaxW == w_mean_CC ~ 'CACES')) %>% #, 
    #MaxW == w_mean_CA ~ 'CAMS', 
   #MaxW == w_mean_ME ~ 'MERRA1.4')) %>% # , 
   # MaxW == w_mean_NN ~ 'NN', 
    
    #MaxW == w_mean_BG ~ 'Big', 
    #MaxW == w_mean_RD ~ 'Random')) %>% 
  mutate(HighWI = factor(HighWI, 
                         c('AV', 'GS', 'CM', 'JS', 'CACES', 'MERRA1.4',
                           'MERRA2.0','CAMS', 'NN','Big','Random')))

