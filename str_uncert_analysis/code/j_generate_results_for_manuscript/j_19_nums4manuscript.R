# File: j_40_nums4manuscript.R
# BNE Annual Uncertainty Analysis
# Authors:
# Sebastian Rowland <sr3463@cumc.columbia.edu>
# Date: 10/04/2022
#
# Contents:
#  N. notes
#  0. preparation
#  a. abstract
#  r1. BNE inputs
#  r2. grid search results 
#  r3. BNE general results
#  r4. uncertainty analysis

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

#### ------------- ####
####  a. abstract  ####
#### ------------- ####




#### ---------------- ####
####  r1. BNE inputs  ####
#### ---------------- ####

# r1.a number training observations
# bring in training data 
train <- read_csv(here::here('inputs', 'pm25', 'training_datasets', 'annual_combined', 
                             'training_cvfolds.csv'))
r1.a.noObs <- nrow(train)

# r1.b num sites
train <- train %>% mutate(loc = paste0(lat, '-', lon))
r1.b.numSites <- length(unique(train$loc))

# r1.c mean conc
r1.c.concMean <- mean(train$obs)

# r1.d sd conc 
r1.d.concSD <- sd(train$obs)

# r1.e min meanpred 
tab1 <- read_csv(here::here(dir.proj, 'manuscript', 'table2_dist_main_vars.csv'))
baseM <- tab1 %>% 
  filter(varName != 'aqs' & !str_detect(varName, 'BNE'))
r1.e.minBaseMMean <- min(baseM$mean)

# r1.f max meanPred 
r1.f.maxBaseMMean <- max(baseM$mean)

# r1.g min SD pred 
r1.g.minBaseMSD <- min(baseM$sd)

# r1.h max SD pred 
r1.h.maxBaseMSD <- max(baseM$sd)

# compile 
r1 <- list(numObs = r1.a.noObs, 
           numSites = r1.b.numSites, 
           meanConc = r1.c.concMean, 
           concSD = r1.d.concSD, 
           minMeanPred = r1.e.minBaseMMean, 
           maxMeanPred = r1.f.maxBaseMMean, 
           minSDPred = r1.g.minBaseMSD, 
           maxSDPred = r1.h.maxBaseMSD)
# look at results 
r1

#### ------------------------- ####
####  r2. grid search results  ####
#### ------------------------- ####

# r2.a rmse improvement 
# r2.a.i bring in grid search results 
gridSearch <- read_csv(here::here(dir.proj,  'manuscript', 'eTable4_grid_search.csv'))

# r2.a.ii rank by coverage 
gridSearch <- gridSearch %>% 
  arrange(desc(cover))
# r2.a.iii comute percent change 
r2.a.rmseGain <- (gridSearch$rmse[8] - gridSearch$rmse[1])/ gridSearch$rmse[1]

# r2.b r2 improvement 
r2.b.r2Gain <- (gridSearch$r2[8] - gridSearch$r2[1])/ gridSearch$r2[1]

# r2.c coverage loss 
r2.c.coverLoss <- (gridSearch$cover[8] - gridSearch$cover[1])/ gridSearch$cover[1]

# r2.d best rmse 
# bring in full grid search 
gridSearch.full <- read_csv(here::here(dir.proj, outPath, 'b_description_bne_grid_search', 
                                        'grid_search.csv'))

r2.d.rmseGainFull <- (gridSearch$rmse[8] - min(gridSearch.full$rmse))/ min(gridSearch.full$rmse)

# r2.e best r2 
r2.e.r2GainFull <- (gridSearch$r2[8] - max(gridSearch.full$r2))/ max(gridSearch.full$r2)

# r2.d final rmse 
r2.d.winnerRMSE <- gridSearch$rmse[8]

# r2.e final r2 
r2.e.winnerR2 <- gridSearch$r2[8]

# r2.f final cover
r2.f.winnerCover <- gridSearch$cover[8]

# r2.g EV rmse 
# r2.g.i bring in the external validation results 
ppd.ev <- read_csv(here::here(dir.proj, 'bne_ppd', ppdPath, 
                           'ev_2_0-5_2_0-5_0-5_0-0498_0-1353.csv'))
preds <- ppd.ev %>% 
  dplyr::select(starts_with('pred_')) 
ppd.ev$pred_min <- apply(preds, 1, min)
ppd.ev$pred_max <- apply(preds, 1, max)

ppd.ev <- ppd.ev %>% 
  mutate(obs_within_range = if_else(obs >= pred_min & obs <= pred_max, 1, 0))
mean(ppd.ev$obs_within_range)
# 22.g.ii bring in EV evaluation 
ev <- read_csv(here::here(dir.proj, 'manuscript', 
                          'eTable4_external_validation.csv'))

ev.baseM <- ev %>% 
  filter(model != 'BNE')
# r2.i minimal rmse of base models 
r2.g.RMSErange <- c(min(ev.baseM$rmse), max(ev.baseM$rmse))
r2.h.RMSEmedian <- median(ev.baseM$rmse)
r2.i.MErange <- c(min(ev.baseM$me), max(ev.baseM$me))
r2.j.MEmedian <- median(ev.baseM$me)
r2.k.r2range <- c(min(ev.baseM$r2), max(ev.baseM$r2))
r2.l.r2median <- median(ev.baseM$r2)
r2.m.sloperange <- c(min(ev.baseM$slope), max(ev.baseM$slope))
r2.n.slopemedian <- median(ev.baseM$slope)

# r2.g.iii actually calculate rmse
r2.o.bneRMSE <- ev$rmse[ev$model == 'BNE']

# r2.h ev ME
r2.p.bneME <- ev$me[ev$model == 'BNE']

# r2.i ev ME sd
r2.q.bneME_sd <- ev$me_sd[ev$model == 'BNE']

# r2.i ev ME sd
r2.r.bner2 <- ev$r2[ev$model == 'BNE']

# calculate cover 
ppd.ev <- ppd.ev %>% 
  mutate(cover = if_else(obs >= y_95CIl & obs < y_95CIu, 1, 0))
r2.s.bne_cover = c(sum(ppd.ev$cover), round(100* mean(ppd.ev$cover), 2))

# r2.i ev ME sd
r2.t.bneSlope <- ev$slope[ev$model == 'BNE']

# we want corr between base model weights and stuff 
# r2.g.ii compute global RMSE
ppd.ev <- ppd.ev %>% 
  mutate(err_av = obs - pred_av, 
         err_cc = obs - pred_cc,
         err_cm = obs - pred_cm,
         err_gs = obs - pred_gs,
         err_js = obs - pred_js,
         err_me = obs - pred_me,
         err_rk = obs - pred_rk,
         err_bne = obs - y_mean, 
         se_bne = (obs- y_mean)^2) %>% 
  mutate(se_tot = err_av^2 + err_cc^2 + err_cm^2 + err_gs^2 + err_js^2 + 
           err_me^2 + err_rk^2) %>% 
  mutate(rel_sq_err_av = err_av^2 / se_tot, 
         rel_sq_err_cc = err_cc^2 / se_tot, 
         rel_sq_err_cm = err_cm^2 / se_tot, 
         rel_sq_err_gs = err_gs^2 / se_tot, 
         rel_sq_err_js = err_js^2 / se_tot, 
         rel_sq_err_me = err_me^2 / se_tot, 
         rel_sq_err_rk = err_rk^2 / se_tot)



r2.u.weights_se_corr <- cor(
  c(ppd.ev$rel_sq_err_av^2, ppd.ev$rel_sq_err_cc^2, ppd.ev$rel_sq_err_cm^2, ppd.ev$rel_sq_err_gs^2,
    ppd.ev$rel_sq_err_js^2, ppd.ev$rel_sq_err_me^2, ppd.ev$rel_sq_err_rk^2), 
  c(ppd.ev$w_mean_av, ppd.ev$w_mean_cc, ppd.ev$w_mean_cm, ppd.ev$w_mean_gs,
    ppd.ev$w_mean_js, ppd.ev$w_mean_me, ppd.ev$w_mean_rk))

# compile
r2 <- list(rmseGain = r2.a.rmseGain, 
           numSites = r2.b.r2Gain, 
           coverLoss = r2.c.coverLoss, 
           winnerRMSE = r2.d.winnerRMSE, 
           winnerR2 = r2.e.winnerR2, 
           winnerCover = r2.f.winnerCover, 
           RMSErange = r2.g.RMSErange, 
           RMSEmedian = r2.h.RMSEmedian, 
           MErange = r2.i.MErange, 
           MEmedian  = r2.j.MEmedian, 
           r2range = r2.k.r2range, 
           r2median = r2.l.r2median, 
           sloperange = r2.m.sloperange,
           slopemedian = r2.n.slopemedian,
           bneRMSE = r2.n.bneRMSE, 
           bneME = r2.o.bneME, 
           bneME_sd = r2.p.bneME_sd,
           bner2 = r2.q.bner2,
           bne_cover = r2.r.bne_cover,
           bneSlope = r2.s.bneSlope,
           m.weights_se_corr = r2.t.weights_se_corr)
# look at results 
r2

#### ------------------------- ####
####  r3. BNE general results  ####
#### ------------------------- ####

# r3.a. RP mean 
# r3.a.i function to read in the data
readAssignedPPD <- function(yyyy) {
  bne.ppd <- fst::read_fst(here::here(dir.proj, 'data', 'ppd_assigned', 'annual', 
                                      paste0('bnePPD_expVar_', yyyy, '.fst'))) %>% 
    mutate(time = yyyy, cell_id = row_number()) %>% 
    dplyr::select(lat, lon, time, ens_mean, ens_sd, rp_mean, rp_sd, y_mean, y_sd, 
                  region)
}
# r3.a.ii bring in all the years of assigned ppd
bne.ppd <- map_dfr(2010:2015, readAssignedPPD)
# r3.a.iii compute mean 
r3.a.meanRP <- mean(bne.ppd$rp_mean)

# r3.bcompute mean 
r3.b.sdRP <- sd(bne.ppd$rp_mean)

# r3.c mean conc 
r3.c.meanPM25 <- mean(bne.ppd$y_mean)

# r3.d SD conc 
r3.d.sdPM25 <- sd(bne.ppd$y_mean)

# r3.e max conc 
r3.e.maxPM25 <- max(bne.ppd$y_mean)

# r3.f loc and time of max value 
r3.f.max.loc <- c(
  bne.ppd$lat[bne.ppd$y_mean == r3.e.maxPM25], 
  bne.ppd$lon[bne.ppd$y_mean == r3.e.maxPM25], 
  bne.ppd$time[bne.ppd$y_mean == r3.e.maxPM25]
)

# r3.g mean in 2010 north west coast 
bne2010nwc <- bne.ppd %>% 
  filter(time == 2010 & region == 'Region10')
r3.g.meanPMNWCoast2010 <- mean(bne2010nwc$y_mean)

# r3.h mean in 2015 north west coast 
bne2015nwc <- bne.ppd %>% 
  filter(time == 2015 & region == 'Region10')
r3.h.meanPMNWCoast2015 <- mean(bne2015nwc$y_mean)

# compile
r3 <- list(meanRP = r3.a.meanRP, 
           sdRP = r3.b.sdRP, 
           meanPM25 = r3.c.meanPM25,
           sdPM25 = r3.d.sdPM25, 
           maxPM25 = r3.e.maxPM25,
           max.loc = r3.f.max.loc,
           meanPMNWCoast2010 = r3.g.meanPMNWCoast2010, 
           meanPMNWCoast2015 = r3.h.meanPMNWCoast2015)
# look at results 
r3


#### -------------------------- ####
####  r4. uncertainty analysis  ####
#### -------------------------- ####

# r4.a max y_sd 
r4.a.max_ySD <- max(bne.ppd$y_sd)

# r4.b loc and time of max y_sd 
r4.b.max_ySD.loc <- c(
  bne.ppd$lat[bne.ppd$y_sd == r4.a.max_ySD], 
  bne.ppd$lon[bne.ppd$y_sd == r4.a.max_ySD], 
  bne.ppd$time[bne.ppd$y_sd == r4.a.max_ySD]
)

r4.c.cor_ySD_ensSD <- cor(bne.ppd$y_sd, bne.ppd$ens_sd, method = 'spearman')
r4.d.cor_ySD_rpSD <- cor(bne.ppd$y_sd, bne.ppd$rp_sd, method = 'spearman')

# r4.b loc and time of max y_sd _scaled
bne.ppd <- bne.ppd %>% 
  mutate(y_sd_scaled = y_sd/ y_mean)
r4.c.max_ySDScaled <- max(bne.ppd$y_sd_scaled)

# r4.b loc and time of max y_sd 
r4.d.max_ySDScaled.loc <- c(
  bne.ppd$lat[bne.ppd$y_sd_scaled == r4.c.max_ySDScaled], 
  bne.ppd$lon[bne.ppd$y_sd_scaled == r4.c.max_ySDScaled], 
  bne.ppd$time[bne.ppd$y_sd_scaled == r4.c.max_ySDScaled]
)

# r.5 change in scaled uncert for pacfici northwest 
bne.2010.pnw <- bne.ppd %>% filter(region == 'Region10' & time == 2010)
bne.2015.pnw <- bne.ppd %>% filter(region == 'Region10' & time == 2015)

r.5.scaledUncertChange <- c(mean(bne.2010.pnw$y_sd_scaled), sd(bne.2010.pnw$y_sd_scaled),
         mean(bne.2015.pnw$y_sd_scaled), sd(bne.2015.pnw$y_sd_scaled))

r4.c.cor_ySDScaled_ensSD <- cor(bne.ppd$y_sd_scaled, bne.ppd$ens_sd, method = 'spearman')
r4.d.cor_ySDScaled_rpSD <- cor(bne.ppd$y_sd_scaled, bne.ppd$rp_sd, method = 'spearman')

# compile
r4 <- list(max_ySD  = r4.a.max_ySD, 
           max_ySD.loc = r4.b.max_ySD.loc,
           cor_ySD_ensSD = r4.c.cor_ySD_ensSD, 
           cor_ySD_rpSD = r4.d.cor_ySD_rpSD, 
           max_ySDScaled = r4.c.max_ySDScaled, 
           max_ySDScaled.loc = r4.d.max_ySDScaled.loc, 
           scaledUncertChange = r.5.e.scaledUncertChange, 
           cor_ySDScaled_ensSD = r4.c.cor_ySDScaled_ensSD, 
           cor_ySDScaled_rpSD = r4.d.cor_ySDScaled_rpSD)
# look at results 
r4
