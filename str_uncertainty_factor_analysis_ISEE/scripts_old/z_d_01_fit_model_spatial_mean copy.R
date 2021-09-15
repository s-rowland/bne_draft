# Compare Aditya and Lawrence Location Data
# BNE Crude Error Assessment 
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Wrangle Data
# 2: Identify Linear Relationships 
# 3: Check for Autocorrelation of the Residuals
# 4: Choose Type of Spatial Model 
# 5: Choose Most Informative Monitor-Proximity Metric
# 6: Confirm Spatial Term
# 7: Assess Explanatory Power
# 8: Visualize Associations

####********************
#### 0: Preparation ####
####********************

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

# 0b Read CONUS shape file 
conus <-   st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                              'conus.shp'))

####******************
#### 1: Read Data ####
####******************

# 1a Read Data
readCombinedData <- function(YYYY) {
  read_fst(here::here('uncertainty_factor_analysis_ISEE', 'data', 'processed', 
                      paste0('AVGSCMJSCC_3.5_explanatory_', YYYY, '.fst'))) %>% 
    mutate(YYYY = YYYY)
}
dta <- map(2010:2015, readCombinedData) %>% 
  bind_rows()

rm(readCombinedData)

# 1b Convert units of population density to thousand ppl per km 
dta <- dta %>% mutate(popD1kper1km = popD*1000000 / 1000) %>% 
  mutate(lat = as.numeric(lat), lon = as.numeric(lon))

# 1c Remove obs with missing data 
dta <- dta %>% 
  dplyr::select(-cell_id, -cellID) %>% 
  na.omit()

# 1d Average across years 
dta <- dta %>% 
  group_by(lat, lon) %>% 
  summarize_all(mean) %>% 
  ungroup() %>% 
  dplyr::select(-state, -region, -YYYY)

# 1e Scale all of the variables... yes! 
dta <- dta %>% 
  mutate_at(vars(-lat, -lon,  -stateNum), scale)

####**************************************
#### 2: Identify Linear Relationships ####
####**************************************

# 2a Fit GAMM with all psp terms 
tic()
mod.allPsp <- gamm(pred_sd ~ s(pred_mean) + s(monDist) + s(winterTemp) +
                     s(summerTemp) + s(popD1kper1km),
                   data = dta)
toc()
# 2b Review edf for potential linear terms 
summary(mod.allPsp$gam)

# No evidence for linear relationships, so we will stick with psp terms for now

####***************************************************
#### 3: Check for Autocorrelation of the Residuals ####
####***************************************************

# 3a Put the residuals in the dataframe 
dta$resid <- mod.allPsp$gam$residuals

# 3b Calculate Moran's I of the residuals 
# 3b.i Create distance matrix
dta.dists <- as.matrix(dist(cbind(dta$lon, dta$lat)))
# 3b.ii Compute the inverse of the distance
dta.dists.inv <- 1/dta.dists
# 3b.iii Make all of the diagonals (self-distance) zero
diag(dta.dists.inv) <- 0
# 3b.iv Calculate Moran's I 
Moran.I(dta$resid, dta.dists.inv)

# The spatial autocorrelation is significant, though not very high, so a spatial
# model is more appropriate

####*************************************
#### 4: Choose Type of Spatial Model ####
####*************************************









# currently, the lagarlm() and related functions/models cannot handle penalized splines 
# so we will first try conditional spatial autoregression model option from 
# glmm. 
# see https://stat.ethz.ch/R-manual/R-devel/library/nlme/html/corClasses.html
# I would say lagsarlm with natural splines with like 8 df each will be...
# sensitivity analyses
# fit model with spatial correlation 

# TODO: make a semivariogram to see which correlation structure best fits 
tic()
mod.corSp <- gamm(pred_sd ~ s(pred_mean) + s(monDist) + s(winterTemp) +
                     s(summerTemp) + s(popD1kper1km),
                   correlation = corSpatial(1, form = ~ lat + lon),
                  data = dta)
toc()

# okay, for now we will use a natural spline approach, with a whole bunch of df. 

# 4a Load the spdep package
p_load(spdep, spatialreg)

# 4b Extract the coordinates as a matric
coords <- dta %>% 
  dplyr::select(lat, lon) %>% 
  mutate(lat = as.numeric(lat), lon= as.numeric(lon)) %>% 
  as.matrix()

# 4c Identify the maximum distance between neighbors
# 4c.i Identify the nearest neighbor for each observation 
neigh_close <- knn2nb(knearneigh(coords, k=1),row.names=row.names(dta))
# 4c.ii Extract the distances
dists <- unlist(nbdists(neigh_close, coords))
# 4c.iii Identify the maximum distance
max_min <- max(dists)

# 4d Compute this thing
dist_nb_mm <- dnearneigh(coords, d1=0, d2=max_min*1.5, row.names=row.names(dta))
# 4e Change its format
oc_nbq_w_mm <- nb2listw(dist_nb_mm)

# 4f Fit our natural spline lm() model 
mod.allns <- lm(pred_sd ~ ns(pred_mean,8) + ns(monDist,8) + ns(winterTemp,8) +
                   ns(summerTemp,8) + ns(popD1kper1km,8),
                 data = dta)
dta$resid <- mod.allns$residuals

# 4g Test to see which type of spatial model best fits the data + model   
oc.lagrange <- lm.LMtests(mod.allns,
                          oc_nbq_w_mm, 
                          test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
oc.lagrange
# 
#
fit.err <-sacsarlm(pred_sd ~ ns(pred_mean,8) + ns(monDist,8) + ns(winterTemp,8) +
                      ns(summerTemp,8) + ns(popD1kper1km,8), 
                    dta, 
                    listw=oc_nbq_w_mm,  method="MC")
summary(fit.err)

# it looks like the error model has a good score. 
tic()
fit.err<-errorsarlm(pred_sd ~ ns(pred_mean,8) + ns(monDist,8) + ns(winterTemp,8) +
                      ns(summerTemp,8) + ns(popD1kper1km,8),
                    dta, oc_nbq_w_mm, 
                    method = 'Matrix',
                    etype="error")
toc()
# okay, do we still get the error for the errorsar model? 
#methods, such as "LU" or "Matrix"

####*********************************************************
#### 5: Choose Most Informative Monitor-Proximity Metric ####
####*********************************************************

# here we fit models with various poximity metrics, 
# and see which one yields highest AIc

####*****************************
#### 6: Confirm Spatial Term ####
####*****************************

# repeat section 4, but with the final selected term 

####*********************************
#### 7: Assess Explanatory Power ####
####*********************************

####*******************************
#### 8: Visualize Associations ####
####*******************************



####*******************************************************************************
#### 4: Make Models with Different Types of Spatial Dependency and Compare AIC ####
####*******************************************************************************

dta <- dta %>% sample_frac(0.5)
# clean up 
rm(dta.dists, dta.dists.inv)
# 4a Make model with a spatial autocorrelation structure 
# (SAR) 
# we can use the variogram to choose between corSpatial, corGaussian, etc
# okay now we are trying corSpatial, but we only have 20k observations
mod.spCorr <- gamm(pred_sd ~ s(pred_mean) + s(monDist) + s(monDist2to4) + s(winterTemp) +
              s(summerTemp) + s(popD1kper1km) + YYYY,
              random = list(state=~1), 
              correlation = corSpatial(1, form = ~ lat + lon| YYYY),
            data = dta)
# we get error: 
#Error in chol.default(V$V[[i]]) : 
#  the leading minor of order 2 is not positive definite
#In addition: Warning message:
#  In Initialize.corSpher(X[[i]], ...) :
#  initial value for 'range' less than minimum distance. Setting it to 1.1 * min(distance)

# try it 
# without + YYYY 
mod.spCorr <- gamm(pred_sd ~ s(pred_mean) + s(monDist) + s(monDist2to4) + s(winterTemp) +
                     s(summerTemp) + s(popD1kper1km),
                   random = list(state=~1), 
                   correlation = corSpatial(1, form = ~ lat + lon| YYYY),
                   data = dta)
# withouy | YYYY 
# we get the same errors. Yeah, I don't think trying to do spatial while you 
# have multiple obs per location will actually work. 

mod.spCorr <- gamm(pred_sd ~ s(pred_mean) + s(monDist) + s(monDist2to4) + s(winterTemp) +
                     s(summerTemp) + s(popD1kper1km) + YYYY,
                   random = list(state=~1), 
                   correlation = corSpatial(1, form = ~ lat + lon),
                   data = dta)
# okay, here we go: 
# ' cannot have zero distances in 'corSpatial' ' ... so there is our answer. 
# then just go for the autocorr version 
# we get this error if we do ~ lat + lon: 
#Error: 'sumLenSq := sum(table(groups)^2)' = 2.60151e+10 is too large.
#Too large or no groups in your correlation structure? 

# wait, no, it still wont work because you can't have distance of zero! 

# now we cna just do this, should actually take care of the auto corr. 
mod.teLatLon <- gamm(pred_sd ~ s(pred_mean) + s(monDist) + s(winterTemp) +
                     s(summerTemp) + s(popD1kper1km) + YYYY + 
                     te(lat, lon, k = 10),
                   random = list(state=~1), 
                   data = dta)

# kkkkk coooooolll
plot(mod.teLatLon$gam)
plot(mod$gam)

modOnlydist <- gamm(pred_sd ~ s(monDist) +  YYYY+
                    te(lat, lon, k = 10),
            random = list(state=~1), 
            data = dta)
plot(modOnlydist$gam)


# okay, it takes a super long time to run this when you use the small dataset. 
# okay, the weid shap isn't due to the adjustment, its due to something else.... 
# so like, there must be a location far from monitors but sitll has low uncerstainty
# I mena, could be a product of the agregation scale too. 

# okay so now I'm checking whether I get the same results with smaller aggregation scale

# next I will isolate locations with that distance. 
# well, I can plot them. 


# oaky well I personally think correlated error is the way to go, and, if not, then correlated Y 
# so I'll try the error first and like, check if that grets ride of the autocorrr. 
# if it dones, then we can stop model-building. 

# well, we would sitll need to check for temporal autocorr
#mod.corrExp <- gamm(pred_sd ~ s(pred_mean) + s(monDist) + s(winterTemp) +
#              s(summerTemp) + s(popD1kper1km) + YYYY + state,
#            correlation = corSpatial(form = ~ lat + lon, type = 'gaussian'),
#            data = dta2010)

# cool, this exhausts the vector memory.... 
# okay.... Well then we will want a smaller set of grids, so we need to use bigger grid cells
# okay, this isn't even that much data. 
# I think that the penalized splines is a real problem. 
# I've spent like an hour hunting and I haven't come across a more clear solution than 
# doing this correlation option. 


# so the way I see it, I have three routes I can take: 
# 1) use natural splines and then use lagsarlm, and just follow that well-validated flowchart 
# 2) use penalized splines, and then use this this correlation structure 
# (difficult because we are already exhausting the vector memory.)
# 3) Go a Bayesian route - again, timing will be bad. 
# The Bayesian route is probably the best for the final model, feels the most 'complete' 
# and least restrictive 
# 4) google spatiotemporal models 



# spTimer might be good - explicitly spatiotemporal, its bayesian
# the problem is that these spatial-only models wont work when you have multiple 
# observations per location 
# at least spTimer is very explicit about everything.
# like, honestly, for the presentation, I would just sya that the variables explain 
# enough fo the spatial autocorr... though we would need to 
# check for temporal autocorr

# we def don't want GPP, what about AR or GP? 
# I think GP would be the right one? its most similar to spatial random effects? 

# Okay, I think I need to take a break for lunch. 

# Next steps: 
# change the code to 
# 








# okay so one option is like to use errorsarlm, which allows us to be specific about what type of model 
# and it'll be super clear. 
# but, that won't allow us to use penalized splines 
# another option is spaMM


# consider package spdep 
# consdier this function lagsarlm

# okay, next I will try model with just spatial error 
# and check whether that deals with the spatial residuals. 


# Conclusion: we have strong evidence of spatial autocorrelation 
# but magnitude of autocorrelation is low. 
# would be interesting to plot these residuals. 
# (note, we will also have nearly-perfect temporal autocorrelation... I mena... )

# 3a Name the all-ps model as the final model because it the final model for now. 



mod.allps <- gam(pred_sd ~ s(pred_mean) + s(monDist) + s(winterTemp) +
                    s(summerTemp) + s(popD1kper1km) + YYYY + state,
                  data = dta)


p_load(spdep)

coords <- dta %>% 
  dplyr::select(lat, lon) %>% 
  mutate(lat = as.numeric(lat), lon= as.numeric(lon)) %>% 
  as.matrix()
IDs <- row.names(dta)
neigh_close <- knn2nb(knearneigh(coords, k=1),row.names=IDs)
dists <- unlist(nbdists(neigh_close, coords))
max_min <- max(dists)



dist_nb_mm <- dnearneigh(coords, d1=0, d2=max_min*1.5, row.names=IDs)
oc_nbq_w_mm <-nb2listw(dist_nb_mm)


mod.alllin <- lm(pred_sd ~ ns(pred_mean, 5) + monDist + winterTemp +
                   summerTemp + popD1kper1km + YYYY + state,
                 data = dta)
dta$resid <- mod.alllin$residuals

# okay, so we can still do this test if the terms are natural splines 
# (note that natural splines will also make the partial R-sq easier)
# but... penalized better? 
oc.lagrange <- lm.LMtests(mod.alllin,
                          oc_nbq_w_mm, 
                          test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
oc.lagrange

# SAR: spatial autoregressive: spatially-lagged Y values

dta.sf <-  st_as_sf(dta, coords = c("lon", "lat"), crs=st_crs("epsg:4326"))

# okay, so lm.LMtests is one code to do tests to pick the best one 
ggplot() + 
  geom_sf(fill = NA)  + 
  geom_sf(data = dta.sf, aes(fill= resid, color = resid), size = 0.9)  
  
ggplot() + 
  geom_sf(fill = NA)  + 
  geom_sf(data = dta.sf, aes(fill= state, color = state), size = 0.9)  


# you know, this is a linear model, and not its not even mixed, so DHARMa is not really necesary. 
simulationOutput <- simulateResiduals(fittedModel = mod, plot = F)

testSpatialAutocorrelation(simulationOutput = simulationOutput, 
                           x = dta$lat, 
                           y= dta$lon)

  
####************************
#### 4: Fit Final Model ####
####************************

# 4a Create onebasis objects
ob.pred_mean <- onebasis(dta$pred_mean, fun = 'ps') 
ob.monDist <- onebasis(dta$monDist, fun = 'ps') 
ob.winterTemp <- onebasis(dta$winterTemp, fun = 'ps') 
ob.summerTemp <- onebasis(dta$summerTemp, fun = 'ps') 
ob.popD1kper1km <- onebasis(dta$popD1kper1km, fun = 'ps') 

# 4b Fit final model 
mod.pred_mean <- gamm(pred_sd ~ ob.pred_mean + s(monDist) + s(winterTemp) +
                     s(summerTemp) + s(popD1kper1km),
                   random = list(state=~1),
                   data = dta)

mod.monDist <- gamm(pred_sd ~ s(pred_mean) + ob.monDist + s(winterTemp) +
                        s(summerTemp) + s(popD1kper1km),
                      random = list(state=~1),
                      data = dta)

mod.winterTemp <- gamm(pred_sd ~ s(pred_mean) + s(monDist) + ob.winterTemp +
                        s(summerTemp) + s(popD1kper1km),
                      random = list(state=~1),
                      data = dta)

mod.summerTemp <- gamm(pred_sd ~ s(pred_mean) + s(monDist) + s(winterTemp) +
                        ob.summerTemp + s(popD1kper1km),
                      random = list(state=~1),
                      data = dta)

# okay, this is the one that misbehaves
# not any more ?
mod.popD1kper1km <- gamm(pred_sd ~ s(pred_mean) + s(monDist) + s(winterTemp) +
                        s(summerTemp) + ob.popD1kper1km,
                      random = list(state=~1),
                      data = dta)

# 4c Report associations
# the function seems to not be working so we will not use it. 

cp.pred_mean <- crosspred(ob.pred_mean, mod.pred_mean$gam, 
                          cen = quantile(dta[,'pred_mean'], 0.5)[[1]], 
                          at = quantile(dta[,'pred_mean'], 0.9)[[1]])

cp.monDist <- crosspred(ob.monDist, mod.monDist$gam, 
                           cen = quantile(dta[,'monDist'], 0.5)[[1]], 
                           at = quantile(dta[,'monDist'], 0.9)[[1]])

cp.winterTemp <- crosspred(ob.winterTemp, mod.winterTemp$gam, 
                          cen = quantile(dta[,'winterTemp'], 0.5)[[1]], 
                          at = quantile(dta[,'winterTemp'], 0.9)[[1]])

cp.summerTemp <- crosspred(ob.summerTemp, mod.summerTemp$gam, 
                          cen = quantile(dta[,'summerTemp'], 0.5)[[1]], 
                          at = quantile(dta[,'summerTemp'], 0.9)[[1]])

cp.popD1kper1km <- crosspred(ob.popD1kper1km, mod.popD1kper1km$gam, 
                          cen = quantile(dta[,'popD1kper1km'], 0.5)[[1]], 
                          at = quantile(dta[,'popD1kper1km'], 0.9)[[1]])


extract_assoc <- function(VarName, cp){
  # VarName <- 'pred_mean' ; obName <- ob.pred_mean
  paste0(VarName, ": ", round(cp$allfit[1][[1]], 5), " 95% CI: ", 
         round(cp$alllow[1][[1]], 5), ', ',
         round(cp$allhigh[1][[1]], 5))
}
extract_assoc('pred_mean', cp.pred_mean)
extract_assoc('monDist', cp.monDist)
extract_assoc('winterTemp', cp.winterTemp)
extract_assoc('summerTemp', cp.summerTemp)
extract_assoc('popD1kper1km', cp.popD1kper1km)

# okay play around with these a bit 

# 
####*******************************
#### 5: Visualize Associations ####
####*******************************

# 5a look at psp plots 

cp.pred_mean <- crosspred(ob.pred_mean, mod.pred_mean$gam, 
                          cen = quantile(dta[,'pred_mean'], 0.5)[[1]], 
                          at = seq(min(dta[,'pred_mean']), max(dta[,'pred_mean']), 
                                   length.out = 100))

cp.monDist <- crosspred(ob.monDist, mod.monDist$gam, 
                               cen = quantile(dta[,'monDist'], 0.5)[[1]], 
                               at = seq(min(dta[,'monDist']), max(dta[,'monDist']), 
                                        length.out = 100))
cp.winterTemp <- crosspred(ob.winterTemp, mod.winterTemp$gam, 
                           cen = quantile(dta[,'winterTemp'], 0.5)[[1]], 
                           at = seq(min(dta[,'winterTemp']), max(dta[,'winterTemp']), 
                                    length.out = 100))
cp.summerTemp <- crosspred(ob.summerTemp, mod.summerTemp$gam, 
                           cen = quantile(dta[,'summerTemp'], 0.5)[[1]], 
                           at = seq(min(dta[,'summerTemp']), max(dta[,'summerTemp']), 
                                    length.out = 100))
cp.popD1kper1km <- crosspred(ob.popD1kper1km, mod.popD1kper1km$gam, 
                           cen = quantile(dta[,'popD1kper1km'], 0.5)[[1]], 
                           at = seq(min(dta[,'popD1kper1km']), max(dta[,'popD1kper1km']), 
                                    length.out = 100))


plot_assoc <- function(cp, VarName){
  # cp <- cp.summerTemp; VarName = 'summerTemp'
  g <- cp
  cp.plot <- data.frame(exp = seq(min(dta[,VarName]), max(dta[,VarName]), 
                                  length.out = 100),
    fit = g$allfit,lci = g$alllow, uci = g$allhigh)
assoc.plot <- ggplot(cp.plot, aes(x = exp)) + 
  geom_hline(yintercept = 0) + 
  geom_point(aes(x=quantile(dta[,VarName], 0.5)[[1]], y=0), color = 'lightblue', size = 3) + 
  geom_line(aes(y=fit)) + 
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.25) + 
  ggtitle(VarName) + 
  labs(x= VarName, y = 'Prediction Uncertainty (ug/m^3)')

dta2 <- dta %>% 
  rename(ActiveVar := !!VarName)

hist.plot <- ggplot(dta2) + 
  geom_histogram(aes(ActiveVar)) + 
  labs(x= VarName, y = 'Count')

print(plot_grid(assoc.plot, hist.plot, nrow = 2))

}

pdf(here::here('ISEE_analysis', 'outputs', 'associationplots_2011.pdf'))
plot_assoc(cp.pred_mean, 'pred_mean')
plot_assoc(cp.monDist, 'monDist')
plot_assoc(cp.winterTemp, 'winterTemp')
plot_assoc(cp.summerTemp, 'summerTemp')
plot_assoc(cp.popD1kper1km, 'popD1kper1km')

#plot(mod.allpsp$gam)
#ggplot(dta) + 
 # geom_histogram(aes(popD1kper1km), bins = 70)
dev.off()
