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

if(!exists("Ran_a_00_ISEE")){
  here::i_am("README.md")
  source(here::here('str_application_uncertainty_distribution', 'scripts', 
                    "a_00_set_up_env_strApp.R"))
}

# 0b Load spatial package 
p_load(spBayes)

# 0c function to make sure inputs are compatible
# from tutorial from http://faculty.ucr.edu/~jflegal/203/spBayes_tutorial.pdf
rmvn <- function(n, mu=0, V = matrix(1)){
  p <- length(mu)
  if(any(is.na(match(dim(V),p))))
    stop("Dimension problem!")
  D <- chol(V)
  t(matrix(rnorm(n*p), ncol=p)%*%D + rep(mu,rep(n,p)))
}

# 0d Set seed 
# from tutorial
set.seed(1)

####-----------------------------
#### 1: Read BNE-factor Data ####
####-----------------------------

# 1a Read Data
readCombinedData <- function(YYYY){
  read_fst(here::here('str_application_uncertainty_distribution', 'data', 'processed', 
                      paste0('AVGSCMJSCC_3.5_explanatory_', YYYY, '_025deg.fst'))) %>% 
    mutate(YYYY = YYYY)
}
dta <- map(2010:2015, readCombinedData) %>% 
  bind_rows()
rm(readCombinedData)

# 1b Convert units of population density to thousand ppl per km 
dta <- dta %>% 
  mutate(pop_density_1kper1000km2 = pop_density*1000000 / 1000) %>% 
  mutate(lat = as.numeric(as.character(lat)), 
         lon = as.numeric(as.character(lon))) 

# 1c Remove obs with missing data 
#dta <- dta %>% 
#dplyr::select(-cell_id) %>% 
# na.omit()

# 1d Average across years 
dta <- dta %>% 
  group_by(lat, lon, state, region) %>% 
  summarize_all(mean) %>% 
  ungroup() %>% 
  dplyr::select(-state, -region, -YYYY)

# 1e Scale all of the variables... yes! 
# Let's not scale, it makes it a bit harder to interpret the plots
dta <- dta %>% 
  mutate_at(vars(-state, -region, -YYYY), scale) 

# 1f Remove missing data 
dta <- dta %>% 
  dplyr::select(-zcta) 
dta <- dta %>% 
  filter(complete.cases(dta))

# 1g Rename variables 
dta <- dta %>% 
  mutate(mon_dist1 = monDist1.mean, 
         mon_dist2 = 2*monDist2.mean-monDist1.mean, 
         mon_dist3 = 3*monDist3.mean-2*monDist2.mean, 
         mon_dist4 = 4*monDist4.mean-3*monDist3.mean, 
         mon_dist5 = 5*monDist5.mean-4*monDist4.mean)

# 1h create dummy variable for the intercept 
dta <- dta %>% mutate(one = 1)

# shrink dta for now 
#dta <- dta %>% slice_sample(prop = 0.25)

# 1i create matrix of the independent variables
X.bne <- cbind(dta$one, dta$pred_mean, dta$mon_dist1, dta$winter_temp, dta$summer_temp, 
               dta$pop_density_1kper1000km2, dta$cloud_cover, dta$elev)
# extract the number of independent variables for constructing the priors
p <- ncol(X.bne)

# 1j create vector of dependent variable 
y.bne <- as.vector(dta$pred_sd)

####---------------------------------
#### 2: Set Ingredients of Model ####
####---------------------------------

# 2a Ingredients of Bayesian model 
# priors for parameters in spLM(): betas are multivariate Normal
priors.1 <- list("beta.Norm"=list(rep(0,p), diag(1000,p)), 
                 "phi.Unif"=c(3/1, 3/0.1), "sigma.sq.IG"=c(2, 2),
                 "tau.sq.IG"=c(2, 0.1))

# function for spatial dependence structure in spLM()
cov.model <- "exponential"

# 2b Ingredients of MCMC
# number of MCMC iterations, used in spLM()
n.samples <- 200
# starting values for parameters in spLM()
starting <- list("phi"=3/0.5, "sigma.sq"=50, "tau.sq"=1) 
# variances for the Metropolis sampler in spLM()
tuning <- list("phi"=0.1, "sigma.sq"=0.1, "tau.sq"=0.1)
# interval for seeing progress of the sampler in spLM()
n.report <- 25

####------------------
#### 3: Fit Model ####
####------------------

# 3a Fit model
# need to use function spMvLM() for multivariate model. 
tic('finished model')
m.1 <- spLM(y.bne ~ X.bne - 1, 
            coords=cbind(dta$lat, dta$lon), 
            starting=starting, tuning=tuning,
            priors=priors.1, cov.model=cov.model, n.samples=n.samples,
            n.report=n.report)
toc()

# argument key
spLM(formula, # a description of the regression model to be fit
     coords, # an n × 2 matrix of the observation coordinates in R2
     starting, # a list with parameter names (could be beta, sigma.sq, tau.sq, phi, or nu). 
               # The value in the list is the parameter’s starting value
     tuning, # a list with parameter names, where the value now defines the 
             # variance of the Metropolis sampler Normal proposal distribution
     priors, # a list with parameter names (could be sigma.sq.ig, tau.sq.ig, phi.unif, nu.unif, beta.norm, or beta.flat).
             # The default is a flat prior
     cov.model, # a keyword that specifies the covariance function used to model 
                # the spatial dependence structure 
                # (could be exponential, matern, spherical, or gaussian)
     n.samples, # the number of MCMC iterations
     n.report) # the interval to report Metropolis sampler acceptance and MCMC progress

####----------------------
#### 3: Examine Chains ####
####----------------------

# these are the chains for the parameters for the spatial model
par(mfrow=c(2,2))
ts.plot(m.1$p.theta.samples[,1],main="sigma sq",ylab="",
        xlim=c(100,nrow(m.1$p.theta.samples)),ylim=c(0,4))
ts.plot(m.1$p.theta.samples[,2],main="tau sq",ylab="",
        xlim=c(100,nrow(m.1$p.theta.samples)),ylim=c(0,1))
ts.plot(m.1$p.theta.samples[,3],main="phi",ylab="",
        xlim=c(50,nrow(m.1$p.theta.samples)))

####------------------------------
#### 4: Examine Model Results ####
####------------------------------

# 4a calculate the number of samples we burned in
burn.in <- 0.5*n.samples

# 4b recover the beta and spatial random effects in one object
m.1 <- spRecover(m.1, start=burn.in, verbose=FALSE)

# 4c report distribution of the those three terms
# via the median and 95% CI. 
round(summary(m.1$p.theta.recover.samples)$quantiles[,c(3,1,5)],2)

# 4d report distribution of the regression coefficients 
# via the median and 95% CI. 
round(summary(m.1$p.beta.recover.samples)$quantiles[,c(3,1,5)],2)

# 4d report distribution of the spatial random effects
# rows are locations'random effects
# columns are posterior samples - median, 95% CI. 
m.1.w.summary <- summary(coda::mcmc(t(m.1$p.w.recover.samples)))$quantiles[,c(3,1,5)]



