# Compare Aditya and Lawrence Location Data
# BNE Crude Error Assessment 
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation 
# 1: Generate Synthetic Data
# 2: Fit Model 
# 3: Examine Model

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

# 0b Load spatial package 
p_load(spBayes)

# 0c function to make sure inputs are compatible
# from tutorial
rmvn <- function(n, mu=0, V = matrix(1)){ p <- length(mu) if(any(is.na(match(dim(V),p))))
  stop("Dimension problem!")
D <- chol(V)
t(matrix(rnorm(n*p), ncol=p)%*%D + rep(mu,rep(n,p)))
}

# 0d Set seed 
# from tutorial
set.seed(1)

####********************************
#### 1: Generate Synthetic Data ####
####********************************

# 1a Define Beta and magnitude of correlation
Beta1 <- 2; Mcorr <- 0.5

# 1a Make spatial locations
dta <- expand.grid(lat = c(1:100), lon = c(1:100))

# 1b Add non-spatial variable 
dta$X1 <- rnorm(nrow(dta),0,5)

# 1c Add dependent variable 
dta$Y <- rnorm(nrow(dta), dta$X1 * Beta1 + Mcorr*(dta$lat + dta$lon), 1)

# 1d Plot
ggplot(dta) + 
  geom_point(aes(x=lon, y=lat, color = Y, fill =Y))
 
ggplot(dta) + 
  geom_point(aes(x=X1, y=Y)) 
# we have a clear association and an underlying spatila component. 


####*********************************
#### 1B: Generate Synthetic Data ####
####*********************************

# n is number of observations
n <- 100
# coords are observation coordinates for spLM() 
coords <- cbind(runif(n,0,1), runif(n,0,1))
# X is matrix of regressors
# X1 is a dummy for intercept; X2 is a covariate
X <- as.matrix(cbind(1, rnorm(n)))
# B is slope vector
# B1 =1 is the intercept, B2=5 is slope of X2 covariate
B <- as.matrix(c(1,5))
# p is number of parameters
# we have 2 parameters of interst in our regression model
p <- length(B)

# Set spatial ccorrelation/ error parameters
sigma.sq <- 2 # variance of spatial process w(s)
tau.sq <- 0.1 # variance of white-noise error
phi <- 3/0.5 # magnitude (?) of autocorrelation

# 1c Generate Y - dependent variable
# D is distance matrix for coordinates
D <- as.matrix(dist(coords)) 
# R is correlation function 
R <- exp(-phi*D)
# w is a spatial process
w <- rmvn(1, rep(0,n), sigma.sq*R)
# y is a vector of spatially referenced dependent variables
y <- rnorm(n, X%*%B + w, sqrt(tau.sq))

# 1d Ingredients of Bayesian model 
# priors for parameters in spLM(): betas are multivariate Normal
priors.1 <- list("beta.Norm"=list(rep(0,p), diag(1000,p)), 
                 "phi.Unif"=c(3/1, 3/0.1), "sigma.sq.IG"=c(2, 2),
                 "tau.sq.IG"=c(2, 0.1))
# function for spatial dependence structure in spLM()
cov.model <- "exponential"

# 1e Ingredients of MCMC
# number of MCMC iterations, used in spLM()
n.samples <- 2000
# starting values for parameters in spLM()
starting <- list("phi"=3/0.5, "sigma.sq"=50, "tau.sq"=1) 
# variances for the Metropolis sampler in spLM()
tuning <- list("phi"=0.1, "sigma.sq"=0.1, "tau.sq"=0.1)
# interval for seeing progress of the sampler in spLM()
n.report <- 500

# 1f Fit model
# need to use function spMvLM() for multivariate model. 
m.1 <- spLM(y~X-1, coords=coords, starting=starting, tuning=tuning,
            priors=priors.1, cov.model=cov.model, n.samples=n.samples,
            n.report=n.report)

# maybe this is ... not worth doing. 
# maybe its better to throw in the towel and just use the GMA approach, because 
# at least then we can use penalized splines. 
# well, yes, and penalized splines are great... but at least I'm actually learning
# something if I do it this way. 
# yeah, I can totally imagine Marianthi saying soemthing like, this is a 
# complicated approach, and we want simple so that it is more interpretable 
# by a non-bayesian reader 
# and you're not providing a clear arguement about why this approach is any better. 

# well, first we don't have to chose the number of knots.
# but - this model can't handle mulitvariate't really handle non-linearity. 

# I don't even know what the point of this is. 
# well, its your job to determine the most appropriate model. 
# if we are most concerned about getting the shape of the relationships right, 
# then the GAM is an easy approach 
# if we are most concerned about dealing with the autocorrelation, then the 
# spBayes is the best approach. 
# I suppose GAM has bigger risk of over adjustment. 

####******************
#### 2: Fit Model ####
####******************

# fit model
# key
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




####**********************
#### 3: Examine Model ####
####**********************

# get stuff from the PPD's
spRecover()

