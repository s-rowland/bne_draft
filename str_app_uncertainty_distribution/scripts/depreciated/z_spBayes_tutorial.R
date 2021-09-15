# spBayes Tutorial
# Nationwide Application
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 
# Sept 12, 2021

####-----------------------
#### Table of Contents ####
####-----------------------

# 0: Preparation 
# 1: Generate Synthetic Data
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

####--------------------------------
#### 1: Generate Synthetic Data ####
####--------------------------------

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
n.samples <- 2000
# starting values for parameters in spLM()
starting <- list("phi"=3/0.5, "sigma.sq"=50, "tau.sq"=1) 
# variances for the Metropolis sampler in spLM()
tuning <- list("phi"=0.1, "sigma.sq"=0.1, "tau.sq"=0.1)
# interval for seeing progress of the sampler in spLM()
n.report <- 500

####------------------
#### 3: Fit Model ####
####------------------

# 3a Fit model
# need to use function spMvLM() for multivariate model. 
m.1 <- spLM(y~X-1, coords=coords, starting=starting, tuning=tuning,
            priors=priors.1, cov.model=cov.model, n.samples=n.samples,
            n.report=n.report)

# arguement key
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



