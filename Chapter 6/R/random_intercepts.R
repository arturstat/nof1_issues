# Written by Artur Araujo
# November 2019

# artur.stat@gmail.com

# while working
# on the IDEAL project

# http://www.ideal.rwth-aachen.de/

# define working directory
while ( !"random_intercepts.R" %in% list.files() ) {
  file <- file.choose();# choose this file
  WorkingDir  <- dirname(file);# get path to file
  setwd(dir=WorkingDir); # define working directory
  rm(file, WorkingDir); # remove objects
}

if (
  !all(
    c(
      "gaba_placebo_diff",
      "gaba_placebo_diff_miss"
    ) %in% ls()
  )
) {
  source(file="./data_normal.R");
}

# R program 6.26

if ( !"lme4" %in% rownames( installed.packages() ) )
  {install.packages(pkgs="lme4");}
library(package=lme4);

if ( !"pbkrtest" %in% rownames( installed.packages() ) )
  {install.packages(pkgs="pbkrtest");}
library(package=pbkrtest);

# Load parallel package
library(parallel);

if (
  .Platform$GUI=="RStudio" &
  !"rstudioapi" %in% rownames( installed.packages() )
) {install.packages(pkgs="rstudioapi");}

if ( !"rstan" %in% rownames( installed.packages() ) )
  {install.packages(pkgs="rstan");}
library(package=rstan);

options( mc.cores=parallel::detectCores() );
rstan_options(auto_write=TRUE);
Sys.setenv(LOCAL_CXX14='g++ -std=c++1y');
Sys.setenv(LOCAL_CXX14FLAGS='-O3');
Sys.setenv(LOCAL_CXX11FLAGS='-O3');

iseed <- 3141593; # seed for RNG

#########################
### Random intercepts ###
#########################

# R program 6.27

# fit linear mixed-effects model
lmm0 <- lmer(
  formula=deltaPain~1+(1 | id),
  data=gaba_placebo_diff,
  REML=TRUE
);

# extract the fixed effects
fixef(lmm0);

# print the variance components
print( x=VarCorr(lmm0), comp=c("Variance", "Std.Dev") );

# Kenward-Roger covariance matrix of the fixed effects
vcovAdj(lmm0);

# create cluster
cl <- makeCluster(
  spec=rep( "localhost", detectCores() ),
  type="PSOCK"
);

# set RNG seed on cluster
clusterSetRNGStream(cl=cl, iseed=iseed);

# parameter confidence intervals
confint(object=lmm0, level=0.95, method="profile");
confint(object=lmm0, level=0.95, method="Wald");

# parametric bootstrap confidence intervals
confint(
  object=lmm0,
  level=0.95,
  method="boot",
  nsim=1000,
  boot.type="perc",
  type="parametric",
  cl=cl
);

# stop cluster
stopCluster(cl=cl);

# clean the environment
rm(lmm0, cl);

###########################
#### Random intercepts ####
## Bayesian inference #####
###########################

# R program 6.28

data_random_intercepts <- with(
  data=gaba_placebo_diff_miss,
  expr={
    list(
      nd=length(id), # number of data elements
      ns=nlevels(id), # number of subjects
      nc=nlevels(Cycle), # number of cycles
      subj=as.integer(id), # subject indicator
      cycle=as.integer(Cycle), # cycle indicator
      delta=as.integer(deltaPeriod), # delta vector
      outcome=deltaPain # outcome vector
    );
  }
);

fit_random_intercepts <- stan(
  file="../Stan/random_intercepts.stan",
  model_name="fit_random_intercepts",
  data=data_random_intercepts,
  chains=parallel::detectCores(),
  iter=25000,
  warmup=5000,
  thin=1,
  init="random",
  seed=iseed,
  algorithm="NUTS",
  control=list(adapt_delta=0.999),
  save_dso=FALSE,
  verbose=TRUE,
  cores=parallel::detectCores()
);
get_elapsed_time(fit_random_intercepts);

print( x=fit_random_intercepts, probs=c(0.025, 0.975) );

stan_plot(fit_random_intercepts, pars=c("pte", "ITE"), ci_level=0.95);

print( x=fit_random_intercepts, pars=c("pte", "ITE"), probs=c(0.025, 0.975) );

# clean the environment
rm(fit_random_intercepts);

############################
#### Random intercepts #####
## AR1 stochastic process ##
## Bayesian inference ######
############################

# R program 6.29

fit_random_intercepts_AR1 <- stan(
  file="../Stan/random_intercepts_AR1.stan",
  model_name="fit_random_intercepts_AR1",
  data=data_random_intercepts,
  chains=parallel::detectCores(),
  iter=12000,
  warmup=2000,
  thin=1,
  init="random",
  seed=iseed,
  algorithm="NUTS",
  control=list(adapt_delta=0.99),
  save_dso=FALSE,
  verbose=TRUE,
  cores=parallel::detectCores()
);
get_elapsed_time(fit_random_intercepts_AR1);

print( x=fit_random_intercepts_AR1, probs=c(0.025, 0.975) );

stan_plot(fit_random_intercepts_AR1, pars=c("pte", "ITE"), ci_level=0.95);

print( x=fit_random_intercepts_AR1, pars=c("pte", "ITE"), probs=c(0.025, 0.975) );

# clean the environment
rm(data_random_intercepts, fit_random_intercepts_AR1);
rm(iseed);
