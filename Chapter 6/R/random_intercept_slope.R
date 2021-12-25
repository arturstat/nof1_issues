# Written by Artur Araujo
# November 2019

# artur.stat@gmail.com

# while working
# on the IDEAL project

# http://www.ideal.rwth-aachen.de/

# define working directory
while ( !"random_intercept_slope.R" %in% list.files() ) {
  file <- file.choose();# choose this file
  WorkingDir  <- dirname(file);# get path to file
  setwd(dir=WorkingDir); # define working directory
  rm(file, WorkingDir); # remove objects
}

if ( !"gaba_placebo_miss" %in% ls() ) {
  source(file="./data_normal.R");
}

# R program 6.17

if ( !"lme4" %in% rownames( installed.packages() ) )
  {install.packages(pkgs="lme4");}
library(package=lme4);

if ( !"pbkrtest" %in% rownames( installed.packages() ) )
  {install.packages(pkgs="pbkrtest");}
library(package=pbkrtest);

# Load parallel package
library(parallel);

# Load nlme package
library(nlme);

if (
  .Platform$GUI=="RStudio" &
  !"rstudioapi" %in% rownames( installed.packages() )
) {install.packages(pkgs="rstudioapi");}

if ( !"rstan" %in% rownames( installed.packages() ) )
  {install.packages(pkgs="rstan");}
library(package=rstan);

options( mc.cores=parallel::detectCores() );
rstan_options(auto_write=TRUE);
#Sys.setenv(LOCAL_CXX14='g++ -std=c++1y');
#Sys.setenv(LOCAL_CXX14FLAGS='-O3');
#Sys.setenv(LOCAL_CXX11FLAGS='-O3');

iseed <- 3141593; # seed for RNG

#############################################
### Correlated random intercept and slope ###
#############################################

# R program 6.18

# fit linear mixed-effects model
lmm0.large <- lmer(
  formula=Pain~
    Treatment+
    (Treatment | id),
  data=gaba_placebo_miss,
  REML=TRUE
);

# extract the fixed effects
fixef(lmm0.large);

# print the variance components
print( x=VarCorr(lmm0.large), comp=c("Variance", "Std.Dev") );

# Kenward-Roger covariance matrix of the fixed effects
vcovAdj(lmm0.large);

# Approximate F-test based on the Kenward-Roger approach.
# The smallModel must have the same
# covariance structure as the largeModel.
# Both models can be fit using REML estimation.
lmm0.small <- update(object=lmm0.large, formula=~.-Treatment);
KRmodcomp(largeModel=lmm0.large, smallModel=lmm0.small);

# create cluster
cl <- makeCluster(
  spec=rep( "localhost", detectCores() ),
  type="PSOCK"
);

# set RNG seed on cluster
clusterSetRNGStream(cl=cl, iseed=iseed);

# Compare models using parametric bootstrap method
PBmodcomp(
  largeModel=lmm0.large,
  smallModel=lmm0.small,
  nsim=1000,
  cl=cl
);

# parameter confidence intervals
confint(object=lmm0.large, level=0.95, method="profile");
confint(object=lmm0.large, level=0.95, method="Wald");

# parametric bootstrap confidence intervals
confint(
  object=lmm0.large,
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
rm(lmm0.large, lmm0.small, cl);

#############################################
### Correlated random intercept and slope ###
## AR1 stochastic process ###################
#############################################

# R program 6.19

# fit linear mixed-effects model
lmm1 <- lme(
  fixed=Pain~Treatment,
  data=gaba_placebo_miss,
  random=~Treatment | id,
  correlation=corAR1(form=~Period | id),
  method="REML"
);

# extract the fixed effects
fixef(lmm1);

# print the variance components
print( x=VarCorr(lmm1), comp=c("Variance", "Std.Dev") );

# covariance matrix of the fixed effects
vcov(lmm1);

# likelihood-ratio test
# nested models fit by ML
lmm1.large <- update(object=lmm1, method="ML");
lmm1.small <- update(object=lmm1.large, fixed=~.-Treatment);
anova(lmm1.small, lmm1.large); # significant Treatment coefficient

# parameter confidence intervals
intervals(object=lmm1, level=0.95, which="all");

# clean the environment
rm(lmm1, lmm1.large, lmm1.small);

###############################################
### Uncorrelated random intercept and slope ###
###############################################

# R program 6.20

# convert treatment variable to binary 0, 1
gaba_placebo_temp <- gaba_placebo_miss;
gaba_placebo_temp$Treatment <-
  as.integer(gaba_placebo_temp$Treatment)-1;

# fit linear mixed-effects model
lmm2.large <- lmer(
  formula=Pain~
    Treatment+
    (Treatment || id),
  data=gaba_placebo_temp,
  REML=TRUE
);

# extract the fixed effects
fixef(lmm2.large);

# print the variance components
print( x=VarCorr(lmm2.large), comp=c("Variance", "Std.Dev") );

# Kenward-Roger covariance matrix of the fixed effects
vcovAdj(lmm2.large);

# Approximate F-test based on the Kenward-Roger approach.
# The smallModel must have the same
# covariance structure as the largeModel.
# Both models can be fit using REML estimation.
lmm2.small <- update(object=lmm2.large, formula=~.-Treatment);
KRmodcomp(largeModel=lmm2.large, smallModel=lmm2.small);

# create cluster
cl <- makeCluster(
  spec=rep( "localhost", detectCores() ),
  type="PSOCK"
);

# set RNG seed on cluster
clusterSetRNGStream(cl=cl, iseed=iseed);

# Compare models using parametric bootstrap method
PBmodcomp(
  largeModel=lmm2.large,
  smallModel=lmm2.small,
  nsim=1000,
  cl=cl
);

# parameter confidence intervals
confint(object=lmm2.large, level=0.95, method="profile");
confint(object=lmm2.large, level=0.95, method="Wald");

# parametric bootstrap confidence intervals
confint(
  object=lmm2.large,
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
rm(gaba_placebo_temp, lmm2.large, lmm2.small, cl);

###############################################
### Uncorrelated random intercept and slope ###
## AR1 stochastic process #####################
## distinct residual variance per cycle #######
###############################################

# R program 6.21

# fit linear mixed-effects model
lmm3 <- lme(
  fixed=Pain~Treatment,
  data=gaba_placebo_miss,
  random=list( id=pdDiag(value=~Treatment) ),
  correlation=corAR1(form=~Period | id),
  weights=varIdent(form=~1 | Cycle),
  method="REML"
);

# extract the fixed effects
fixef(lmm3);

# print the variance components
print( x=VarCorr(lmm3), comp=c("Variance", "Std.Dev") );

# covariance matrix of the fixed effects
vcov(lmm3);

# likelihood-ratio test
# nested models fit by ML
lmm3.large <- update(object=lmm3, method="ML");
lmm3.small <- update(object=lmm3.large, fixed=~.-Treatment);
anova(lmm3.small, lmm3.large); # significant Treatment coefficient

# parameter confidence intervals
intervals(object=lmm3, level=0.95, which="all");

# clean the environment
rm(lmm3, lmm3.large, lmm3.small);

#############################################
### Correlated random intercept and slope ###
## Bayesian inference #######################
#############################################

# R program 6.22

data_random_slope <- with(
  data=gaba_placebo_miss,
  expr={
    list(
      nd=length(id), # number of data elements
      ns=nlevels(id), # number of subjects
      nt=nlevels(Treatment), # number of treatments
      subj=as.integer(id), # subject indicator
      treat=as.integer(Treatment), # treatment indicator
      outcome=Pain # outcome vector
    );
  }
);

fit_random_slope <- stan(
  file='../Stan/correlated_random_intercept_slope.stan',
  model_name="fit_random_slope",
  data=data_random_slope,
  chains=parallel::detectCores(),
  iter=3000,
  warmup=1000,
  thin=1,
  init="random",
  seed=iseed,
  algorithm="NUTS",
  save_dso=FALSE,
  verbose=TRUE,
  cores=parallel::detectCores()
);
get_elapsed_time(fit_random_slope);

print(
  x=fit_random_slope,
  pars=c("MU", "ALPHA", "PSI", "sigma", "lp__"),
  probs=c(0.025, 0.975)
);

stan_plot(fit_random_slope, pars=c("PTE", "ITE"), ci_level=0.95);

print( x=fit_random_slope, pars=c("PTE", "ITE"), probs=c(0.025, 0.975) );

# clean the environment
rm(fit_random_slope);

#############################################
### Correlated random intercept and slope ###
## AR1 stochastic process ###################
## Bayesian inference #######################
#############################################

# R program 6.23

data_random_slope_ar1 <- with(
  data=gaba_placebo_miss,
  expr={
    list(
      nd=length(id), # number of data elements
      ns=nlevels(id), # number of subjects
      nt=nlevels(Treatment), # number of treatments
      np=max(Period), # number of periods
      subj=as.integer(id), # subject indicator
      treat=as.integer(Treatment), # treatment indicator
      period=as.integer(Period), # period indicator
      outcome=Pain # outcome vector
    );
  }
);

fit_random_slope_ar1 <- stan(
  file='../Stan/correlated_random_intercept_slope_AR1.stan',
  model_name="fit_random_slope_ar1",
  data=data_random_slope_ar1,
  chains=parallel::detectCores(),
  iter=3000,
  warmup=1000,
  thin=1,
  init="random",
  seed=iseed,
  algorithm="NUTS",
  control=list(
    adapt_delta=0.99,
    max_treedepth=12
  ),
  save_dso=FALSE,
  verbose=TRUE,
  cores=parallel::detectCores()
);
get_elapsed_time(fit_random_slope_ar1);

print(
  x=fit_random_slope_ar1,
  pars=c("MU", "ALPHA", "PSI", "sigma", "rho", "lp__"),
  probs=c(0.025, 0.975)
);

stan_plot(fit_random_slope_ar1, pars=c("PTE", "ITE"), ci_level=0.95);

print( x=fit_random_slope_ar1, pars=c("PTE", "ITE"), probs=c(0.025, 0.975) );

# clean the environment
rm(fit_random_slope_ar1);

###############################################
### Uncorrelated random intercept and slope ###
## Bayesian inference #########################
###############################################

# R program 6.24

fit_random_slope_un <- stan(
  file='../Stan/uncorrelated_random_intercept_slope.stan',
  model_name="fit_random_slope_un",
  data=data_random_slope,
  chains=parallel::detectCores(),
  iter=3000,
  warmup=1000,
  thin=1,
  init="random",
  seed=iseed,
  algorithm="NUTS",
  save_dso=FALSE,
  verbose=TRUE,
  cores=parallel::detectCores()
);
get_elapsed_time(fit_random_slope_un);

print(
  x=fit_random_slope_un,
  probs=c(0.025, 0.975)
);

stan_plot(fit_random_slope_un, pars=c("pte", "ITE"), ci_level=0.95);

print( x=fit_random_slope_un, pars=c("pte", "ITE"), probs=c(0.025, 0.975) );

# clean the environment
rm(data_random_slope, fit_random_slope_un);

###############################################
### Uncorrelated random intercept and slope ###
## AR1 stochastic process #####################
## Bayesian inference #########################
###############################################

# R program 6.25

fit_random_slope_un_ar1 <- stan(
  file='../Stan/uncorrelated_random_intercept_slope_AR1.stan',
  model_name="fit_random_slope_un_ar1",
  data=data_random_slope_ar1,
  chains=parallel::detectCores(),
  iter=3000,
  warmup=1000,
  thin=1,
  init="random",
  seed=iseed,
  algorithm="NUTS",
  save_dso=FALSE,
  verbose=TRUE,
  cores=parallel::detectCores()
);
get_elapsed_time(fit_random_slope_un_ar1);

print(
  x=fit_random_slope_un_ar1,
  probs=c(0.025, 0.975)
);

stan_plot(fit_random_slope_un_ar1, pars=c("pte", "ITE"), ci_level=0.95);

print( x=fit_random_slope_un_ar1, pars=c("pte", "ITE"), probs=c(0.025, 0.975) );

# clean the environment
rm(data_random_slope_ar1, fit_random_slope_un_ar1);
rm(iseed);
