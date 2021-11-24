# Written by Artur Araujo
# November 2019

# artur.stat@gmail.com

# while working
# on the IDEAL project

# http://www.ideal.rwth-aachen.de/

# define working directory
while ( !"treatment_within_subject.R" %in% list.files() ) {
  file <- file.choose();# choose this file
  WorkingDir  <- dirname(file);# get path to file
  setwd(dir=WorkingDir); # define working directory
  rm(file, WorkingDir); # remove objects
}

if ( !"gaba_placebo_miss" %in% ls() ) {
  source(file="./data_normal.R");
}

# R program 6.13

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

join_factor <- function(x, y, sep="") {
  if ( length(x) != length(y) )
    stop("'x', 'y' lengths differ");
  ret <- vector( mode="character", length=length(x) );
  for ( i in 1:length(x) ) {
    ret[i] <- paste(x[i], y[i], sep=sep);
  }
  lev <- unique(ret);
  lev <- lev[order(nchar(lev), lev)];
  ret <- factor(x=ret, levels=lev);
  return(ret);
}

####################################################################
### Intercept varying among subject and treatment within subject ###
####################################################################

# R program 6.14

# fit linear mixed-effects model
lmm0.large <- lmer(
  formula=Pain~
    Treatment+(1 | id)+
    (1 | id:Cycle)+
    (1 | id:Treatment),
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

####################################################################
### Intercept varying among subject and treatment within subject ###
## AR1 stochastic process ##########################################
####################################################################

# This is not implemented in lmer.
# It is implemented in lme, but support is limited.

####################################################################
### Intercept varying among subject and treatment within subject ###
## Bayesian inference ##############################################
####################################################################

# R program 6.15

data_treat_subj <- with(
  data=gaba_placebo_miss,
  expr={
    subj_treat <- join_factor(id, Treatment, sep=":");
    subj_cycle <- join_factor(id, Cycle, sep=":");
    list(
      nd=length(id), # number of data elements
      ns=nlevels(id), # number of subjects
      nt=nlevels(Treatment), # number of treatments
      nm=nlevels(subj_treat),# number of subject*treatment levels
      nc=nlevels(subj_cycle), # number of subject*cycle levels
      subj=as.integer(id), # subject indicator
      treat=as.integer(Treatment), # treatment indicator
      subj_treat=as.integer(subj_treat), # subject*treatment indicator
      subj_cycle=as.integer(subj_cycle), # subject*cycle indicator
      outcome=Pain # outcome vector
    );
  }
);

fit_treat_subj <- stan(
  file="../Stan/treatment_within_subject.stan",
  model_name="fit_treat_subj",
  data=data_treat_subj,
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
get_elapsed_time(fit_treat_subj);

print(
  x=fit_treat_subj,
  pars=c(
    "mu", "pte", "phi", "psi",
    "eta", "sigma", "ALPHA"
  ),
  probs=c(0.025, 0.975)
);

print( x=fit_treat_subj, pars=c("THETA"), probs=c(0.025, 0.975) );

print( x=fit_treat_subj, pars=c("OMEGA", "lp__"), probs=c(0.025, 0.975) );

stan_plot(fit_treat_subj, pars=c("pte", "ITE"), ci_level=0.95);

print( x=fit_treat_subj, pars=c("pte", "ITE"), probs=c(0.025, 0.975) );

# clean the environment
rm(data_treat_subj, fit_treat_subj);

####################################################################
### Intercept varying among subject and treatment within subject ###
## AR1 stochastic process ##########################################
## Bayesian inference ##############################################
####################################################################

# R program 6.16

data_treat_subj_AR1 <- with(
  data=gaba_placebo_miss,
  expr={
    subj_treat <- join_factor(id, Treatment, sep=":");
    list(
      nd=length(id), # number of data elements
      ns=nlevels(id), # number of subjects
      nt=nlevels(Treatment), # number of treatments
      nm=nlevels(subj_treat),# number of subject*treatment levels
      np=max(Period), # number of periods
      subj=as.integer(id), # subject indicator
      treat=as.integer(Treatment), # treatment indicator
      subj_treat=as.integer(subj_treat), # subject*treatment indicator
      period=as.integer(Period), # period indicator
      outcome=Pain # outcome vector
    );
  }
);

fit_treat_subj_AR1 <- stan(
  file="../Stan/treatment_within_subject_AR1.stan",
  model_name="fit_treat_subj_AR1",
  data=data_treat_subj_AR1,
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
get_elapsed_time(fit_treat_subj_AR1);

print(
  x=fit_treat_subj_AR1,
  pars=c(
    "mu", "pte", "phi", "psi",
    "sigma", "rho", "ALPHA"
  ),
  probs=c(0.025, 0.975)
);

print( x=fit_treat_subj_AR1, pars=c("THETA", "lp__"), probs=c(0.025, 0.975) );

stan_plot(fit_treat_subj_AR1, pars=c("pte", "ITE"), ci_level=0.95);

print( x=fit_treat_subj, pars=c("pte", "ITE"), probs=c(0.025, 0.975) );

# clean the environment
rm(data_treat_subj_AR1, fit_treat_subj_AR1);
rm(iseed, join_factor);
