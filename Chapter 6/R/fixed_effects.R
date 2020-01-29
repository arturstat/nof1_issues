# Written by Artur Araujo
# November 2019

# artur.stat@gmail.com

# while working
# on the IDEAL project

# http://www.ideal.rwth-aachen.de/

# define working directory
while ( !"fixed_effects.R" %in% list.files() ) {
  file <- file.choose();# choose this file
  WorkingDir  <- dirname(file);# get path to file
  setwd(dir=WorkingDir); # define working directory
  rm(file, WorkingDir); # remove objects
}

if (
  !all(
    c(
      "gaba_placebo",
      "gaba_placebo_miss",
      "gaba_placebo_diff"
    ) %in% ls()
  )
) {
  source(file="./data_normal.R");
}

# R program 6.7

if ( !"emmeans" %in% rownames( installed.packages() ) )
  {install.packages(pkgs="emmeans");}
library(package=emmeans);

if ( !"lme4" %in% rownames( installed.packages() ) )
  {install.packages(pkgs="lme4");}
library(package=lme4);

library(nlme);

##############################################
### fixed treatment by subject interaction ###
##############################################

# R program 6.8

# fit linear fixed-effects model
model0 <- lm(
  formula=Pain~
    Treatment+
    id+
    id:Treatment,
  data=gaba_placebo
);

summary(model0); # summary table
# Note that for this model the 'TreatmentGabapentin'
# coefficient is not equal to the sample treatment effect.

## unweighted estimate ##
means.u0 <- emmeans(object=model0, specs=~Treatment);
estimate.u0 <- pairs(x=means.u0, reverse=TRUE);
print(estimate.u0);
confint(object=estimate.u0, level=0.95);

## weighted estimate ##
means.w0 <- emmeans(
  object=model0, specs=~Treatment, weights="cells"
);
estimate.w0 <- pairs(x=means.w0, reverse=TRUE);
print(estimate.w0);
confint(object=estimate.w0, level=0.95);

# clean the environment
rm(model0, means.w0, estimate.w0);
rm(means.u0, estimate.u0);

##############################################
### fixed treatment by subject interaction ###
## random cycle by subject interaction #######
##############################################

# R program 6.9

# fit linear mixed-effects model
model1 <- lmer(
  formula=Pain~
    Treatment+
    id+
    id:Treatment+
    (1 | id:Cycle),
  data=gaba_placebo,
  REML=TRUE
);

summary(model1);

## weighted estimate ##
means.w1 <- emmeans(
  object=model1, specs=~Treatment, weights="cells"
);
estimate.w1 <- pairs(x=means.w1, reverse=TRUE);
print(estimate.w1);
confint(object=estimate.w1, level=0.95);

# clean the environment
rm(model1, means.w1, estimate.w1);

##############################################
### fixed treatment by subject interaction ###
## AR(1) stochastic process ##################
##############################################

# R program 6.10

# fit linear fixed-effects model
model2 <- gls(
  model=Pain~
    Treatment+
    id+
    id:Treatment,
  data=gaba_placebo_miss,
  correlation=corAR1(form=~Period | id)
);

summary(model2); # summary table

## weighted estimate ##
means.w2 <- emmeans(
  object=model2, specs=~Treatment, weights="cells"
);
estimate.w2 <- pairs(x=means.w2, reverse=TRUE);
print(estimate.w2);
confint(object=estimate.w2, level=0.95);

# clean the environment
rm(model2, means.w2, estimate.w2);

##############################################
### fixed treatment by subject interaction ###
## distinct residual variance per cycle ######
##############################################

# R program 6.11

# fit linear fixed-effects model
model3 <- gls(
  model=Pain~
    Treatment+
    id+
    id:Treatment,
  data=gaba_placebo_miss,
  weights=varIdent(form=~1 | Cycle)
);

summary(model3); # summary table

## weighted estimate ##
means.w3 <- emmeans(
  object=model3, specs=~Treatment, weights="cells"
);
estimate.w3 <- pairs(x=means.w3, reverse=TRUE);
print(estimate.w3);
confint(object=estimate.w3, level=0.95);

# clean the environment
rm(model3, means.w3, estimate.w3);

############################
### fixed subject effect ###
############################

# R program 6.12

# fit linear fixed-effects model
model4 <- lm(
  formula=deltaPain~id,
  data=gaba_placebo_diff
);

# summary table
summary(model4);
# Note that for this model the '(Intercept)'
# coefficient is not equal to the sample treatment effect.

## unweighted estimate ##
means.u4 <- emmeans(object=model4, specs=~1);
print(means.u4);
test(object=means.u4);

## weighted estimate ##
means.w4 <- emmeans(
  object=model4, specs=~1, weights="cells"
);
print(means.w4);
test(object=means.w4);

# clean the environment
rm(model4, means.w4);
rm(means.u4);
