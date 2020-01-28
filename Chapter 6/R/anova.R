# Written by Artur Araujo

# artur.stat@gmail.com
# aamarinhodearaujo1@sheffield.ac.uk

# November 2019

# define working directory
while ( !"anova.R" %in% list.files() ) {
  file <- file.choose();# choose this file
  WorkingDir  <- dirname(file);# get path to file
  setwd(dir=WorkingDir); # define working directory
  rm(file, WorkingDir); # remove objects
}

if ( !"gaba_placebo" %in% ls() ) {
  source(file="./data_normal.R");
}

# R program 6.5

if ( !"car" %in% rownames( installed.packages() ) )
  {install.packages(pkgs="car");}
library(package=car);

if ( !"emmeans" %in% rownames( installed.packages() ) )
  {install.packages(pkgs="emmeans");}
library(package=emmeans);

##############################################
### ANOVA Treatment by Subject interaction ###
##############################################

# R program 6.6

replications(
  formula=Pain~Treatment+id+Treatment:id,
  data=gaba_placebo
);

aov0 <- aov(
  formula=Pain~Treatment+id+Treatment:id,
  data=gaba_placebo,
  contrasts=list(Treatment=contr.sum, id=contr.sum)
);

# type I SS
summary(object=aov0);

# type II SS
Anova(mod=aov0, type=2);

# type III SS
Anova(mod=aov0, type=3);

# report the means
means.aov0 <- emmeans(
  object=aov0,
  specs=~Treatment,
  weights="cells"
);
print(means.aov0);

# clean the environment
rm(aov0, means.aov0);
