# Written by Artur Araujo
# November 2019

# artur.stat@gmail.com

# while working
# on the IDEAL project

# http://www.ideal.rwth-aachen.de/

# define working directory
while ( !"summary_measures.R" %in% list.files() ) {
  file <- file.choose();# choose this file
  WorkingDir  <- dirname(file);# get path to file
  setwd(dir=WorkingDir); # define working directory
  rm(file, WorkingDir); # remove objects
}

if ( !"gaba_placebo_diff" %in% ls() ) {
  source(file="./data_normal.R");
}

#################################
### Summary measures approach ###
#################################

# R program 6.4

# compute per subject means
gaba_placebo_sum <- with(
  data=gaba_placebo_diff,
  expr={
    by(
      data=deltaPain,
      INDICES=id,
      FUN=mean,
      na.rm=TRUE
    );
  }
);

# Two sided one sample t-test 5% level
t.test(
  x=gaba_placebo_sum,
  alternative="two.sided",
  mu=0,
  conf.level=0.95
);

# clean the environment
rm(gaba_placebo_sum);
