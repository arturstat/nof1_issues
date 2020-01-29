# Written by Artur Araujo
# November 2019

# artur.stat@gmail.com

# while working
# on the IDEAL project

# http://www.ideal.rwth-aachen.de/

# define working directory
while ( !"ttest.R" %in% list.files() ) {
  file <- file.choose();# choose this file
  WorkingDir  <- dirname(file);# get path to file
  setwd(dir=WorkingDir); # define working directory
  rm(file, WorkingDir); # remove objects
}

if (
  !all(
    c(
      "gaba_placebo",
      "gaba_placebo_diff"
    ) %in% ls()
  )
) {
  source(file="./data_normal.R");
}

# R program 6.1

## Two-sided paired t-test 5% level ##
t.test(
  x=gaba_placebo_diff$Pain_Gabapentin,
  y=gaba_placebo_diff$Pain_Placebo,
  alternative="two.sided",
  mu=0,
  paired=TRUE,
  var.equal=TRUE,
  conf.level=0.95
);

# R program 6.2

## Two-sided one sample t-test 5% level ##
t.test(
  x=gaba_placebo_diff$deltaPain,
  alternative="two.sided",
  mu=0,
  conf.level=0.95
);

# R program 6.3

# change reference treatment
gaba_placebo <- within(
  data=gaba_placebo,
  expr={
    Treatment <- relevel(
      x=Treatment,
      ref=levels(Treatment)[2]
    );
  }
);

## Two-sided paired t-test 5% level ##
# Alternative dataset #
t.test(
  formula=Pain~Treatment,
  data=gaba_placebo,
  alternative="two.sided",
  mu=0,
  paired=TRUE,
  var.equal=TRUE,
  conf.level=0.95,
  na.action=na.pass
);

# change back reference treatment
gaba_placebo <- within(
  data=gaba_placebo,
  expr={
    Treatment <- relevel(
      x=Treatment,
      ref=levels(Treatment)[2]
    );
  }
);
