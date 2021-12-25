# Written by Artur Araujo
# November 2019

# artur.stat@gmail.com

# while working
# on the IDEAL project

# http://www.ideal.rwth-aachen.de/

# define working directory
while ( !"forest_plot_normal.R" %in% list.files() ) {
  file <- file.choose();# choose this file
  WorkingDir  <- dirname(file);# get path to file
  setwd(dir=WorkingDir); # define working directory
  rm(file, WorkingDir); # remove objects
}

if ( !"gaba_placebo_diff_miss" %in% ls() ) {
  source(file="./data_normal.R");
}

# R program 6.30

# Load lattice package
library(lattice);

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

###################
### Forest plot ###
###################

# R program 6.31

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
get_elapsed_time(fit_random_intercepts);

# define function
my_data <- function(
  object,
  pte,
  ite,
  id,
  order="upr",
  decreasing=FALSE,
  probs=c(0.025, 0.975)
) {
  ret <- as.data.frame(
    x=summary(
      object=object,
      pars=c(pte, ite),
      probs=probs
    )$summary
  )[,c(1, 4, 5)];
  names(ret) <- c("mean", "lwr", "upr");
  ret$id <- c("population", id);
  o <- switch (order,
    "lwr"=order(ret$lwr, decreasing=decreasing),
    "upr"=order(ret$upr, decreasing=decreasing)
  );
  ret <- ret[c(1, o[o!=1]),];
  ret$id <- factor(x=ret$id, levels=ret$id);
  return(ret);
} # my_data

# define panel function
my_panel <- function(
  x,
  y,
  lwr,
  upr,
  ref,
  x.text,
  y.text,
  labels.text,
  col.text,
  ...
) {
  pch.points <- rep( x=16, times=length(y) );
  pch.points[y=="population"] <- 18;
  panel.points(
    x,
    y,
    type="p", # 'p' for points
    cex=0.25, # character size
    pch=pch.points, # character
    col="black", # color
    ...
  ); # plot point estimates
  col.segments <- rep( x="blue", times=length(y) );
  col.segments[y=="population"] <- "red";
  panel.segments(
    x0=lwr,
    y0=y,
    x1=upr,
    y1=y,
    col=col.segments,
    lty=1,
    lwd=0.25
  ); # plot interval estimates
  panel.abline(
    v=ref,
    col="black",
    lty=2,
    lwd=0.25
  ); # plot reference line
  panel.abline(
    h=0.4,
    col="black",
    lty=1,
    lwd=0.25
  ); # plot x-axis line
  panel.text(
    x=x.text,
    y=y.text,
    labels=labels.text,
    cex=0.35,
    col=col.text,
    lty=1,
    lwd=0.35
  ); # plot text
} # my_panel

# define prepanel function
my_prepanel <- function(
  x,
  y,
  lwr,
  upr,
  ...
) {
  xlim <- range(lwr, upr);
  xlim <- xlim+c(-0.05, 0.05)*diff(xlim);
  xlim <- trunc(xlim);
  ret <- list(xlim=xlim);
  return(ret);
} # my_prepanel

# prepare data for plotting
sum_random_intercepts <- my_data(
  object=fit_random_intercepts,
  pte="pte",
  ite="ITE",
  id=levels(gaba_placebo_diff_miss$id)
);

# create trellis object
trellis_random_intercepts <- with(
  data=sum_random_intercepts,
  expr={
    stripplot(
      id~mean,
      xlab="",
      ylab="",
      par.settings=list(
        axis.line=list(col="transparent")
      ),
      scales=list(
        draw=TRUE,
        relation="free",
        col="black",
        cex=0.25,
        lwd=0.25
      ),
      panel=my_panel,
      prepanel=my_prepanel,
      lwr=lwr,
      upr=upr,
      ref=0,
      x.text=c(-3.5, 1.5),
      y.text=c(5, 5),
      labels.text=paste(
        "Favors",
        levels(gaba_placebo$Treatment)
      ),
      col.text=gray(level=0.25)
    );
  }
);

tiff(
  filename="../forest_plot_R.tiff",
  width=1080,
  height=1080,
  units="px",
  pointsize=1,
  compression="lzw",
  res=300,
  bg="white",
  type="cairo"
);

# plot trellis object
plot(trellis_random_intercepts);

dev.off();

# clean the environment
rm(data_random_intercepts, fit_random_intercepts);
rm(sum_random_intercepts, trellis_random_intercepts);
rm(my_data, my_panel, my_prepanel);
rm(iseed);
