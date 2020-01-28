# R program C.19
#
# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Create figure 3.
#
# Remarks:
#  Needs several hours to complete.

files <- c(
  "boot.statistic.R", "bootR.R",
  "checkArgs.boot.R", "checkArgs.randomIntercepts.R",
  "checkObject.statistic.R", "dpill.R",
  "estimateITE.lme.randomIntercepts.R", "lmeModel.R",
  "lmeStatistic.R", "lmeStatistic.randomIntercepts.R",
  "locpoly.R", "naive.R", "newFrame.R",
  "randomIntercepts.R", "shrunk.R", "squareError.R",
  "statistic.args.R", "statistic.dots.R"
);

# define working directory
while ( !all( files %in% list.files(path="./include") ) ) {
  file <- file.choose();# choose this file
  WorkingDir  <- dirname(file);# get path to file
  setwd(dir=WorkingDir); # define working directory
  rm(file, WorkingDir); # remove objects
}

# query working directory
#getwd();

# list files in working directory
#list.files();

# get list of files to source
files <- list.files(path="./include");

# source files
for (file in files) {
  source(
    file=paste("./include/", file, sep="")
  );
}
rm(file);

iseed <- 3141593; # seed for RNG

tau <- 0; # population average of true individual treatment effect
n <- 3; # number of cycles per subject
psi <- 2; # variance of the true individual treatment effect
sigma <- 1; # residual variance

# probability for the computations
# of the x-axis limits of the plot
prob <- 0.999;

# colors for each line in the plot
color <- rainbow(n=9);

# weight for the shrunk estimate
k <- n*psi/(n*psi+sigma);

a <- qnorm(
  p=(1+c(-1, 1)*prob)/2,
  mean=tau,
  sd=sqrt( k^2*(psi+sigma/n) )
); # shrunk

b <- qnorm(
  p=(1+c(-1, 1)*prob)/2,
  mean=tau,
  sd=sqrt(psi+sigma/n)
); # naive

# compute x-axis limits
xlim <- (a+b)/2;
xlim[1] <- floor(xlim[1]);
xlim[2] <- ceiling(xlim[2]);

# create parallel cluster
cl <- parallel::makeCluster(
  spec=parallel::detectCores(), type="PSOCK"
);

# export list of files to cluster
parallel::clusterExport(
  cl=cl, varlist=c("files"), envir=environment()
);

# source files in cluster
invisible(
  parallel::clusterEvalQ(
    cl=cl,
    expr={
      for (file in files) {
        source(
          file=paste("./include/", file, sep="")
        );
      }
      rm(files, file);
    }
  )
);

rm(files);

#############################
####### Simulate data #######
#############################

# set seed on parallel cluster
parallel::clusterSetRNGStream(cl=cl, iseed=iseed);

system.time(
  boot1 <- bootR(
    func=randomIntercepts, # simulation function
    args=list(
      size=as.integer( rep(x=n, times=10) ),
      fixed=tau,
      random=list(psi=psi, sigma=sigma),
      model=list()
    ), # list of arguments to simulation function
    stat=naive, # statistic function
    nsim=1e5, # number of simulations
    cl=cl # parallel cluster
  )
); # 1 subject per series of n-of-1 trials

# set seed on parallel cluster
parallel::clusterSetRNGStream(cl=cl, iseed=iseed);

system.time(
  boot2 <- bootR(
    func=randomIntercepts, # simulation function
    args=list(
      size=as.integer( rep(x=n, times=2) ),
      fixed=tau,
      random=list(psi=psi, sigma=sigma),
      model=list()
    ), # list of arguments to simulation function
    stat=shrunk, # statistic function
    nsim=5e5, # number of simulations
    cl=cl # parallel cluster
  )
); # 2 subjects per series of n-of-1 trials

# set seed on parallel cluster
parallel::clusterSetRNGStream(cl=cl, iseed=iseed);

system.time(
  boot3 <- bootR(
    func=randomIntercepts, # simulation function
    args=list(
      size=as.integer( rep(x=n, times=3) ),
      fixed=tau,
      random=list(psi=psi, sigma=sigma),
      model=list()
    ), # list of arguments to simulation function
    stat=shrunk, # statistic function
    nsim=333333, # number of simulations
    cl=cl # parallel cluster
  )
); # 3 subjects per series of n-of-1 trials

# set seed on parallel cluster
parallel::clusterSetRNGStream(cl=cl, iseed=iseed);

system.time(
  boot4 <- bootR(
    func=randomIntercepts, # simulation function
    args=list(
      size=as.integer( rep(x=n, times=4) ),
      fixed=tau,
      random=list(psi=psi, sigma=sigma),
      model=list()
    ), # list of arguments to simulation function
    stat=shrunk, # statistic function
    nsim=2.5e5, # number of simulations
    cl=cl # parallel cluster
  )
); # 4 subjects per series of n-of-1 trials

# set seed on parallel cluster
parallel::clusterSetRNGStream(cl=cl, iseed=iseed);

system.time(
  boot5 <- bootR(
    func=randomIntercepts, # simulation function
    args=list(
      size=as.integer( rep(x=n, times=5) ),
      fixed=tau,
      random=list(psi=psi, sigma=sigma),
      model=list()
    ), # list of arguments to simulation function
    stat=shrunk, # statistic function
    nsim=2e5, # number of simulations
    cl=cl # parallel cluster
  )
); # 5 subjects per series of n-of-1 trials

# set seed on parallel cluster
parallel::clusterSetRNGStream(cl=cl, iseed=iseed);

system.time(
  boot10 <- bootR(
    func=randomIntercepts, # simulation function
    args=list(
      size=as.integer( rep(x=n, times=10) ),
      fixed=tau,
      random=list(psi=psi, sigma=sigma),
      model=list()
    ), # list of arguments to simulation function
    stat=shrunk, # statistic function
    nsim=1e5, # number of simulations
    cl=cl # parallel cluster
  )
); # 10 subjects per series of n-of-1 trials

# set seed on parallel cluster
parallel::clusterSetRNGStream(cl=cl, iseed=iseed);

system.time(
  boot20 <- bootR(
    func=randomIntercepts, # simulation function
    args=list(
      size=as.integer( rep(x=n, times=20) ),
      fixed=tau,
      random=list(psi=psi, sigma=sigma),
      model=list()
    ), # list of arguments to simulation function
    stat=shrunk, # statistic function
    nsim=5e4, # number of simulations
    cl=cl # parallel cluster
  )
); # 20 subjects per series of n-of-1 trials

# set seed on parallel cluster
parallel::clusterSetRNGStream(cl=cl, iseed=iseed);

system.time(
  boot100 <- bootR(
    func=randomIntercepts, # simulation function
    args=list(
      size=as.integer( rep(x=n, times=100) ),
      fixed=tau,
      random=list(psi=psi, sigma=sigma),
      model=list()
    ), # list of arguments to simulation function
    stat=shrunk, # statistic function
    nsim=1e4, # number of simulations
    cl=cl # parallel cluster
  )
); # 100 subjects per series of n-of-1 trials

# set seed on parallel cluster
parallel::clusterSetRNGStream(cl=cl, iseed=iseed);

system.time(
  boot1000 <- bootR(
    func=randomIntercepts, # simulation function
    args=list(
      size=as.integer( rep(x=n, times=1000) ),
      fixed=tau,
      random=list(psi=psi, sigma=sigma),
      model=list()
    ), # list of arguments to simulation function
    stat=shrunk, # statistic function
    nsim=1e3, # number of simulations
    cl=cl # parallel cluster
  )
); # 1000 subjects per series of n-of-1 trials

parallel::stopCluster(cl=cl);

###############################
###### Compute bandwidth ######
###############################

system.time(
  h1 <- dpill(boot1)
); # 1 subject per series of n-of-1 trials

system.time(
  h2 <- dpill(boot2)
); # 2 subjects per series of n-of-1 trials

system.time(
  h3 <- dpill(boot3)
); # 3 subjects per series of n-of-1 trials

system.time(
  h4 <- dpill(boot4)
); # 4 subjects per series of n-of-1 trials

system.time(
  h5 <- dpill(boot5)
); # 5 subjects per series of n-of-1 trials

system.time(
  h10 <- dpill(boot10)
); # 10 subjects per series of n-of-1 trials

system.time(
  h20 <- dpill(boot20)
); # 20 subjects per series of n-of-1 trials

system.time(
  h100 <- dpill(boot100)
); # 100 subjects per series of n-of-1 trials

system.time(
  h1000 <- dpill(boot1000)
); # 1000 subjects per series of n-of-1 trials

#######################################
### Compute local linear regression ###
#######################################

system.time(
  fit1 <- locpoly(
    x=boot1, bandwidth=h1, range.x=xlim, truncate=TRUE
  )
); # 1 subject per series of n-of-1 trials

system.time(
  fit2 <- locpoly(
    x=boot2, bandwidth=h2, range.x=xlim, truncate=TRUE
  )
); # 2 subjects per series of n-of-1 trials

system.time(
  fit3 <- locpoly(
    x=boot3, bandwidth=h3, range.x=xlim, truncate=TRUE
  )
); # 3 subjects per series of n-of-1 trials

system.time(
  fit4 <- locpoly(
    x=boot4, bandwidth=h4, range.x=xlim, truncate=TRUE
  )
); # 4 subjects per series of n-of-1 trials

system.time(
  fit5 <- locpoly(
    x=boot5, bandwidth=h5, range.x=xlim, truncate=TRUE
  )
); # 5 subjects per series of n-of-1 trials

system.time(
  fit10 <- locpoly(
    x=boot10, bandwidth=h10, range.x=xlim, truncate=TRUE
  )
); # 10 subjects per series of n-of-1 trials

system.time(
  fit20 <- locpoly(
    x=boot20, bandwidth=h20, range.x=xlim, truncate=TRUE
  )
); # 20 subjects per series of n-of-1 trials

system.time(
  fit100 <- locpoly(
    x=boot100, bandwidth=h100, range.x=xlim, truncate=TRUE
  )
); # 100 subjects per series of n-of-1 trials

system.time(
  fit1000 <- locpoly(
    x=boot1000, bandwidth=h1000, range.x=xlim, truncate=TRUE
  )
); # 1000 subjects per series of n-of-1 trials

##########################
###### Plot results ######
##########################

tiff(
  filename="Figure3.tif",
  width=1920,
  height=1080,
  units="px",
  pointsize=2,
  compression="lzw",
  res=400,
  bg="white",
  type="cairo"
);

old <- par();

par(
  mar=c(
    7.1, # bottom margin default 5.1
    9.1, # left margin default 4.1
    4.1, # top margin default 4.1
    2.1 # right margin default 2.1
  )
);

plot(
  fit1,
  xlim=xlim,
  ylim=c(0, 1),
  xlab="",
  ylab="",
  type="l",
  col=color[1],
  lty=1,
  lwd=0.75,
  bty="n",
  axes=FALSE
); # 1 subject per series of n-of-1 trials

# define x-axis
axis(
  side=1,
  at=seq(from=xlim[1], to=xlim[2], by=2),
  labels=TRUE,
  tick=TRUE,
  outer=FALSE,
  lty="solid",
  lwd=1.25,
  lwd.ticks=1.25,
  cex.axis=2.5,
  mgp=c(-3, -1, -3)
);

# define y-axis
axis(
  side=2,
  at=seq(from=0, to=1, by=0.2),
  labels=TRUE,
  tick=TRUE,
  outer=FALSE,
  lty="solid",
  lwd=1.25,
  lwd.ticks=1.25,
  cex.axis=2.5,
  mgp=c(3, 1, 0)
);

# define xy-axis label
title(
  xlab=expression( hat(theta) ),
  ylab=expression(
    paste("MSE[", theta, "|", hat(theta), "]")
  ),
  line=5,
  cex.lab=3
);

lines(
  fit2, col=color[2], lty=2, lwd=0.75
); # 2 subjects per series of n-of-1 trials

lines(
  fit3, col=color[3], lty=3, lwd=0.75
); # 3 subjects per series of n-of-1 trials

lines(
  fit4, col=color[4], lty=4, lwd=0.75
); # 4 subjects per series of n-of-1 trials

lines(
  fit5, col=color[5], lty=5, lwd=0.75
); # 5 subjects per series of n-of-1 trials

lines(
  fit10, col=color[6], lty=6, lwd=0.75
); # 10 subjects per series of n-of-1 trials

lines(
  fit20, col=color[7], lty=7, lwd=0.75
); # 20 subjects per series of n-of-1 trials

lines(
  fit100, col=color[8], lty=8, lwd=0.75
); # 100 subjects per series of n-of-1 trials

lines(
  fit1000, col=color[9], lty=9, lwd=0.75
); # 1000 subjects per series of n-of-1 trials

legend(
  x="top",
  legend=c(
    1, 2, 3,
    4, 5, 10,
    20, 100, 1000
  ),
  col=color,
  lty=1:9,
  lwd=0.75,
  bty="n",
  cex=2.5,
  xjust=0,
  yjust=0,
  ncol=1,
  horiz=FALSE
);

par(old);

dev.off();
