# IDEAL project
# http://www.ideal.rwth-aachen.de/
#
# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Simulates a dataset and computes a statistic from it.
#  Simulations are replicated a given number of times.
#  Parallel computation is implemented through package 'parallel'.
#
# Parameters:
# func        function which simulates the data.
# args        list of arguments to 'func'.
# stat        function which takes the object returned
#             by 'func', and computes a statistic.
# measure     function which computes a performance measure
#             from the estimand and the estimate.
# nsim        number of runs per parallel thread.
# cl          a parallel cluster from package 'parallel'.
#             if NULL, a parallel cluster is created
#             on the local machine with the total number
#             of threads available on that machine.
# ncpus       number of threads for parallel computation.
#             is ignored when 'cl' is not NULL.
# iseed       seed for the Random Number Generator.
#             is ignored when 'cl' is not NULL.
# ...         additional arguments are passed to 'stat'.
#
# Return value:
#  An object of class 'boot.statistic' containing
# a 'parameter' data.frame, an 'estimate' data.frame,
# and a 'measure' data.frame.
#  The data.frames returned each have a total number of rows
# equal to the number of rows returned by each run of 'stat',
# times the number of runs per parallel thread,
# times the number of threads in the parallel cluster.
#
# Remarks:
#  None.

# R program C.2

bootR <- function(
  func, # function which simulates the data
  args, # list of arguments to 'func'
  stat, # function which computes a statistic
  measure=squareError, # function which computes a performance measure
  nsim=1000, # number of runs per parallel thread
  cl = NULL, # parallel cluster
  ncpus=NULL, # number of parallel threads
  iseed=NULL, # seed for RNG
  ... # additional arguments are passed to 'stat'
) {
  ## check arguments
  object <- checkArgs.boot(
    func, args, stat, measure, nsim, cl, ncpus, iseed, ...
  );
  if ( is.character(object) ) stop(object);
  
  nsim <- trunc(nsim);
  if ( !is.null(ncpus) ) ncpus <- trunc(ncpus);
  if ( !is.null(iseed) ) iseed <- trunc(iseed);
  
  ronum <- nrow(object[[1]]);
  cname <- colnames(x=object[[1]]);
  
  # determine storage modes for data.frame
  modes <- character( length=ncol(object[[1]]) );
  for ( i in seq_along(modes) ) {
    modes[i] <- typeof(object[[1]][,i]);
  }
  
  rm(object); # remove object
  
  dots <- list(...); # get additional arguments
  
  if (length(dots)==0) {statistic <- statistic.args;}
  else {statistic <- statistic.dots;}
  
  ## create parallel cluster on local machine
  if ( is.null(cl) ) {
    nnodes <- if ( is.null(ncpus) ) {parallel::detectCores();}
    else {ncpus;}
    cl <- tryCatch( # atempt a 'FORK' cluster
      expr={
        parallel::makeCluster(spec=nnodes, type="FORK");
      },
      error=function(e) {e;}
    );
    if ( inherits(x=cl, what="error") ) { # if it does not work
      # create a "PSOCK" cluster
      cl <- parallel::makeCluster(spec=nnodes, type="PSOCK");
    }
    rm(nnodes); # remove object
    parallel::clusterSetRNGStream(cl=cl, iseed=iseed); # set seed on parallel cluster
    noCL <- TRUE;
  } else noCL <- FALSE;
  
  ## export objects to parallel cluster
  parallel::clusterExport(
    cl=cl,
    varlist=c(
      "newFrame",
      "func",
      "args",
      "stat",
      "nsim",
      "dots",
      "statistic",
      "ronum",
      "cname",
      "modes"
    ),
    envir=environment()
  );
  
  ## evaluate expression on parallel cluster
  clret <- parallel::clusterEvalQ(
    cl=cl,
    expr={
      ## list for saving the whole data
      ret <- vector(mode="list", length=2);
      ret[[1]] <- newFrame(modes=modes, nrow=nsim*ronum, col.names=cname);
      ret[[2]] <- newFrame(modes=modes, nrow=nsim*ronum, col.names=cname);

      ## run simulations
      for (i in 1:nsim) {
        object <- statistic(func, args, stat, dots);
        s <- ronum*(i-1)+1; e <- ronum*i; # compute indexes
        ret[[1]][s:e,] <- object[[1]];
        ret[[2]][s:e,] <- object[[2]];
      }
      rm(i, object, s, e); # remove objects
      ret; # return object
    }
  );
  
  rm(dots, statistic); # remove objects
  
  ## stop parallel cluster
  if (noCL) {
    parallel::stopCluster(cl=cl);
    rm(cl);
  }
  rm(noCL);
  
  ## define object to be returned
  ret <- methods::new(
    Class="boot.statistic",
    parameter=newFrame(
      modes=modes,
      nrow=length(clret)*nsim*ronum,
      col.names=cname
    ),
    estimate=newFrame(
      modes=modes,
      nrow=length(clret)*nsim*ronum,
      col.names=cname
    ),
    measure=newFrame(
      modes=modes,
      nrow=length(clret)*nsim*ronum,
      col.names=cname
    )
  );
  
  rm(modes, cname); # remove objects
  
  ## avoid the use of 'cbind' and 'rbind'
  for ( i in 1:length(clret) ) {
    s <- nsim*ronum*(i-1)+1; e <- nsim*ronum*i;
    ret@parameter[s:e,] <- clret[[i]][[1]];
    ret@estimate[s:e,] <- clret[[i]][[2]];
    ret@measure[s:e,] <- measure(clret[[i]][[1]], clret[[i]][[2]]);
  }
  
  rm(clret, i, s, e); # remove objects
  
  if ( (numFail <- sum( is.na(ret@estimate[,1]) )/ronum) > 0 ) {
    warning("some bootstrap runs failed (", numFail, "/", nrow(ret@estimate)/ronum, ")");
    index <- which( !stats::complete.cases(ret@estimate*0) );
    ret@parameter <- ret@parameter[-index, , drop=FALSE];
    ret@estimate <- ret@estimate[-index, , drop=FALSE];
    ret@measure <- ret@measure[-index, , drop=FALSE];
    rm(index);
  }
  
  rm(ronum, numFail); # remove object
  
  return(ret);
} # bootR
