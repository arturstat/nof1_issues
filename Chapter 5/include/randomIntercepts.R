# IDEAL project
# http://www.ideal.rwth-aachen.de/
#
# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Simulates data for the random intercepts model.
#
# Parameters:
# size        vector of number of measurements for each subject.
#             elements are truncated to the nearest integer.
# fixed			  population mean.
# random		  list of 'psi', 'sigma'.
#             'psi' is the interactive variance.
#             'sigma' is the residual variance.
# model		    list of ARIMA parameters.
#             see 'arima.sim' from 'stats' package.
# checkArgs   if TRUE checks the arguments.
#
# Return value:
#  An object of class 'lmeStatistic.randomIntercepts'.
#
# Remarks:
#  None.

# R program C.14

randomIntercepts <- function(
  size, # Vector of number of measurements for each subject.
  fixed, # Population mean.
  random, # list of 'psi', 'sigma'.
  model=list(), # list of ARIMA parameters.
  checkArgs=TRUE
) {
  if ( !is.integer(size) ) size <- as.integer(size);
  
  # check arguments
  if (checkArgs) {
    message <- checkArgs.randomIntercepts(
      size,
      fixed,
      random,
      model
    );
    if ( !is.null(message) ) stop(message);
    rm(message);
  }
  
  numO <- sum(size); # total number of measurements
  
  # setup matrix to hold true individual treatment effects
  parameter <- matrix(data=NA, nrow=length(size), ncol=1);
  rownames(parameter) <- 1:length(size);
  colnames(parameter) <- "(Intercept)";
  
  # setup data.frame to hold the simulated data
  data <- data.frame(
    "subject"=vector(mode="numeric", length=numO), # the grouping variable
    "cycle"=vector(mode="integer", length=numO), # the position variable
    "outcome"=vector(mode="numeric", length=numO)
  );
  
  rm(numO); # remove object
  
  i <- 1; j <- 0; # initialize indexes
  while ( i <= length(size) ) { # loop through the subjects
    s <- j+1; e <- j+size[i]; # define start and end indexes
    
    data$subject[s:e] <- i; # save subject index
    data$cycle[s:e] <- 1:size[i]; # save cycle index
    
    # individual mean
    data$outcome[s:e] <- parameter[i,1] <- stats::rnorm(
      n=1,
      mean=fixed[1],
      sd=sqrt(random$psi)
    );
    
    # add residual error
    data$outcome[s:e] <- data$outcome[s:e]+
      stats::arima.sim(
        model=model,
        n=size[i],
        rand.gen=stats::rnorm,
        mean=0,
        sd=sqrt(random$sigma)
      );
    
    i <- i+1; j <- e; # update indexes
  }
  
  # remove objects
  rm(i ,j, s, e);
  
  data$subject <- as.factor(data$subject);
  
  p <- length(model$ar); q <- length(model$ma);
  if (p==0 & q==0) correlation <- NULL
  # cycle is the position variable
  else if (p==1 & q==0) correlation <- nlme::corAR1(form=~cycle|subject)
  # subject is the grouping variable
  else correlation <- nlme::corARMA(form=~cycle|subject, p=p, q=q);
  rm(p, q);
  
  ret <- methods::new(
    Class="lmeStatistic.randomIntercepts",
    parameter=parameter,
    data=data,
    fixed=outcome~1,
    random=~1|subject,
    groups=NULL,
    correlation=correlation,
    weights=NULL,
    method="REML"
  );
  
  # remove objects
  rm(parameter);
  rm(data);
  rm(correlation);
  
  return(ret);
} # randomIntercepts
