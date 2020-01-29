# Written by Artur Araujo
# July 2018

# artur.stat@gmail.com

# while working
# on the IDEAL project

# http://www.ideal.rwth-aachen.de/

# R program B.2

nof1.pwr.t <- function(
  ncycle, # number of cycles
  nsubject, # number of subjects
  psi, # interactive sd
  sigma, # residual sd
  delta, # effect size
  alpha=0.025, # significance level
  power=0.8 # power
) {
  if ( missing(nsubject) & missing(ncycle) )
    stop("either argument 'nsubject' or 'ncycle' must be specified!");
  if (psi < 0)
    stop("argument 'psi' must be non-negative!");
  if (sigma < 0)
    stop("argument 'sigma' must be non-negative!");
  if (alpha < 0 | alpha > 1)
    stop("argument 'alpha' must lie between 0 and 1!");
  if (power < 0 | power > 1)
    stop("argument 'power' must lie between 0 and 1!");
  if ( missing(ncycle) ) {
    if (nsubject <= 1)
      stop("argument 'nsubject' must be greater than 1!");
    s1 <- psi^2/delta^2*( qnorm(p=1-alpha)+qnorm(p=power) )^2; s1 <- ceiling(s1);
    repeat {
      s2 <- psi^2/delta^2*( qt(p=1-alpha,df=s1-1)+qt(p=power,df=s1-1) )^2;
      s2 <- ceiling(s2); if (abs(s2-s1) <= 1) break; s1 <- s2;
    }
    if (nsubject < s2)
      stop("argument 'nsubject' must be greater or equal to ", s2, "!");
    t <- ( qt(p=1-alpha, df=nsubject-1)+qt(p=power, df=nsubject-1) )^2;
    n <- sigma^2*t/(nsubject*delta^2-psi^2*t); n <- ceiling(n);
    return(n);
  } else if ( missing(nsubject) ) {
    if (ncycle < 0)
      stop("argument 'ncycle' must be non-negative!");
    s0 <- 1/delta^2*(psi^2+sigma^2/ncycle);
    s1 <- s0*( qnorm(p=1-alpha)+qnorm(p=power) )^2; s1 <- ceiling(s1);
    repeat {
      s2 <- s0*( qt(p=1-alpha,df=s1-1)+qt(p=power,df=s1-1) )^2;
      s2 <- ceiling(s2); if (abs(s2-s1) <= 1) break; s1 <- s2;
    }
    return(s2);
  }
} # nof1.pwr.t
