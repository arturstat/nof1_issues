# IDEAL project
# http://www.ideal.rwth-aachen.de/

# R program C.15

invisible(
  setGeneric(
    name="shrunk",
    def=function(object) {
      standardGeneric("shrunk")
    },
    valueClass="list"
  )
);

# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Computes shrunk means from fitting a linear
# mixed-effects model with 'lme' from package 'nlme'.
#
# Parameters:
# object      an object inheriting
#             from class 'lmeStatistic'.
#
# Return value:
#   A list of 'parameter' and 'estimate'.
#
# Remarks:
#  None.

shrunk.lmeStatistic <- function(
  object
) {
  statistic <- tryCatch(
    expr={
      fit <- if ( is.null(object@groups) ) {
        nlme::lme(
          data=object@data,
          fixed=object@fixed,
          random=object@random,
          correlation=object@correlation,
          weights=object@weights,
          method=object@method
        );
      } else {
        nlme::lme(
          data=object@data,
          fixed=object@fixed,
          random=object@random,
          groups=object@groups,
          correlation=object@correlation,
          weights=object@weights,
          method=object@method
        );
      } # fit linear mixed-effects model
      
      # fix call
      fit$call$fixed <- object@fixed;
      fit$call$random <- object@random;
      fit$call$correlation <- object@correlation;
      fit$call$weights <- object@weights;
      fit$call$method <- object@method;
      
      # compute shrunk estimates
      object@estimate(fit); # return object
    },
    error=function(e) {e;}
  );
  
  if ( inherits(statistic, "error") ) {
    statistic <- object@parameter;
    statistic[,] <- NA;
  }
  
  ret <- list(
    "parameter"=object@parameter,
    "estimate"=statistic
  );
  
  rm(statistic); # remove object
  
  return(ret);
} # shrunk.lmeStatistic

setMethod(
  f="shrunk",
  signature="lmeStatistic",
  definition=shrunk.lmeStatistic
);
