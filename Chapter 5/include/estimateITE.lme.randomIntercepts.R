# IDEAL project
# http://www.ideal.rwth-aachen.de/
#
# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Estimates individual treatment effects
# from a random intercepts model.
#
# Remarks:
#  None.

# R program C.7

estimateITE.lme.randomIntercepts <- function(
  object
) {
  var <- all.vars(object$call$random); # get subject variable
  if (length(var)==0) { # add support for alternative formula
    var <- names( eval(object$call$random) );
  }
  
  ret <- object$coefficients$random[[var]]+
      object$coefficients$fixed; # compute estimate
  
  rm(var); # remove variable
  
  return(ret);
} # estimateITE.lme.randomIntercepts
