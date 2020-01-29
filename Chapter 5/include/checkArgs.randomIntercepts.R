# IDEAL project
# http://www.ideal.rwth-aachen.de/
#
# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Checks arguments to randomIntercepts function.
#
# Remarks:
#  None.

# R program C.4

checkArgs.randomIntercepts <- function(
  size,
  fixed,
  random,
  model
) {
  # check 'size'
  if ( !is.integer(size) )
    return("'size' must be 'integer'");
  if ( any(size < 1) )
    return("elements of 'size' must be greater or equal to 1");
  
  # check 'fixed'
  if ( !is.numeric(fixed) )
    return("'fixed' must be 'numeric'");
  if ( length(fixed) < 1)
    return("'fixed' must be a vector with at least 1 element");
  
  # check 'random'
  if ( !is.list(random) )
    return("'random' must be a list");
  if (length(random)!=2)
    return("'random' must be a list with 2 elements");
  if ( !all( names(random) %in% c("psi", "sigma") ) )
    return("'random' must be a list with names 'psi', 'sigma'");
  for (i in 1:2) {
    if ( !is.numeric(random[[i]]) )
      return(
        paste(
          "'random$",
          names(random)[i],
          "' must be 'numeric'",
          sep=""
        )
      );
    if (random[[i]] < 0)
      return(
        paste(
          "'random$",
          names(random)[i],
          "' must be non-negative",
          sep=""
        )
      );
  }
  
  # check 'model'
  # the following code is taken from 'arima.sim'
  if ( !is.list(model) )
    return("'model' must be list");
  p <- length(model$ar);
  if (p) {
    minroots <- min( Mod( polyroot( c(1, -model$ar) ) ) );
    if (minroots <= 1)
      return("'ar' part of model is not stationary");
    rm(minroots);
  }
  q <- length(model$ma);
  d <- 0;
  if ( !is.null(ord <- model$order) ) {
    if (length(ord) != 3L)
      return("'model$order' must be of length 3");
    if (p != ord[1L])
      return("inconsistent specification of 'ar' order");
    if (q != ord[3L])
      return("inconsistent specification of 'ma' order");
    d <- ord[2L];
    if (d != round(d) || d < 0)
      return("number of differences must be a positive integer");
    rm(ord);
  }
  rm(p, q, d);
  
  return(NULL);
} # checkArgs.randomIntercepts
