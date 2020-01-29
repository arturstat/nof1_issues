# IDEAL project
# http://www.ideal.rwth-aachen.de/
#
# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Checks arguments to bootR function.
#
# Remarks:
#  None.

# R program C.3

checkArgs.boot <- function(
  func,
  args,
  stat,
  measure,
  nsim,
  cl,
  ncpus,
  iseed,
  ...
) {
  if ( !is.function(func) )
    return("'func' is not a function");
  if ( !is.list(args) )
    return("'args' is not a list");
  if ( !is.function(stat) )
    return("'stat' is not a function");
  object <- do.call(what=func, args=args);
  chk <- checkObject.statistic( func=stat, args=list(object, ...) );
  rm(object);
  if ( is.character(chk) ) return(chk);
  if ( !is.function(measure) )
    return("'measure' is not a function");
  if (nsim < 1)
    return("'nsim' must be greater or equal to 1");
  if (
    !( is.null(cl) | inherits( x=cl, what=c("SOCKcluster", "cluster") ) )
  ) return("'cl' is not a parallel cluster");
  if ( !is.null(ncpus) ) {
    if (ncpus < 1)
      return("'ncpus' must be greater or equal to 1");
  }
  if ( !( is.null(iseed) | is.numeric(iseed) ) )
    return("'iseed' must be either NULL or numeric");
  return(chk);
} # checkArgs.boot
