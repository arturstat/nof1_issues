# IDEAL project
# http://www.ideal.rwth-aachen.de/
#
# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Defines 'boot.statistic' class.
#
# Remarks:
#  None.

# R program C.1

setClass(
  Class="boot.statistic",
  slots=c(
    parameter="data.frame",
    estimate="data.frame",
    measure="data.frame"
  )
);

valid.boot.statistic <- function(
  object
) {
  if (
    nrow(object@parameter) != nrow(object@estimate) |
    nrow(object@parameter) != nrow(object@measure)
  ) return("'parameter', 'estimate', 'measure' number of rows differ");
  if (
    ncol(object@parameter) != ncol(object@estimate) |
    ncol(object@parameter) != ncol(object@measure)
  ) return("'parameter', 'estimate', 'measure' number of columns differ");
  if (
    !all.equal( colnames(object@parameter), colnames(object@estimate) ) |
    !all.equal( colnames(object@parameter), colnames(object@measure) )
  ) return("'parameter', 'estimate', 'measure' column names differ");
  if (
    length( unique( colnames(object@parameter) ) ) !=
    length( colnames(object@parameter) )
  ) return("'parameter', 'estimate', 'measure' column names are not unique");
  if ( is.null( colnames(object@parameter) ) )
    return("'parameter', 'estimate', 'measure' column names are NULL");
  return(TRUE);
} # valid.boot.statistic

invisible(
  setValidity(
    Class="boot.statistic",
    method=valid.boot.statistic
  )
);
