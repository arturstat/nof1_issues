# R program C.11
#
# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Estimate local polynomial regression.
#
# Remarks:
#  None.

invisible(
  setGeneric(
    name="locpoly",
    def=function(x, ...) {
      standardGeneric("locpoly")
    },
    valueClass="ANY"
  )
);

locpoly.boot.statistic <- function(
  x,
  y, # ignored for this method
  index=1,
  ...
) {
  index <- trunc(index);
  if ( index > ncol(x@estimate) ) index <- ncol(x@estimate);
  
  ret <- KernSmooth::locpoly(
    x=x@estimate[,index],
    y=x@measure[,index],
    ...
  );
  
  return(ret);
} # locpoly.boot.statistic

setMethod(
  f="locpoly",
  signature="boot.statistic",
  definition=locpoly.boot.statistic
);
