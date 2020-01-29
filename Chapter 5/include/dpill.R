# IDEAL project
# http://www.ideal.rwth-aachen.de/
#
# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Computes bandwidth for
# local polynomial regression.
#
# Remarks:
#  None.

# R program C.6

invisible(
  setGeneric(
    name="dpill",
    def=function(x, ...) {
      standardGeneric("dpill")
    },
    valueClass="ANY"
  )
);

dpill.boot.statistic <- function(
  x,
  y, # ignored for this method
  index=1,
  ...
) {
  index <- trunc(index);
  if ( index > ncol(x@estimate) ) index <- ncol(x@estimate);
  
  # compute bandwidth
  ret <- KernSmooth::dpill(
    x=x@estimate[,index],
    y=x@measure[,index],
    ...
  );
  
  return(ret);
} # dpill.boot.statistic

setMethod(
  f="dpill",
  signature="boot.statistic",
  definition=dpill.boot.statistic
);
