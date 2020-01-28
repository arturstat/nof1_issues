# R program C.16
#
# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Compute square error.
#
# Remarks:
#  None.

squareError <- function(
  true, # the estimand
  estimate # the estimate
) {
  return( (true-estimate)^2 );
} # squareError
