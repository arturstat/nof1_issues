# IDEAL project
# http://www.ideal.rwth-aachen.de/
#
# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Computes the square error.
#
# Remarks:
#  None.

# R program C.16

squareError <- function(
  true, # the estimand
  estimate # the estimate
) {
  return( (true-estimate)^2 );
} # squareError
