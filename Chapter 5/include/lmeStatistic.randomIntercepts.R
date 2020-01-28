# R program C.10
#
# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Define 'lmeStatistic.randomIntercepts' class.
#
# Remarks:
#  None.

setClass(
  Class="lmeStatistic.randomIntercepts",
  contains="lmeStatistic"
);

initialize.lmeStatistic.randomIntercepts <- function(.Object, ...) {
  .Object <- methods::callNextMethod(.Object, ...);
  .Object@estimate <- estimateITE.lme.randomIntercepts; # slot is forced to this function
  return(.Object);
} # initialize.lmeStatistic.randomIntercepts

setMethod(
  f="initialize",
  signature="lmeStatistic.randomIntercepts",
  definition=initialize.lmeStatistic.randomIntercepts
);
