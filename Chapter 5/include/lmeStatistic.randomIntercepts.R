# IDEAL project
# http://www.ideal.rwth-aachen.de/
#
# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Defines 'lmeStatistic.randomIntercepts' class.
#
# Remarks:
#  None.

# R program C.10

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
