# IDEAL project
# http://www.ideal.rwth-aachen.de/

# R program C.12

invisible(
  setGeneric(
    name="naive",
    def=function(object) {
      standardGeneric("naive")
    },
    valueClass="list"
  )
);

# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Computes naive means.
#
# Parameters:
# object      an object inheriting from class
#             'lmeStatistic.randomIntercepts'.
#
# Return value:
#   A list of 'parameter' and 'estimate'.
#
# Remarks:
#  None.

naive.lmeStatistic.randomIntercepts <- function(
  object
) {
  statistic <- as.matrix(
    by(
      data=object@data[,all.vars(object@fixed)],
      INDICES=object@data[,all.vars(object@random)],
      FUN=mean
    )
  ); # compute naive estimates
  colnames(statistic) <- colnames(object@parameter);
  rownames(statistic) <- rownames(object@parameter);
  
  ret <- list(
    "parameter"=object@parameter,
    "estimate"=statistic
  );
  
  rm(statistic); # remove object
  
  return(ret);
} # naive.lmeStatistic.randomIntercepts

setMethod(
  f="naive",
  signature="lmeStatistic.randomIntercepts",
  definition=naive.lmeStatistic.randomIntercepts
);
