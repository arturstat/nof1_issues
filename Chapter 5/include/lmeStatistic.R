# R program C.9
#
# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Define 'lmeStatistic' class.
#
# Remarks:
#  None.

setClassUnion(
  name="lmeStatistic.parameter",
  members=c(
    "data.frame",
    "matrix"
  )
);

setClass(
  Class="lmeStatistic",
  slots=c(
    parameter="lmeStatistic.parameter",
    estimate="function"
  ),
  contains="lmeModel"
);
