# IDEAL project
# http://www.ideal.rwth-aachen.de/
#
# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Defines 'lmeStatistic' class.
#
# Remarks:
#  None.

# R program C.9

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
