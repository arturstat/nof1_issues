# IDEAL project
# http://www.ideal.rwth-aachen.de/
#
# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Computes a statistic.
#
# Remarks:
#  None.

# R program C.18

statistic.dots <- function(
  func,
  args,
  stat,
  dots
) {
  ret <- do.call(
    what=stat,
    args=list(
      do.call(what=func, args=args),
      unlist(dots)
    )
  );
  return(ret);
} # statistic.dots
