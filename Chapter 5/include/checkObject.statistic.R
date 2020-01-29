# IDEAL project
# http://www.ideal.rwth-aachen.de/
#
# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Checks if 'stat' function returns
# an object with the required structure.
#
# Remarks:
#  This function is required by
# 'checkArgs.boot' function.

# R program C.5

checkObject.statistic <- function(
  func,
  args
) {
  chk <- do.call(what=func, args=args);
  if ( !is.list(chk) )
    return(
      paste(
        "function '",
        deparse( substitute(func) ),
        "' must return a list",
        sep=""
      )
    );
  if (length(chk) != 2)
    return(
      paste(
        "function '",
        deparse( substitute(func) ),
        "' must return a list with 2 elements",
        sep=""
      )
    );
  if (
    !(
      ( is.matrix(chk[[1]]) & is.matrix(chk[[2]]) ) |
      ( is.data.frame(chk[[1]]) & is.data.frame(chk[[2]]) )
    )
  ) return(
      paste(
        "function '",
        deparse( substitute(func) ),
        "' must return a list of matrices or data.frames",
        sep=""
      )
    );
  if ( nrow(chk[[1]]) != nrow(chk[[2]]) )
    return(
      paste(
        "function '",
        deparse( substitute(func) ),
        "' must return a list of matrices or data.frames
        with the same number of rows",
        sep=""
      )
    );
  if ( ncol(chk[[1]]) != ncol(chk[[2]]) )
    return(
      paste(
        "function '",
        deparse( substitute(func) ),
        "' must return a list of matrices or data.frames
        with the same number of columns",
        sep=""
      )
    );
  if ( !all.equal( colnames(chk[[1]]), colnames(chk[[2]]) ) )
    return(
      paste(
        "function '",
        deparse( substitute(func) ),
        "' must return a list of matrices or data.frames
        with the same column names",
        sep=""
      )
    );
  if ( length( unique( colnames(chk[[1]]) ) ) != length( colnames(chk[[1]]) ) )
    return(
      paste(
        "function '",
        deparse( substitute(func) ),
        "' must return a list of matrices or data.frames
        with unique column names",
        sep=""
      )
    );
  if ( is.null( colnames(chk[[1]]) ) )
    return(
      paste(
        "function '",
        deparse( substitute(func) ),
        "' must return a list of matrices or data.frames
        with defined column names",
        sep=""
      )
    );
  return(chk);
} # checkObject.statistic
