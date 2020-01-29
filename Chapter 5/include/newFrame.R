# IDEAL project
# http://www.ideal.rwth-aachen.de/
#
# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Creates a data.frame.
#
# Parameters:
# modes       vector of 'modes' for the columns.
# nrow        number of rows.
# col.names   vector of column names.
# row.names   vector of row names.
#
# Return value:
#  A data.frame.
#
# Remarks:
#  For internal use.

# R program C.13

newFrame <- function(
  modes,
  nrow,
  col.names,
  row.names=1:nrow
) {
  ret <- vector( mode="list", length=length(modes) );
  for ( i in seq_along(modes) ) {
    ret[[i]] <- vector(mode=modes[i], length=nrow);
  }
  class(ret) <- "data.frame";
  colnames(ret) <- col.names;
  rownames(ret) <- row.names;
  return(ret);
} # newFrame
