# R program C.17

statistic.args <- function(
  func,
  args,
  stat,
  dots
) {
  ret <- do.call(
    what=stat,
    args=list(
      do.call(what=func, args=args)
    )
  );
  return(ret);
} # statistic.args
