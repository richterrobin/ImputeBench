

#' Setting up MICE
#'
#' @description Internal function of ImputeBench setting the MICE parameters.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries.
#'
#' @return A list `args` consisting of the numeric entry between 0 and 1 called `$mincor`, the positive integer entry called
#' `$m` and the positive integer entry called `$maxit`.
#'
#' @seealso `wMICE`
#'

train_MICE = function(data){
  # Hard-coded parameters:
  min.correlation = 0.5
  m = 5
  max.it = 5
  #########################################################################
  args = list(mincor = min.correlation,
              m = m,
              maxit = max.it)
  return(args)
}
