
#' Wrapper of MICE for ImputeBench
#'
#' @description Wrapper of the MICE imputation function of the `mice` package to included it as a default imputation method
#' in ImputeBench.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries.
#' @param args A list with three entries: A numeric between 0 and 1 called `$mincor`, a positive integer called `$m`
#' and a positive integer called `$maxit`. Default is `list(mincor = NULL, m = 5, maxit = 5)`.
#'
#' @import mice
#'
#' @references S. van Buuren, K. Groothuis-Oudshoorn (2011) `mice`: Multivariate Imputation by Chained Equations.
#' Journal of Statistical Software, 45(3), 1-67.
#'
#' @return A numeric matrix without missing entries
#'

wMICE = function(data,
                 args = list(mincor = NULL,
                             m = 5,
                             maxit = 5)){
  if(!base::is.null(args)){
    mincor = args$mincor
    m.0 = args$m
    maxit = args$maxit
    if(!is.null(mincor)){
      fits.mice = mice::mice(data,
                             m = m.0,
                             predictorMatrix = mice::quickpred(data, mincor = mincor),
                             maxit = maxit)
    } else{
      fits.mice = mice::mice(data,
                             m = m.0,
                             maxit = maxit)
    }
    imputed.data = mice::complete(fits.mice)
  } else{
    fits.mice = mice::mice(data)
    imputed.data = mice::complete(fits.mice)
  }
  return(imputed.data)
}


