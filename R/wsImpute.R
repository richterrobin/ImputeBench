
#' Wrapper of Soft Impute for ImputeBench
#'
#' @description Wrapper of soft impute of the `softImpute` package to included it after data standardization (see details)
#' as a default imputation method in ImputeBench.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries.
#' @param args A list with three entries: A positive integer entry called `$rank.max`, a positive numeric called `$lambda`
#' and a character string called `$type`. Default is `list(rank.max = 50, lambda = 2, type = "svd")`.
#'
#' @details Imputation by soft imputed is performaed after standardization of the data by subtracting the column means and division
#' by the column standrad deviation. Post imputation the data is transformed back by multiplication with the pre-imputation column
#' standard deviation and by adding the pre-imputation column mean.
#'
#' @import softImpute
#' @import matrixStats
#'
#' @references R. Mazumder, T. Hastie, R. Tibshirani (2010) Spectral Regularization Algorithms for Learning Large Incomplete
#' Matrices. Journal of Machine Learning Research, 11, 2287-2322.
#'
#' @return A numeric matrix without missing entries
#'
#' @export
#'

wsImpute = function(data,
                    args = list(rank.max = 50,
                                lambda = 2,
                                type = "svd")){
  means = matrixStats::colMeans2(data, na.rm = TRUE)
  sd = matrixStats::colSds(data, na.rm = TRUE)
  data.stdzd = (data - matrix(rep(means, each = nrow(data)), nrow = nrow(data))) /
    matrix(rep(pmax(sd, 0.0001), each = nrow(data)), nrow = nrow(data))
  fits.sI = softImpute::softImpute(data.stdzd ,
                                   rank.max = args$rank.max,
                                   lambda = args$lambda,
                                   type = args$type)
  imputed.matrix = softImpute::complete(data.stdzd, fits.sI)

  imputed.matrix = (matrix(rep(pmax(sd, 0.0001), each = nrow(data)), nrow = nrow(data)) * imputed.matrix) +
    matrix(rep(means, each = nrow(data)), nrow = nrow(data))

  return(imputed.matrix)
}


