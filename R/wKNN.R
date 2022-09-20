

#' Wrapper of k-nn Impute for ImputeBench
#'
#' @description Wrapper of k-nn impute of the `VIM` package to included it as a default imputation method in ImputeBench.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries.
#' @param args A list with one integer entry `$k`. Default is `list(k = 5)`.
#'
#' @import VIM
#'
#' @references A. Kowarik, M. Templ (2016) Imputation with R package VIM. Journal of Statistical Software, 74(7), 1-16.
#'
#' @seealso `train_KNN`
#'
#' @return A numeric matrix without missing entries
#'
#' @export
#'



wKNN = function(data,
                args = list(k = 5)){
  p = base::ncol(data)
  k.0 = args$k
  data = base::as.data.frame(data)
  imputed.matrix = VIM::kNN(data,
                            k = k.0)
  imputed.matrix = imputed.matrix[,1:p]
  imputed.matrix = base::as.matrix(imputed.matrix)
  return(imputed.matrix)
}
