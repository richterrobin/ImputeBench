
#' Baseline Imputation Method of ImputeBench
#'
#' @description The baseline imputation method included in ImputeBench is supporting at the moment mean and median imputation.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries.
#' @param args A list with one entry `$method`. Default is `list(method = "median")`.
#'
#' @import matrixStats
#'
#' @return A numeric matrix without missing entries
#'
#' @export
#'


trivial.impute = function(data,
                          args = list(method = "median")){
  p = base::ncol(data)
  imputation = switch(args$method,
                      mean =  matrixStats::colMeans2(base::as.matrix(data),na.rm = TRUE),
                      median = matrixStats::colMedians(base::as.matrix(data), na.rm = TRUE))
  imputed.data = data
  for(k in 1:p){
    imputed.data[(base::is.na(imputed.data)[,k]),k] = imputation[k]
  }
  return(imputed.data)
}
