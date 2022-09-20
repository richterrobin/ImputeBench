
#' Baseline (Mean/Median) Imputation Method
#'
#' @param data A numeric matrix with missing entries given by `NA`.
#' @param args A list of arguments consisting of a single entry `method` (possible entries are `"median"` (default) and `"mean"`).
#'
#' @details The function of the baseline imputation method of ImputeBench. Supported are mean and median imputation.
#'
#' @return A numeric matrix with imputed missing entries.
#'
#' @import matrixStats
#'
#' @export
#'


baseline_imputation = function(data,
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
