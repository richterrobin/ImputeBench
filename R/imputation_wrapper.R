
#' Imputing the Data by Provided Imputation Methods
#'
#' @description Internal ImputeBench function that calling the imputation functions and outputting a list of imputed matrices
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries.
#' @param methods A list of functions, each entry taking as input a numeric matrix with missing entries via `data` and a list of
#' parameters via `args` and outputting an imputed matrix.
#' @param method.names A vector of character strings providing names for each entry in `methods`.
#' @param method_arguments A list of parameter lists to be passed index-wise to the `args` argument of the corresponding entry in
#' `methods`.
#' @param only.count.columns A vector of indices that correspond to all integer but not binary columns in `data`.
#' @param binary.columns A vector of indices that correspond to all binary columns in `data`.
#' @param max.time The maximal amount of seconds an imputation method is allowed to take. Default is \code{2592000}, i.e. 30 days.
#'
#' @details Imputation methods are employed over a for-loop and after imputation is performed integer columns are rounded and binary
#' columns are rounded and set to 0 if below 0 or 1 if above 1.
#'
#' @import tictoc
#' @import R.utils
#' @import ddpcr
#'
#' @return A list of imputed matrices.
#'


imputation_wrapper = function(data,
                              methods,
                              method.names,
                              method_arguments,
                              only.count.columns = NULL,
                              binary.columns = NULL,
                              max.time = 2592000){
  nbr.methods = base::length(methods)
  # We need empty lists for the results
  imputations = list()
  # We need to compute every imputed data matrix in a new list entry to not take previously computed matrices in their place when
  # their computation fails in try(). We need to use try() so that the whole simulation does not break just because a data set is
  # too large to compute for a certain method in time.
  imputed.data = list()
  computation.time = list()
  for(t in 1:nbr.methods){
    # print(t)
    # t = 4
    tictoc::tic()
    try(
      R.utils::withTimeout(expr = (
        ddpcr::quiet( expr = (imputed.data[[t]] = base::do.call(methods[[t]], args = list(data = data,
                                                                                         args = method_arguments[[t]]))))),
        timeout = max.time, #18000
        onTimeout = "warning", substitute = TRUE),
      silent = TRUE)
    timer = tictoc::toc(quiet = TRUE)
    if(length(imputed.data) < t){
      warning(paste("Imputation Failed for Method ", method.names[t], ". Imputation Errors is set to 100"))
      imputations[[t]] = NULL
      computation.time[[t]] = NULL
    } else{
      # We need to round the count and binary data columns and take the max (and min) with 0 (and 1).
      if(length(only.count.columns) != 0){
        imputed.data[[t]][, only.count.columns] = round(imputed.data[[t]][ , only.count.columns])
      }
      if(length(binary.columns) != 0){
        imputed.data[[t]][, binary.columns] = base::pmax( base::pmin( base::round(imputed.data[[t]][ , binary.columns]), 1) , 0)
      }
      imputations[[t]] = imputed.data[[t]]
      computation.time[[t]] = timer[[2]] - timer[[1]]
    }
  }
  output = list( imputations = imputations,
                 computation.time = computation.time)
  return(output)
}



