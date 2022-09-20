
#' Extracting the Amount of Missing Entries
#'
#' @param data Matrix with numeric entries and missing entries given as `NA`.
#'
#' @description Internal function of ImputeBench.
#'
#' @return A vector with the column-wise fraction of missing entries.
#'


amount_missing = function(data){
  n = base::nrow(data)
  p = base::ncol(data)
  miss.pattern = base::matrix(as.numeric(is.na(data)),
                              ncol = ncol(data),
                              nrow = nrow(data))
  nbr.missing = base::colSums(miss.pattern)
  amount.missing = nbr.missing / base::nrow(miss.pattern)
  return(amount.missing)
}










