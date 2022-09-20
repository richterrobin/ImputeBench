
#' Adding Missing Entries to a Matrix
#'
#' @param data Matrix with numeric entries that is to be masked.
#' @param mask Binary matrix indicating where missing entries should be imposed (0 = missing, 1 = not missing)
#'
#'
#' @return A numeric matrix with missing entries given by `NA`
#'
#' @export
#'


add_missing = function(data, mask){
  data.vec = c(data)
  miss.vec =  c(mask)
  data.vec[which(miss.vec == 0)] = NA
  data = matrix(data.vec, ncol = ncol(data),nrow = nrow(data))
  return(data)
}
