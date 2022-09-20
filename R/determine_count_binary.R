

#' Detect Integer and Binary Data Columns
#'
#' @description Find all columns that consist of only integer values and of those all columns that consist of only entries in 0 and 1.
#'
#' @param data A matrix with numerical entries.
#'
#' @return A list of two vectors, the first of which contains the column indices of all integer columns and the second of which contains
#' the column indices of all binary columns.
#'


determine_count_binary = function(data){

  p = base::ncol(data)
  n = base::nrow(data)

  counts = ((data == round(data)) | is.na(data))
  binaries = ((data %in% c(0,1)) | is.na(data))

  count  = base::which(base::colSums(base::matrix(counts, ncol = p, nrow = n), na.rm = TRUE) == n)
  binary = base::which(base::colSums(base::matrix(binaries, ncol = p, nrow = n), na.rm = TRUE) == n)

  output = list(count.columns = count,
                binary.columns = binary)
  return(output)
}
