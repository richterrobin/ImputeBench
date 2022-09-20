
#' Setting up missForest
#'
#' @description Internal function of ImputeBench setting the missForest parameters.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries.
#'
#' @return A list `args` consisting of one positive integer entry called `$ntree`.
#'
#' @seealso `wmissF`
#'

train_missF = function(data){
  # Hard-coded parameters:
  ntree = 100
  #########################################################################
  args = list(ntree = ntree)
  return(args)
}
