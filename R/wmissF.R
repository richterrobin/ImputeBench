
#' Wrapper of missForest for ImputeBench
#'
#' @description Wrapper of the missForest imputation function of the `missForest` package to included it as a default imputation
#' method in ImputeBench.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries.
#' @param args A list with one positive integer entry called `$ntree`. Default is `list(ntree = 100)`.
#'
#' @import missForest
#'
#' @references D.J. Stekhoven, P. Buehlmann (2012) `MissForest` - nonparametric missing value imputation for mixed-type data.
#' Bioinformatics, 28(1), 112-118.
#'
#' @return A numeric matrix without missing entries
#'

wmissF = function(data,
                  args = list(ntree = 100)){
  if(!base::is.null(args)){
    ntree.0 = args$ntree
    imp.list = missForest::missForest(data,
                                      ntree = ntree.0)
  } else{
    imp.list = missForest::missForest(data)
  }
  imputed.data = imp.list[[1]]
  return(imputed.data)
}
