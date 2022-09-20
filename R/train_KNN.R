
#' Training k-nn Impute via Bisection
#'
#' @description Training of the parameter `k` of the k-nn imputation exported from the `VIM` package by a bisection algorithm
#' (called within the wrapper function `wKNN`).
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries.
#'
#' @return A list `args` consisting of one positive integer entry named `$k`.
#'
#' @import R.utils
#' @import ddpcr
#'
#' @seealso `wKNN`, `RMSEscaled_onmask`, `simulate_mask`
#'
#' @export
#'

train_KNN = function(data){

  # Hard-coded parameters:
  maximal.knn = base::min(101, base::ncol(data))
  repetitions = 5
  bisection.partition = 10
  bisection.depth = 2
  default.K = 5
  max.time = 2592000
  #########################################################################
  cblist = determine_count_binary(data)
  only.count.columns = base::setdiff(cblist$count.columns, cblist$binary.columns)
  binary.columns = cblist$binary.columns
  each.trained.k = rep(c(100), repetitions)
  starting.values = c(1, min(base::ncol(data), maximal.knn))

  for(t in 1:repetitions){
    missingness.scenario = missingness_scenario_from_parameters(nbr_columns = ncol(data),
                                                                missingness_parameters = list("MCAR" = list(columns = 1:ncol(data),
                                                                                                            size = floor(ncol(data)/2),
                                                                                                            probability = 0.2)))
    new.mask = simulate_mask(data = data,
                             scenario = missingness.scenario)
    masked.data = data
    for(clmn in 1:base::ncol(data)){
      masked.data[base::which(new.mask[,clmn] == 0),clmn] = NA
    }
    for(dep in 1:bisection.depth){
      evaluation.points = base::round(seq(from = starting.values[1],
                                          to = starting.values[2],
                                          length.out = bisection.partition))
      bisection = base::rep(c(100), times = bisection.partition)
      ccc = 1
      for(m in evaluation.points){
        par = list(k = m)
        try(
          R.utils::withTimeout(expr = (
            ddpcr::quiet( expr = (imputed_data = wKNN(data = masked.data,
                                                      args = par ) ))),
            timeout = max.time, #18000
            onTimeout = "warning", substitute = TRUE),
          silent = TRUE)
        if(base::exists("imputed_data")){
          if(!(base::is.null(imputed_data))){
            if(length(only.count.columns) != 0){
              imputed_data[, only.count.columns] = base::pmax(round(imputed_data[ , only.count.columns]), 0 )
            }
            if(length(binary.columns) != 0){
              imputed_data[, binary.columns] = base::pmax( base::pmin( base::round(imputed_data[ , binary.columns]), 1) , 0)
            }
            bisection[ccc]  = RMSEscaled_onmask(data = data,
                                                imputed_data = imputed_data,
                                                scaling_type = "StandardDev",
                                                mask = new.mask,
                                                reduced.output = TRUE)
          }
        }
        ccc = ccc + 1
      }
      middle = base::which(bisection == base::min(bisection))
      # If middle is a vector, we take the first
      middle = middle[1]
      # NOw we need to update the starting values
      starting.values[1] = evaluation.points[base::max(middle-1,1)]
      starting.values[2] = evaluation.points[base::min(middle+1,bisection.partition)]
    }
    if(base::min(bisection) == 100){
      trained.k = default.K
      warning("KNN was not trained properly, due to error or time out.")
    } else{
      each.trained.k[t] = evaluation.points[middle]
    }
  }
  trained.k = median(each.trained.k)
  args = list(k = trained.k)
  return(args)
}

