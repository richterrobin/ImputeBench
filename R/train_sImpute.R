
#' Training Soft Impute via Bisection
#'
#' @description Training of the parameter `lambda` of soft impute of the `softImpute` package by a bisection algorithm
#' (called within the wrapper function `wsImpute`). Moreover, setting of the remaining parameters.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries.
#'
#' @return A list `args` consisting of the positive numeric entry named `$lambda`, the positive integer entry `$rank.max` and
#' the character string entry `$type`.
#'
#' @import R.utils
#' @import ddpcr
#'
#' @seealso `wsImpute`, `RMSEscaled_onmask`, `simulate_mask`
#'
#' @export
#'

train_sImpute = function(data){

  # Hard-coded parameters:

  maximal.lambda = 100*(base::min(base::ncol(data),base::nrow(data)))
  minimal.lambda = 0.001*(base::min(base::ncol(data),base::nrow(data)))
  repetitions = 10
  bisection.partition = 10
  bisection.depth = 2
  default.lambda = 5 # The actual default lambda is 0 but that seems not appropriate

  rank.max = base::min(base::ncol(data),base::nrow(data)) - 1 # The actual default is 2, also this seems to be not appropriate.
  type = "svd" # Our chosen default
  max.time = 2592000
  #########################################################################
  # Finding the count and binary columns
  cblist = determine_count_binary(data)
  only.count.columns = base::setdiff(cblist$count.columns, cblist$binary.columns)
  binary.columns = cblist$binary.columns
  # Preparing the lambda vector
  each.trained.lambda = base::rep(c(100), repetitions)
  starting.values = c(minimal.lambda, maximal.lambda)
  # We need to initialize an MCAR base scenario
  missingness.scenario = missingness_scenario_from_parameters(nbr_columns = ncol(data),
                                                              missingness_parameters = list("MCAR" = list(columns = 1:ncol(data),
                                                                                                          size = floor(ncol(data)/2),
                                                                                                          probability = 0.2)))
  for(t in 1:repetitions){
    # t = 1
    # We need to simulate an additional mask
    new.mask = simulate_mask(data = data,
                             scenario = missingness.scenario)
    # And impose this mask on the data
    masked.data = data
    for(clmn in 1:base::ncol(data)){
      masked.data[base::which(new.mask[,clmn] == 0),clmn] = NA
    }
    for(dep in 1:bisection.depth){
      # dep = 1
      log.evaluation.points = seq(from = log(starting.values[1]),
                                  to = log(starting.values[2]),
                                  length.out = bisection.partition)
      evaluation.points = exp(log.evaluation.points)
      bisection = base::rep(c(100), times = bisection.partition)
      counter = 1
      # rm("imputed_data")
      for(m in evaluation.points){
        # m = 1
        par = list(lambda = m,
                   rank.max = rank.max,
                   type = type)
        # Recommended: Using the try() function together with a time.out as to not get stuck here.
        try(
          R.utils::withTimeout(expr = (
            ddpcr::quiet( expr = (imputed_data = wsImpute(data = masked.data,
                                                          args = par ) ))),
            timeout = max.time, #18000
            onTimeout = "warning", substitute = TRUE),
          silent = TRUE)
        # If the imputation worked we need to adjust count and binary columns:
        if(base::exists("imputed_data")){
          if(!(base::is.null(imputed_data))){
            if(length(only.count.columns) != 0){
              imputed_data[, only.count.columns] = base::pmax(round(imputed_data[ , only.count.columns]), 0 )
            }
            if(length(binary.columns) != 0){
              imputed_data[, binary.columns] = base::pmax( base::pmin( base::round(imputed_data[ , binary.columns]), 1) , 0)
            }
            # And we need to compute the error:
            bisection[counter]  = RMSEscaled_onmask(data = data,
                                                    imputed_data = imputed_data,
                                                    scaling_type = "StandardDev",
                                                    mask = new.mask,
                                                    reduced.output = TRUE)
          } # Note that if the if-clause is not satisfied, i.e. imputation has not worked the error will remain at 100 as initialized.
        }
        counter = counter + 1
      }
      # bisection
      middle = base::which(bisection == base::min(bisection))
      # If middle is a vector, we take the first
      middle = middle[1]
      # Now we need to update the starting values
      starting.values[1] = evaluation.points[base::max(middle-1,1)]
      starting.values[2] = evaluation.points[base::min(middle+1,bisection.partition)]
    }
    each.trained.lambda[t] = evaluation.points[middle]
  }
  # evaluation.points
  # If training never worked:
  if(base::min(bisection) == 100){
    trained.lambda = default.lambda
    warning("soft Impute was not trained properly, due to error or time out.")
  } else{
    # Otherwise:
    trained.lambda = median(each.trained.lambda)
  }
  args = list(lambda = trained.lambda,
              rank.max = rank.max,
              type = type)
  return(args)
}


