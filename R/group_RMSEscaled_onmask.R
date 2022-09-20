
#' Grouped and Scaled RMSE for Imputation Error/Loss
#'
#' @description Computing the error/loss of an imputed matrix given its ground truth via a scaled RMSE averaged over multiple column
#' groups.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries (ground truth).
#' @param mask A matrix with binary entries (0 = missing, 1 = not missing), the (additional) missingness pattern on which the
#' imputed matrix is compared against the ground truth matrix.
#' @param imputed_data An imputed numeric matrix of `data`.
#' @param error_grouping A list of column indices establishing the error groups that should be computed separately.
#' @param scaling_type Character string setting the scaling of the data columns, possible values are `"StandardDev"` and `"IQR"`.
#' Default is `"StandardDev"`.
#' @param scaling_robust Positive numeric, robustness parameter avoiding division by close to zero when the columns are scaled by the standard deviation
#' when computing the imputation error. Default is `0.01`.
#'
#' @details For each list entry of `error_grouping` the root mean squared error is compute over all column indices of the respective
#' group where each column is scaled by its respective standard deviation (`scaling_type = "StandardDev`) or its respective
#' IQR (`scaling_type = "IQR"`) - division by zero is avoided via `scaling_robust`. The overall error/loss is computed by averaging the
#' errors/losses over the groups.
#'
#' @return A list with the first entry being the overall error/loss and the second entry being the vector of all group-wise errors/losses.
#'
#' @export
#'


group_RMSEscaled_onmask = function(data,
                                   mask,
                                   imputed_data,
                                   error_grouping,
                                   scaling_type = "StandardDev",
                                   scaling_robust = 0.01){
  data = base::as.matrix(data)
  K = base::length(error_grouping)
  RMSE.vec = 1:K
  # We will need to count if there are groups where no missingness was additionally drawn
  discard = 0
  for(k in 1:K){
    if(is.null(error_grouping[[k]])){
      RMSE.vec[k] = 0
      discard = discard + 1
    } else{
      eval.RMSE = RMSEscaled_onmask(base::as.matrix(data)[,error_grouping[[k]]],
                                    base::as.matrix(imputed_data)[,error_grouping[[k]]],
                                    scaling_type = scaling_type,
                                    scaling_robust = scaling_robust,
                                    mask = base::as.matrix(mask)[,error_grouping[[k]]])
      RMSE.vec[k] = eval.RMSE$RMSE
      missing = eval.RMSE$missing
      if(missing == 0){
        discard = discard + 1
        RMSE.vec[k] = NA
      }
    }
  }
  if(discard < K){
    RMSE = (1/(K - discard))*(base::sum(RMSE.vec,na.rm = TRUE))
  } else{
    RMSE = NA
  }
  output = list(overall = RMSE,
                group_wise = RMSE.vec)
  return(output)
}


