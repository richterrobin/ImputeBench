
#' Scaled RMSE for Imputation Error/Loss
#'
#' @description Computing the error/loss of an imputed matrix given its ground truth via a scaled RMSE.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries (ground truth).
#' @param mask A matrix with binary entries (0 = missing, 1 = not missing), the (additional) missingness pattern on which the
#' imputed matrix is compared against the ground truth matrix.
#' @param imputed_data An imputed numeric matrix of `data`.
#' @param scaling_type Character string setting the scaling of the data columns, possible values are `"StandardDev"` and `"IQR"`.
#' Default is `"StandardDev"`.
#' @param scaling_robust Positive numeric, robustness parameter avoiding division by close to zero when the columns are scaled by the standard deviation
#' when computing the imputation error. Default is `0.01`.
#' @param reduced.output Boolean, controlling whether the number of missing entries should be outputted as well. Default is `FALSE`.
#'
#' @details The root mean squared error is compute over all missing entries according to `mask` that are not `NA` in `data` where each
#' column is scaled by its respective standard deviation (`scaling_type = "StandardDev`) or its respective IQR (`scaling_type = "IQR"`)
#' - division by zero is avoided via `scaling_robust`.
#'
#' @return A list with the first entry being the scaled RMSE and the second entry the number of imputed matrix entries
#' (`reduced.output = FALSE`) or the RMSE (`reduced.output = TRUE`).
#'
#' @export
#'



RMSEscaled_onmask = function(data,
                             imputed_data,
                             scaling_type = "StandardDev",
                             mask,
                             reduced.output = FALSE,
                             scaling_robust = 0.01){

  data = as.matrix(data)
  data.vec = c(data)
  real.miss = base::which(base::is.na(data.vec))
  simulated.miss = base::which(mask == 0)
  additional.missing.entries = setdiff(simulated.miss, real.miss)
  missing = base::length(additional.missing.entries)
  mask.vec = c(mask)
  # We need to scale by either the standard deviation or the IQR
  scaling = switch(scaling_type,
                   StandardDev = matrixStats::colSds(base::as.matrix(data), na.rm = TRUE),
                   IQR = matrixStats::colIQRs(base::as.matrix(data), na.rm = TRUE))
  Scaling = switch(scaling_type,
                   StandardDev = base::matrix(base::rep(base::pmax(scaling,scaling_robust),
                                                        each = base::nrow(data)),
                                              nrow = base::nrow(base::as.matrix(data)),
                                              ncol = base::ncol(base::as.matrix(data))),
                   IQR = base::matrix(base::rep(base::pmax(scaling,scaling_robust),
                                                each = base::nrow(data)),
                                      nrow = base::nrow(base::as.matrix(data)),
                                      ncol = base::ncol(base::as.matrix(data))))
  if(missing != 0){
    imputed_data.vec = c(base::as.matrix(imputed_data))
    Scaling.vec = c(Scaling)
    RMSE = sqrt((base::sum( ((data.vec[additional.missing.entries] - imputed_data.vec[additional.missing.entries])/
                               Scaling.vec[additional.missing.entries])^2))/missing)
  } else {
    RMSE = 0
  }
  if(reduced.output){
    output = RMSE
  } else{
    output = list(RMSE = RMSE,
                  missing = missing)
  }
  return(output)
}
