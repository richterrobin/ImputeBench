

#' Computing the Log-Odds of an Imputed Matrix
#'
#' @description Computing the log-odds error/loss for imputed binary matrices.
#'
#' @param data A matrix with binary entries or `NA` entries marking missing entries.
#' @param imputed_data A binary imputed matrix of `data`.
#' @param mask A matrix with binary entries (0 = missing, 1 = not missing), the (additional) missingness pattern on which the
#' imputed matrix is compared against the ground truth matrix.
#'
#' @details Hard coded is an `epsilon = 1e-15` to avoid division by 0.
#'
#' @return The log-odds error/loss of the imputed matrix.
#'
#' @export
#'

log_odds = function(data,
                    imputed_data,
                    mask){

  if(base::min(c(mask)) == 0){
    GP = length(which((data == imputed_data & data == 1) & mask == 0))
    GN = length(which((data == imputed_data & data == 0) & mask == 0))
    FN = length(which((data != imputed_data & data == 1) & mask == 0))
    FP = length(which((data != imputed_data & data == 0) & mask == 0))
    GPR = GP/(length(which(data == 1 & mask == 0)))
    GNR = GN/(length(which(data == 0 & mask == 0)))
    FNR = FN/(length(which(data == 1 & mask == 0)))
    FPR = FP/(length(which(data == 0 & mask == 0)))
    epsilon = 1e-15
    log.odds = log((GPR*GNR)/(FPR*FNR + epsilon))
  } else{
    log.odds = NA
  }
  return(log.odds)
}
