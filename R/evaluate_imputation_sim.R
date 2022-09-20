
#' Evaluating an Imputation on Simulated Data
#'
#' @description ImputeBench internal function computing the various errors/losses for an imputed matrix. Written for benchmarking
#' imputation methods on simulated data, i.e. distinguishing errors on Gaussian, Poisson, binary, t-distributed, sine-transformed
#' and spline-transformed columns.
#'
#' @param data A numeric matrix (ground truth).
#' @param mask A matrix with binary entries (0 = missing, 1 = not missing), the (additional) missingness pattern on which the
#' imputed matrix is compared against the ground truth matrix.
#' @param imputed_data An imputed matrix of `data`.
#' @param data.grouping A vector indicating how long the simulated data groups are. Expected is a vector with six entries, the first
#' indicates the amount of Gaussian columns, the second the amount of Poisson columns, the third the amount of binary columns,
#' the fourth the amount of t-distributed columns, the fifth the amount of sine-transformed columns and the sixth the amount
#' of spline-transformed columns.
#' @param error_measure Choose one of the error measures from \code{c("l2")} (At the moment only `"l2"` is supported).
#' Default is \code{"l2"}.
#' @param scaling_robust A positive integer, the robustness parameter of the scaling by the standard deviation (to avoid division
#' by 0). Default is `0.01`.
#'
#'
#' @return A list of imputation errors (overall, continuous columns, all six different column types and the log-odds of the binary
#' columns).
#'

evaluate_imputation_sim = function(data,
                                   mask,
                                   imputed_data,
                                   data.grouping,
                                   error_measure = "l2",
                                   scaling_robust = 0.01){

  # We need to first compute the overall error
  overall.error = switch(error_measure,
                         l2 = RMSEscaled_onmask(data = data,
                                                imputed_data = imputed_data,
                                                scaling_type = "StandardDev",
                                                mask = mask,
                                                reduced.output = TRUE,
                                                scaling_robust = scaling_robust))
  # Now we need to group the data, imputation and mask to compute the error for each data type
  start.pois = data.grouping[1] + 1
  start.bin = start.pois + data.grouping[2]
  start.t = start.bin + data.grouping[3]
  start.nonlin = start.t + data.grouping[4]
  start.spline = start.nonlin + data.grouping[5]

  if(data.grouping[1] != 0){
    only.gauss = 1:data.grouping[1]
    data.gauss = data[,only.gauss]
    imputed_data.gauss = imputed_data[,only.gauss]
    mask.gauss = mask[,only.gauss]
    error.gauss = switch(error_measure,
                         l2 = RMSEscaled_onmask(data = data.gauss,
                                         imputed_data = imputed_data.gauss,
                                         scaling_type = "StandardDev",
                                         reduced.output = TRUE,
                                         mask = mask.gauss,
                                         scaling_robust = scaling_robust))
  } else{
    error.gauss = NA
  }
  if(data.grouping[2] != 0){
    only.pois = (data.grouping[1] + 1):(data.grouping[1] + data.grouping[2] )
    data.pois = data[,start.pois:(start.bin - 1)]
    imputed_data.pois = imputed_data[,start.pois:(start.bin - 1)]
    mask.pois = mask[,start.pois:(start.bin - 1)]
    error.pois = switch(error_measure,
                        l2 = RMSEscaled_onmask(data = data.pois,
                                        imputed_data = imputed_data.pois,
                                        scaling_type = "StandardDev",
                                        reduced.output = TRUE,
                                        mask = mask.pois,
                                        scaling_robust = scaling_robust))
  } else{
    error.pois = NA
  }
  if(data.grouping[3] != 0){
    only.bin = (data.grouping[1] + data.grouping[2] + + 1 ):
      (data.grouping[1] + data.grouping[2] +data.grouping[3] )
    data.bin = data[,start.bin:(start.t - 1)]
    imputed_data.bin = imputed_data[,start.bin:(start.t - 1)]
    mask.bin = mask[,start.bin:(start.t - 1)]
    error.binary.mse = switch(error_measure,
                              l2 = RMSEscaled_onmask(data = data.bin,
                                              imputed_data = imputed_data.bin,
                                              scaling_type = "StandardDev",
                                              mask = mask.bin,
                                              reduced.output = TRUE,
                                              scaling_robust = scaling_robust))
    error.binary.logodds =  log_odds(data = data.bin,
                                     imputed_data = imputed_data.bin,
                                     mask = mask.bin)
  } else{
    error.binary.mse = NA
    error.binary.logodds = NA
  }
  if(data.grouping[4] != 0){
    only.t = (data.grouping[1] + data.grouping[2] +data.grouping[3] + 1 ):
      (data.grouping[1] + data.grouping[2] +data.grouping[3] + data.grouping[4])
    data.t = data[,start.t:(start.nonlin - 1)]
    imputed_data.t = imputed_data[,start.t:(start.nonlin - 1)]
    mask.t = mask[,start.t:(start.nonlin - 1)]
    error.t = switch(error_measure,
                     l2 = RMSEscaled_onmask(data = data.t,
                                     imputed_data = imputed_data.t,
                                     scaling_type = "StandardDev",
                                     mask = mask.t,
                                     reduced.output = TRUE,
                                     scaling_robust = scaling_robust))
  } else{
    error.t = NA
  }
  if(data.grouping[5] != 0){
    only.nonlin = (data.grouping[1] + data.grouping[2] +data.grouping[3] + data.grouping[4] + 1 ):
      (data.grouping[1] + data.grouping[2] +data.grouping[3] + data.grouping[4] + data.grouping[5])
    data.nonlin = data[,start.nonlin:(start.nonlin + data.grouping[5] - 1)]
    imputed_data.nonlin = imputed_data[,start.nonlin:(start.nonlin + data.grouping[5] - 1)]
    mask.nonlin = mask[,start.nonlin:(start.nonlin + data.grouping[5] - 1)]
    error.sine = switch(error_measure,
                        l2 = RMSEscaled_onmask(data = data.nonlin,
                                        imputed_data = imputed_data.nonlin,
                                        scaling_type = "StandardDev",
                                        mask = mask.nonlin,
                                        reduced.output = TRUE,
                                        scaling_robust = scaling_robust))
  } else{
    error.sine = NA
  }
  if(data.grouping[6] != 0){
    only.spline = (data.grouping[1] + data.grouping[2] +data.grouping[3] + data.grouping[4] + data.grouping[5] + 1 ):
      (data.grouping[1] + data.grouping[2] +data.grouping[3] + data.grouping[4] + data.grouping[5] + data.grouping[6])
    data.spline = data[,start.spline:(start.spline + data.grouping[6] - 1)]
    imputed_data.spline = imputed_data[,start.spline:(start.spline + data.grouping[6] - 1)]
    mask.spline = mask[,start.spline:(start.spline + data.grouping[6] - 1)]
    error.spline = switch(error_measure,
                          l2 = RMSEscaled_onmask(data = data.spline,
                                          imputed_data = imputed_data.spline,
                                          scaling_type = "StandardDev",
                                          mask = mask.spline,
                                          reduced.output = TRUE,
                                          scaling_robust = scaling_robust))
  } else{
    error.spline = NA
  }
  if((data.grouping[1] + data.grouping[4] + data.grouping[5]) != 0){
    if((data.grouping[4] + data.grouping[5]) != 0){
      if(data.grouping[4] != 0){
        data.continuous = data[,c(1:(start.pois-1),start.t:(start.nonlin + data.grouping[5] - 1))]
      } else{
        data.continuous = data.nonlin
      }
      if(data.grouping[5] == 0){
        data.continuous = data.t
      }
      if((data.grouping[1] != 0) & (data.grouping[4] != 0)){
        if(data.grouping[5] != 0 ){
          mask.continuous = cbind(mask.gauss,
                                  mask.t,
                                  mask.nonlin)
          only.continuous = (data.grouping[1] + data.grouping[2] +data.grouping[3] + 1 ):
            (data.grouping[1] + data.grouping[2] +data.grouping[3] + data.grouping[4] + data.grouping[5])
          imputed_data.continuous = base::cbind(imputed_data.gauss,
                                                imputed_data.t,
                                                imputed_data.nonlin)
        } else{
          imputed_data.continuous = base::cbind(imputed_data.gauss,
                                                imputed_data.t)
          mask.continuous = base::cbind(mask.gauss,
                                        mask.t)
          only.continuous = (data.grouping[1] + data.grouping[2] +data.grouping[3] + 1 ):
            (data.grouping[1] + data.grouping[2] + data.grouping[3] + data.grouping[4])
        }
      } else{
        if(data.grouping[1] != 0){
          imputed_data.continuous = base::cbind(imputed_data.gauss,
                                                imputed_data.nonlin)
          mask.continuous = cbind(mask.gauss,
                                  mask.nonlin)
          only.continuous = (data.grouping[1] + data.grouping[2] + data.grouping[3] + 1 ):
            (data.grouping[1] + data.grouping[2] +data.grouping[3]  + data.grouping[5])
        } else{
          if(data.grouping[4] != 0){
            imputed_data.continuous = base::cbind(imputed_data.t,
                                                  imputed_data.nonlin)
            mask.continuous = cbind(mask.t,
                                    mask.nonlin)
            only.continuous = (data.grouping[1] + data.grouping[2] +data.grouping[3] + 1 ):
              (data.grouping[1] + data.grouping[2] +data.grouping[3] + data.grouping[4] + data.grouping[5])

          } else{
            imputed_data.continuous = imputed_data.nonlin
            mask.continuous = mask.nonlin
            only.continuous = (data.grouping[1] + data.grouping[2] +data.grouping[3] + 1 ):
              (data.grouping[1] + data.grouping[2] +data.grouping[3] + data.grouping[4] + data.grouping[5])
          }
        }
      }
      if(data.grouping[1] != 0){
        only.continuous = c( 1:data.grouping[1], only.continuous)
      }
    } else{
      only.continuous = 1:data.grouping[1]
      data.continuous = data.gauss
      imputed_data.continuous = imputed_data.gauss
      mask.continuous = mask.gauss
    }
    error.continuous = switch(error_measure,
                              l2 = RMSEscaled_onmask(data = data.continuous,
                                              imputed_data = imputed_data.continuous,
                                              scaling_type = "StandardDev",
                                              mask = mask.continuous,
                                              reduced.output = TRUE,
                                              scaling_robust = scaling_robust))
  } else{
    error.continuous = NA
  }

  output = list(overall.error = overall.error,
                error.gauss = error.gauss,
                error.pois = error.pois,
                error.binary.mse = error.binary.mse,
                error.binary.logodds = error.binary.logodds,
                error.t = error.t,
                error.sine = error.sine,
                error.spline = error.spline,
                error.continuous = error.continuous)


  return(output)
}

