
#' Plot Piecewise-Sigmoid Function
#'
#' @description Visualizing the piecewise-sigmoid function used to construct MAR and MNAR missingness patterns in `data_ImputeBench`
#' and `simulation_ImputeBench`.
#'
#' @param probability.upper A numeric between 0 and 1. Default is `0.8`.
#' @param probability.lower A numeric between 0 and 1. Default is `0.2`.
#' @param probability.midpoint A numeric vector with entries between 0 and 1. Default is `c(0.5,0.5)`
#'
#' @details For details on the construction of the piecewise-sigmoid see the accompanying vignette or the paper RMTBM22.
#'
#' @references Robin Richter, Anne Miloschewski, Juliana F. Tavares, Monique M.B. Breteler and Sach Mukherjee, "Benchmarking Single
#' Imputation Methods", in preparation.
#'
#' @return Outputs a plot of the piecewise-sigmoid function.
#'
#' @export
#'

plot_sigmoid = function(probability.upper = 0.8,
                        probability.lower = 0.2,
                        probability.midpoint = c(0.5,0.5)){

  curve = NULL

  p_2 = probability.upper
  p_mid = probability.midpoint[2]
  p_1 = probability.lower

  x_1 = -1
  x_2 = 1
  x_mid = probability.midpoint[1]*(x_2 - x_1) + x_1

  r_1 = (1/(x_1 - x_mid))*(log((1-p_mid) / p_mid) - log( (1 - p_1)/p_1))
  alpha_1 = log((1-p_1)/p_1) + (r_1*x_1)
  r_2 = (1/(x_mid - x_2))*(log((1-p_2) / p_2) - log( (1 - p_mid)/p_mid))
  alpha_2 = log((1-p_mid)/p_mid) + (r_2*x_mid)
  func = function(x){ ifelse(x < x_mid, (1/(1+exp((-r_1*x)+alpha_1))) , (1/(1+exp((-r_2*x)+alpha_2))))}
  curve(func, from = x_1, to = x_2, ylim = c(0,1), ylab = "Probability of being Missing", xlab = "Value")

}









