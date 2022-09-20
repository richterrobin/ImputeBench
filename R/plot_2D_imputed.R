

#' Plotting Observed and Imputed Data Distributions (2D)
#'
#' @description Plotting the distribution of observed and imputed data entries for a given data matrix with missing entries
#' and an imputation thereof, in 2 dimensions using ggplot2.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries (ground truth matrix).
#' @param imputed_data An imputed matrix of `data`.
#' @param title A character string controlling the title of the plot. Default is `NULL`.
#' @param clms An integer vector, indicating the columns to be plotted. Default is `c(1,2)`.
#' @param jitter A positive numeric controlling the jitter on the discrete values. Default is `0.4`.
#' @param alpha A positive numeric controlling the transparency of the observed data points. Default is `0.25`.
#'
#' @details Plotting the imputed values in `clms` of `data` against the observed ones in the same columns.
#' The output plot is a 2D scattered point plot (with additional jitter in the case that one or both variables of `clms` are
#' discrete) with color coded which entries have been imputed. The `alpha` parameter controls the alpha of the complete points.
#' The `jitter` parameter controls the extra jitter.
#'
#' @return A ggplot2 plot.
#'
#' @export
#'


plot_2D_imputed = function(data,
                           imputed_data,
                           alpha = 0.25,
                           jitter = 0.4,
                           clms = c(1,2),
                           title = NULL){

  if(length(clms) != 2){
    stop("Length of clms needs to be exactly 2")
  }

  plot = analyse_missingness_plots(data = data,
                                   clms = clms,
                                   mask = NULL,
                                   imputed.data = imputed_data,
                                   error.groups = NULL,
                                   first.clms.entry.missing = TRUE,
                                   only.first.mask.missingness = FALSE,
                                   title = title,
                                   jitter = jitter,
                                   alpha_set = alpha)
  return(plot)
}

