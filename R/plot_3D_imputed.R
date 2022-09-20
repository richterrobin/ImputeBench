

#' Plotting Observed and Imputed Data Distributions (3D)
#'
#' @description Plotting the distribution of observed and imputed data entries for a given data matrix with missing entries
#' and an imputation thereof, in 3 dimensions using plotly.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries (ground truth matrix).
#' @param imputed_data An imputed matrix of `data`.
#' @param title A character string controlling the title of the plot. Default is `NULL`.
#' @param clms An integer vector, indicating the columns to be plotted. Default is `c(1,2,3)`.
#' @param jitter A positive numeric controlling the jitter on the discrete values. Default is `0.4`.
#' @param alpha A positive numeric controlling the transparency of the observed data points. Default is `0.25`.
#'
#' @details Plotting the imputed values in `clms` of `data` against the observed ones in the same columns.
#' The output plot is a 3D scattered point plot (with additional jitter in the case that one or both variables of `clms` are
#' discrete) with color coded which entries have been imputed. The `alpha` parameter controls the alpha of the complete points.
#' The `jitter` parameter controls the extra jitter.
#'
#' @return A plotly plot.
#'
#' @export
#'


plot_3D_imputed = function(data,
                           imputed_data,
                           alpha = 0.25,
                           jitter = 0.4,
                           clms = c(1,2,3),
                           title = NULL){

  if(length(clms) != 3){
    stop("Length of clms needs to be exactly 3")
  }

  plot = analyse_missingness_plots(data = data,
                                   clms = clms,
                                   mask = NULL,
                                   imputed.data = imputed_data,
                                   error.groups = NULL,
                                   title = title,
                                   first.clms.entry.missing = TRUE,
                                   only.first.mask.missingness = FALSE,
                                   jitter = jitter,
                                   alpha_set = alpha)
  return(plot)
}
