

#' Plotting Observed, Missing and Imputed Data Distributions (3D)
#'
#' @description Plotting the distribution of observed, missing and imputed data points for a given data matrix, its missingness
#' pattern and an imputation thereof, in 3 dimensions using plotly.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries (ground truth matrix).
#' @param mask A matrix with binary entries (0 = missing, 1 = not missing), the (additional) missingness pattern on which the
#' imputed matrix is compared against the ground truth matrix.
#' @param imputed_data An imputed matrix of `data`.
#' @param title A character string controlling the title of the plot. Default is `NULL`.
#' @param clms An integer vector, indicating the columns to be plotted. Default is `c(1,2,3)`.
#' @param jitter A positive numeric controlling the jitter on the discrete values. Default is `0.4`.
#' @param alpha A positive numeric controlling the transparency of the observed data points. Default is `0.25`.
#' @param alpha_line A postive numeric controlling the transparency of the lines connecting missing and imputed values.
#' Default is `1`.
#'
#' @details Plotting the imputed vs. the missing values in `clms` of `data` including where of interest the observed entries.
#' The output plot is a 3D scattered point plot (with additional jitter in the case that one or both variables of `clms` are
#' discrete) where observed points are blue dots and missing points are red dots, while imputed points are orange dots.
#' Each missing point is connected to its imputed value (consider that no distinction is made which of the axis-values are missing, or if
#' multiple are missing). The `alpha` parameter controls the alpha of the complete points. The `jitter` parameter controls
#' the extra jitter.
#'
#' @return A plotly plot.
#'
#' @export
#'

plot_3D_imputed_sim = function(data,
                               mask,
                               imputed_data,
                               alpha = 0.25,
                               alpha_line = 1,
                               jitter = 0.4,
                               clms = c(1,2,3),
                               title = NULL){

  if(length(clms) != 3){
    stop("Length of clms needs to be exactly 3")
  }

  plot = analyse_missingness_plots(data = data,
                                   clms = clms,
                                   mask = mask,
                                   alpha_line = alpha_line,
                                   imputed.data = imputed_data,
                                   error.groups = NULL,
                                   first.clms.entry.missing = TRUE,
                                   only.first.mask.missingness = FALSE,
                                   jitter = jitter,
                                   title = title,
                                   alpha_set = alpha)
  return(plot)
}


