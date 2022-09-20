

#' Plotting Observed and Imputed Data Distributions (1D)
#'
#' @description Plotting the distribution of observed and imputed data entries for a given data matrix with missing entries
#' and an imputation thereof, in 1 dimension using ggplot2.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries (ground truth matrix).
#' @param imputed_data An imputed matrix of `data`.
#' @param title A character string controlling the title of the plot. Default is `NULL`.
#' @param clm (1D) An integer, indicating the column to be plotted. Default is `1`.
#' @param binwidth (1D) A positive numeric controlling the binwidth. Default is `NULL`.
#'
#' @details Plotting the imputed values in `clm` of `data` against the observed ones in the same column.  In the case that `clm`
#' is continuous, the output is a histogram plot with the imputed values displayed as transparent red bars and the observed
#' values in filled grey bars. In the case that `clm` is discrete, the output is a bar plot color coded for imputed
#' and observed entries. In the continuous case via `binwidth` the width of the bins can be changed.
#'
#' @return A ggplot2 plot.
#'
#' @export
#'



plot_1D_imputed = function(data,
                           imputed_data,
                           clm = 1,
                           binwidth = NULL,
                           title = NULL){

  if(length(clm) != 1){
    stop("Length of clm needs to be exactly 1")
  }

  plot = analyse_missingness_plots(data = data,
                                   clms = clm,
                                   mask = NULL,
                                   imputed.data = imputed_data,
                                   error.groups = NULL,
                                   title = title,
                                   binwidth = binwidth,
                                   first.clms.entry.missing = TRUE,
                                   only.first.mask.missingness = FALSE)

  return(plot)
}

