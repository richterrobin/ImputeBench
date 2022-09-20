

#' Plotting Observed, Missing and Imputed Data Distributions (1D)
#'
#' @description Plotting the distribution of observed, missing and imputed data points for a given data matrix, its missingness
#' pattern and an imputation thereof, in 1 dimensions using ggplot2.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries (ground truth matrix).
#' @param mask A matrix with binary entries (0 = missing, 1 = not missing), the (additional) missingness pattern on which the
#' imputed matrix is compared against the ground truth matrix.
#' @param imputed_data An imputed matrix of `data`.
#' @param title A character string controlling the title of the plot. Default is `NULL`.
#' @param clm An integer, indicating the column to be plotted. Default is `1`.
#' @param additional.imputations A list of imputed matrices to compare different imputation methods. Default is `NULL`.
#' @param imp.names A vector of character strings providing names to the list of imputation methods in the case
#' `additional.imputations` are passed. The first entry corresponds to `imputed_data` while the next entries correspond in their order
#' to the list entries of `additional.imputations`. Default is `NULL`.
#' @param jitter A positive numeric controlling the jitter. Default is `0.4`.
#' @param alpha A positive numeric controlling the transparency of the observed data points. Default is `0.25`.
#'
#' @details Plotting the imputed vs. the missing values in `clm` of `data` including where of interest the observed entries.
#' In the case that `clm` is continuous, the output is a scatter plot of imputed vs. missing values, the line `x=y` is drawn
#' in black. In the case that `clm` is discrete, the output is a tile plot of the imputed vs. the missing values, the diagonal
#' is marked by a red frame around the tiles. In the continuous case via `alpha` the alpha can be changed.
#'
#' @return A ggplot2 plot.
#'
#' @export
#'



plot_1D_imputed_sim = function(data,
                               mask,
                               imputed_data,
                               additional.imputations = NULL,
                               imp.names = NULL,
                               clm = 1,
                               alpha = 1,
                               jitter = 0.4,
                               title = NULL){

  if(length(clm) != 1){
    stop("Length of clm needs to be exactly 1")
  }
  if(!is.null(imp.names)){
    if(length(imp.names) != (length(additional.imputations) + 1)){
      stop("The length of imp.names must be length(additional.imputations) + 1.")
    }
  }

  plot = analyse_missingness_plots(data = data,
                                   clms = clm,
                                   mask = mask,
                                   imputed.data = imputed_data,
                                   error.groups = NULL,
                                   alpha_set = alpha,
                                   multiple.imputations = additional.imputations,
                                   imp.names = imp.names,
                                   first.clms.entry.missing = TRUE,
                                   only.first.mask.missingness = FALSE,
                                   title = title,
                                   jitter = jitter)
  return(plot)
}
