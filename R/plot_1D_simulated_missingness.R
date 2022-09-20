

#' Plotting Observed and Missing Data Distributions (1D)
#'
#' @description Plotting the distribution of the observed and missing data entries for a given data matrix and
#' missingness pattern, in 1 dimension using ggplot2.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries (ground truth matrix).
#' @param mask A matrix with binary entries (0 = missing, 1 = not missing), the (additional) missingness pattern on which the
#' imputed matrix is compared against the ground truth matrix.
#' @param title A character string controlling the title of the plot. Default is `NULL`.
#' @param clm An integer, indicating the column to be plotted. Default is `1`.
#'
#' @details Plotting the missingness pattern of `clm` of `data` against its data distribution. All rows with a (real) missing
#' value in `clm` are left out. In the case that `clm` is continuous, the output plot is a box-plot comparing the subdistributions
#' of missing entries and existing entries. In the case that `clm` is discrete, the output plot is a filled bar plot with color
#' coded missingness (Additionally one finds the total number of missing and existing entries for the given bin on the top and
#' bottom of the bar).
#'
#' @return A ggplot2 plot.
#'
#' @export
#'



plot_1D_simulated_missingness = function(data,
                                         mask,
                                         clm = 1,
                                         title = NULL){

  if(length(clm) != 1){
    stop("Length of clm needs to be exactly 1")
  }
  if(sum(mask[,clm]) == nrow(mask) ){
    stop("Column does not observe missing entries")
  }

  plot = analyse_missingness_plots(data = data,
                                   clms = clm,
                                   title = title,
                                   mask = mask,
                                   error.groups = NULL,
                                   first.clms.entry.missing = TRUE,
                                   only.first.mask.missingness = FALSE)

  return(plot)
}

