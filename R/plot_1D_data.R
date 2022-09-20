


#' Plotting Data Distribution (1D)
#'
#' @description Plotting the distribution of given data in 1 dimensions using ggplot2.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries.
#' @param title A character string controlling the title of the plot. Default is `NULL`.
#' @param clm An integer, indicating the column to be plotted. Default is `1`.
#' @param binwidth (1D) A positive numeric controlling the binwidth. Default is `NULL`.
#'
#' @details Plotting the data distribution of selected columns in `clm` of `data` as a histogram with customizable `binwidth`.
#'
#' @return A ggplot2 plot.
#'
#' @export
#'


plot_1D_data = function(data,
                        clm = 1,
                        title = NULL,
                        binwidth = NULL){

  if(length(clm) != 1){
    stop("Length of clm needs to be exactly 1")
  }

  plot = analyse_missingness_plots(data = data,
                                   clms = c(clm),
                                   title = title,
                                   binwidth = binwidth,
                                   mask = NULL,
                                   error.groups = NULL,
                                   first.clms.entry.missing = FALSE,
                                   only.first.mask.missingness = FALSE,
                                   jitter = 1)

  return(plot)
}
