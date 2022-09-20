
#' Plotting the Correlation between Observed Data and the Missingness Pattern (1D)
#'
#' @description Plotting the distribution of observed entries of a given data matrix with respect to the missingness pattern of an
#' additional column of the same matrix, in 1 dimension using ggplot2.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries (ground truth matrix).
#' @param title A character string controlling the title of the plot. Default is `NULL`.
#' @param miss.clm An integer, indicating the column whose missingness pattern is to be plotted. Default is `1`.
#' @param data.clm An integer, indicating the column whose data distribution is to be plotted. Default is `2`.
#'
#' @details Plotting the missingness pattern of `miss.clm` with respect to the data distribution of `data.clm`.
#' All rows with a missing value in `data.clm` are left out. In the case that `data.clm` is continuous, the output plot is a
#' boxplot comparing the subdistributions of rows with a missing entry in `miss.clm` and those with an existing entry in
#' `miss.clm`. In the case that `data.clm` is discrete, the output plot is a filled bar plot with the missingness in
#' `miss.clm` color coded (Additionally one finds the total number of data rows for each value-missingness-pair on the top
#' and bottom of each bar).
#'
#' @return A ggplot2 plot.
#'
#' @export
#'



plot_1D_real_missingness = function(data,
                                    miss.clm = 1,
                                    data.clm = 2,
                                    title = NULL){

  if(length(miss.clm) != 1){
    stop("Length of miss.clm needs to be exactly 1")
  }
  if(length(data.clm) != 1){
    stop("Length of data.clm needs to be exactly 1")
  }
  if(sum(as.numeric(is.na(data[,miss.clm]))) == 0 ){
    stop("Column does not observe missing entries")
  }

  clms = c(miss.clm, data.clm)

  plot = analyse_missingness_plots(data = data,
                                   clms = clms,
                                   mask = NULL,
                                   title = title,
                                   error.groups = NULL,
                                   first.clms.entry.missing = TRUE,
                                   only.first.mask.missingness = FALSE)

  return(plot)
}

