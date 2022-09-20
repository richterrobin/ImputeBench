
#' Plot Missingness Distribution
#'
#' @description Plotting the distribution of missing entries over all variables as a ggplot2 histogram.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries.
#' @param groups A list of column groups. If provided, the output plot is faceted by the groups. Default is `NULL`.
#' @param x_axis A character string, either `"percent"` or `"amount"` controlling the labeling of the x-axis either report
#' relative or total amount of missing entries. Default is `"percent"`.
#' @param title A character string controlling the title of the plot. Default is `NULL`.
#' @param binwidth A positive numeric controlling the binwidth of the histogram. Default is `1/30`.
#'
#' @details Plotting the histogram over the amount of missing entries in the variables. When specifying `groups` the histogram
#' is spread over multiple facets.
#'
#' @return A ggplot2 plot.
#'
#' @export
#'

plot_missingness_over_variables = function(data,
                                           groups = NULL,
                                           x_axis = "percent",
                                           binwidth = NULL,
                                           title = NULL){
  if(x_axis %in% c("percent","amount")){

    plots = analyse_missingness_plots(data = data,
                                      clms = NULL,
                                      mask = NULL,
                                      binwidth = binwidth,
                                      title = title,
                                      error.groups = groups,
                                      missing.label = x_axis,
                                      first.clms.entry.missing = FALSE,
                                      only.first.mask.missingness = FALSE,
                                      thres_name = 0.05,
                                      jitter = 0.4)

    plot = plots$hist

  } else{
    stop("x_axis must be either 'percent' or 'amount'.")
  }

  return(plot)
}



