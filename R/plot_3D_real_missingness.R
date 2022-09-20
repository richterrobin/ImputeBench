
#' Plotting the Correlation between Observed Data and the Missingness Pattern (3D)
#'
#' @description Plotting the distribution of observed entries of a given data matrix with respect to the missingness pattern of an
#' additional column of the same matrix, in 3 dimensions using ggplot2 and plotly.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries (ground truth matrix).
#' @param title A character string controlling the title of the plot. Default is `NULL`.
#' @param miss.clm An integer, indicating the column whose missingness pattern is to be plotted. Default is `1`.
#' @param data.clms An integer vector, indicating the columns whose data distribtuion is to be plotted.
#' Default is `c(2,3,4)`.
#' @param type A character string. In the case the third variable is discrete, choose between `"scatter"` and `"facets"`.
#' Default is `"scatter"`.
#' @param jitter A positive numeric controlling the jitter on the discrete values. Default is `0.4`.
#' @param alpha A positive numeric controlling the transparency of the observed data points. Default is `0.25`.
#'
#' @details Plotting the missingness pattern of `miss.clm` with respect to the data distribution of `data.clms`.
#' All rows with a missing value in `data.clms` are left out.
#'
#' ### `type = "scatter"`
#' The output plot is a 3D scatter point plot (with additional jitter in the case that one, two or all variables of `data.clms` are
#' discrete) with color coded missingness in `miss.clm`. The `alpha` parameter controls the alpha of the complete points.
#'  The `jitter` parameter controls the extra jitter.
#'
#' ### `type = "facets"`
#' The output plot is a 2D scatter point plot faceted by the third variable of `data.clms` and color coded by the missingness
#' pattern of `miss.clm`. Only available if the third column of `data.clms` is a discrete variable. The `alpha` parameter controls the
#' alpha of the complete points. The `jitter` parameter controls the extra jitter.
#'
#' @return A ggplot2 plot (`type = facets`) or a plotly plot (`type = "scatter"`).
#'
#' @export
#'


plot_3D_real_missingness = function(data,
                                    miss.clm = 1,
                                    data.clms = c(2,3,4),
                                    type = "scatter",# "facet"
                                    alpha = 0.25,
                                    jitter = 0.4,
                                    title = NULL){

  if(length(miss.clm) != 1){
    stop("Length of miss.clm needs to be exactly 1")
  }
  if(length(data.clms) != 3){
    stop("Length of data.clm needs to be exactly 3")
  }
  if(sum(as.numeric(is.na(data[,miss.clm]))) == 0 ){
    stop("Column does not observe missing entries")
  }

  clms = c(miss.clm, data.clms)

  plots = analyse_missingness_plots(data = data,
                                    clms = clms,
                                    mask = NULL,
                                    title = title,
                                    error.groups = NULL,
                                    alpha_set = alpha,
                                    first.clms.entry.missing = TRUE,
                                    only.first.mask.missingness = FALSE,
                                    jitter = jitter)

  if(identical(type, "scatter")){
    if("scatter" %in% names(plots)){
      plot = plots$scatter
    } else{
      plot = plots
    }
  } else if(identical(type, "facets")){
    if("facets" %in% names(plots)){
      plot = plots$facets
    } else{
      stop("Type facets can only be used if the third data variables is discrete")
    }
  } else{
    stop("No eligible type.")
  }

  return(plot)
}
