

#' Plotting Data Distribution (3D)
#'
#' @description Plotting the distribution of given data in 3 dimensions using ggplot2 and plotly.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries.
#' @param title A character string controlling the title of the plot. Default is `NULL`.
#' @param clms An integer vector, indicating the columns to be plotted. Default is `c(1,2,3)`.
#' @param type A character string. In the case the third variable is discrete, choose between `"scatter"` and
#' `"facets"`. Default is `"scatter"`.
#' @param jitter A positive numeric controlling the jitter on the discrete values. Default is `0.4`.
#' @param alpha A positive numeric controlling the transparency of the points. Default is `0.25`.
#'
#' @details Plotting the data distribution of selected columns in `clms` of `data`.
#'
#' ### `type = "scatter"`
#'
#' The result is a scatter plot with jitter added to discrete variables (customizable via `jitter`) with customizable transparency
#' of the points via `alpha`.
#'
#' ### `type = "facets"`
#'
#' Only possible if the third column of `clms` is discrete. The result is a scatter plot over the first two entries of `clms` faceted
#' over the third entry of `clms`. As in the 2D plot jitter is added to discrete variables of the scatter plots and can be controlled
#' via `jitter` and the transparency of the points can be controlled via `alpha`.
#'
#' @return A ggplot2 plot (`type = facets`) or a plotly plot (`type = "scatter"`).
#'
#' @export
#'

plot_3D_data = function(data,
                        clms = c(1,2,3),
                        alpha = 0.25,
                        jitter = 0.4,
                        type = "scatter", # "facets"
                        title = NULL){

  if(length(clms) != 3){
    stop("Length of clms needs to be exactly 3")
  }
  if(!(type %in% c("scatter","facets"))){
    stop("type must be one of 'scatter' or 'facets'.")
  }

  plots = analyse_missingness_plots(data = data,
                                   clms = clms,
                                   mask = NULL,
                                   error.groups = NULL,
                                   first.clms.entry.missing = FALSE,
                                   only.first.mask.missingness = FALSE,
                                   jitter = jitter,
                                   title = title,
                                   alpha_set = alpha)

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
      stop("Type facets can only be used if the third variable is discrete")
    }
  }

  return(plot)
}
