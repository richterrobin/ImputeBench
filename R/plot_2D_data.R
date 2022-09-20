

#' Plotting Data Distribution (2D)
#'
#' @description Plotting the distribution of given data in 2 dimensions using ggplot2.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries.
#' @param title A character string controlling the title of the plot. Default is `NULL`.
#' @param clms An integer vector, indicating the columns to be plotted. Default is `c(1,2)`.
#' @param type A character string. In the case two discrete variables are selected via `clms`, choose between `"scatter"` and
#' `"tile"`. Default is `"scatter"`.
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
#' ### `type = "tile"`
#'
#' Only possible if both variables are discrete: The result is a tile plot with labeled tiles.
#'
#' @return A ggplot2 plot.
#'
#' @export
#'


plot_2D_data = function(data,
                        clms = c(1,2),
                        alpha = 0.25,
                        jitter = 0.4,
                        type = "scatter", # "tile"
                        title = NULL){
  if(length(clms) != 2){
    stop("Length of clms needs to be exactly 2")
  }
  if(!(type %in% c("scatter","tile"))){
    stop("type must be one of 'scatter' or 'tile'.")
  }

  plots = analyse_missingness_plots(data = data,
                                    clms = clms,
                                    mask = NULL,
                                    error.groups = NULL,
                                    title = title,
                                    first.clms.entry.missing = FALSE,
                                    only.first.mask.missingness = FALSE,
                                    jitter = jitter,
                                    alpha_set = alpha)

  if(identical(type, "scatter")){
    if("scatter" %in% names(plots)){
      plot = plots$scatter
    } else{
      plot = plots
    }
  } else if(identical(type, "tile")){
    if("tile" %in% names(plots)){
      plot = plots$tile
    } else{
      stop("Type tile can only be used for two discrete variables")
    }
  }

  return(plot)
}

