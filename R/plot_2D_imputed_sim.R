

#' Plotting Observed, Missing and Imputed Data Distributions (2D)
#'
#' @description Plotting the distribution of observed, missing and imputed data points for a given data matrix, its missingness
#' pattern and an imputation thereof, in 2 dimensions using ggplot2.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries (ground truth matrix).
#' @param mask A matrix with binary entries (0 = missing, 1 = not missing), the (additional) missingness pattern on which the
#' imputed matrix is compared against the ground truth matrix.
#' @param imputed_data An imputed matrix of `data`.
#' @param title A character string controlling the title of the plot. Default is `NULL`.
#' @param clms An integer vector, indicating the columns to be plotted. Default is `c(1,2)`.
#' @param type A character string. In the case two discrete variables are selected via `clms`, choose between `"scatter"` and
#' `"tile"`. Default is `"scatter"`.
#' @param jitter A positive numeric controlling the jitter on the discrete values. Default is `0.4`.
#' @param alpha A positive numeric controlling the transparency of the observed data points. Default is `0.25`.
#' @param alpha_line A postive numeric controlling the transparency of the lines connecting missing and imputed values.
#' Default is `1`.
#'
#' @details Plotting the imputed vs. the missing values in `clms` of `data` including where of interest the observed entries.
#'
#' ### `type = "scatter"`
#'
#' The output plot is a 2D scattered point plot (with additional jitter in the case that one or both variables of `clms` are
#' discrete) where observed points are blue dots and missing points are red dots, while imputed points are shown as red crosses.
#' Each missing point is connected to its imputed value (consider that no distinction is made if only the x-value, only the y-value or
#' both values are missing). The `alpha` parameter controls the alpha of the complete points. The `jitter` parameter controls
#' the extra jitter.
#'
#' ### `type = "tile"`
#' The output is a tile plot in which for two discrete variables the distribution over the discrete bins is shown for the latent
#' data (in black at the bottom of each tile) and of the imputed data (in red on the top of each tile). Color coded is the relative
#' change in value.
#'
#' @return A ggplot2 plot.
#'
#' @export
#'

plot_2D_imputed_sim = function(data,
                               mask,
                               imputed_data,
                               alpha = 0.25,
                               jitter = 0.4,
                               alpha_line = 1,
                               clms = c(1,2),
                               type = "scatter", # "tile"
                               title = NULL){

  if(length(clms) != 2){
    stop("Length of clms needs to be exactly 2")
  }

  plots = analyse_missingness_plots(data = data,
                                    clms = clms,
                                    mask = mask,
                                    imputed.data = imputed_data,
                                    error.groups = NULL,
                                    first.clms.entry.missing = TRUE,
                                    only.first.mask.missingness = FALSE,
                                    jitter = jitter,
                                    alpha_line = alpha_line,
                                    title = title,
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
  } else{
    stop("Type is not eligible.")
  }
  return(plot)
}

