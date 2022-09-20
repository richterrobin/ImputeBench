

#' Plotting Observed and Missing Data Distributions (3D)
#'
#' @description Plotting the distribution of the observed and missing data entries for a given data matrix and
#' missingness pattern, in 3 dimensions using ggplot2 and plotly.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries (ground truth matrix).
#' @param mask A matrix with binary entries (0 = missing, 1 = not missing), the (additional) missingness pattern on which the
#' imputed matrix is compared against the ground truth matrix.
#' @param title A character string controlling the title of the plot. Default is `NULL`.
#' @param clms An integer vector, indicating the columns to be plotted. Default is `c(1,2,3)`.
#' @param type A character string. In the case the third variable is discrete, choose between `"scatter"` and `"facets"`.
#' Default is `"scatter"`.
#' @param jitter A positive numeric controlling the jitter on the discrete values. Default is `0.4`.
#' @param alpha A positive numeric controlling the transparency of the observed data points. Default is `0.25`.
#' @param only.first.entry Boolean, if `TRUE` only the missingness pattern of the first entry of `clms` is distinguished.
#' If `FALSE` distinction between all possible combination of missing entries between the columns indicated in `clms` is made.
#' Default is `FALSE`.
#'
#' @details Plotting the missingness pattern of `clms` of `data` (depending on `only.first.entry` considering only the first entry
#' of `clms` or all) against its/their data distribution. All rows with a (real) missing value in `clms` are left out.
#'
#' ### `type = "scatter"`
#' The output plot is a 3D scatter point plot (with additional jitter in the case that one, two or all variables of `data.clms` are
#' discrete) with color coded missingness pattern of the first (if `only.first.entry`is `TRUE`) or all (if `only.first.entry` is `FALSE`)
#' columns of `clms`. The `alpha` parameter controls the alpha of the complete points. The `jitter` parameter controls the extra jitter.
#'
#' ### `type = "facets"`
#' The output plot is a 2D scatter point plot faceted by the third variable of `data.clms` with color coded missingness
#' pattern of the first (if `only.first.entry`is `TRUE`) or all (if `only.first.entry` is `FALSE`) columns of `clms`.
#' Only available if the third column of `data.clms` is a discrete variable. The `alpha` parameter controls the alpha of the complete points.
#' The `jitter` parameter controls the extra jitter.
#'
#' @return A ggplot2 plot (`type = facets`) or a plotly plot (`type = "scatter"`).
#'
#' @export
#'

plot_3D_simulated_missingness = function(data,
                                         mask,
                                         clms = c(1,2,3),
                                         type = "scatter", #  "facets"
                                         jitter = 0.4,
                                         alpha = 0.25,
                                         title = NULL,
                                         only.first.entry = FALSE){

  if(length(clms) != 3){
    stop("Length of clm needs to be exactly 3")
  }
  if(sum(mask[,clms[1]]) == nrow(mask) ){
    stop("The first column does not observe missing entries")
  }

  plots = analyse_missingness_plots(data = data,
                                    clms = clms,
                                    mask = mask,
                                    error.groups = NULL,
                                    first.clms.entry.missing = TRUE,
                                    only.first.mask.missingness = only.first.entry,
                                    alpha_set = alpha,
                                    title = title,
                                    jitter = jitter)
  # "scatter" %in% names(plots)
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
      stop("Type facets can only be used if the third column is discrete")
    }
  } else{
    stop("Type is not eligible.")
  }
  return(plot)
}
