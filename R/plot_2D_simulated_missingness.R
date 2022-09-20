

#' Plotting Observed and Missing Data Distributions (2D)
#'
#' @description Plotting the distribution of the observed and missing data entries for a given data matrix and
#' missingness pattern, in 2 dimensions using ggplot2.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries (ground truth matrix).
#' @param mask A matrix with binary entries (0 = missing, 1 = not missing), the (additional) missingness pattern on which the
#' imputed matrix is compared against the ground truth matrix.
#' @param title A character string controlling the title of the plot. Default is `NULL`.
#' @param clms An integer vector, indicating the columns to be plotted. Default is `c(1,2)`.
#' @param type A character string. In the case one variable is discrete and the other continuous, choose between `"scatter"`
#' and `"box"`. Default is `"scatter"`. In the case both variables are discrete, choose between `"scatter"` and `"facets"`.
#'Default is `"scatter"`.
#' @param jitter A positive numeric controlling the jitter on the discrete values. Default is `0.4`.
#' @param alpha A positive numeric controlling the transparency of the observed data points. Default is `0.25`.
#' @param only.first.entry Boolean, if `TRUE` only the missingness pattern of the first entry of `clms` is distinguished.
#' If `FALSE` distinction between all possible combination of missing entries between the columns indicated in `clms` is made.
#' Default is `FALSE`.
#'
#' @details Plotting the missingness pattern of `clms` of `data` (depending on `only.first.entry` considering only the first entry
#' of `clms` or all) against their data distribution. All rows with a (real) missing value in `clms` are left out.
#'
#' ### `type = "scatter"`
#' The output plot is a 2D scattered point plot (with additional jitter in the case that one or both variables of `clms` are
#' discrete) with color coded missingness of the first (if `only.first.entry`is `TRUE`) or all (if `only.first.entry` is `FALSE`)
#' columns of `clms`. The `alpha` parameter controls the alpha of the complete points. The `jitter` parameter controls the extra jitter.
#'
#' ### `type = "box"`
#' The output plot is a box plot stratified by the discrete variable of `clms` on the x-axis and color coded by the missingness
#' pattern of the first (if `only.first.entry`is `TRUE`) or all (if `only.first.entry` is `FALSE`)
#' columns of `clms`. Only available if `clms` consists of a discrete and a continuous variable.
#'
#' ### `type = "facets"`
#' The output plot is a filled bar plot faceted by the second variable of `clms` and color coded by the missingness
#' pattern of the first (if `only.first.entry`is `TRUE`) or all (if `only.first.entry` is `FALSE`)
#' columns of `clms`. Only available if `clms` consists of two discrete variables.
#'
#' @return A ggplot2 plot.
#'
#' @export
#'

plot_2D_simulated_missingness = function(data,
                                         mask,
                                         clms = c(1,2),
                                         type = "scatter", # "box" or "facets"
                                         only.first.entry = FALSE,
                                         alpha = 0.25,
                                         jitter = 0.4,
                                         title = NULL ){

  if(length(clms) != 2){
    stop("Length of clm needs to be exactly 2")
  }
  if(sum(mask[,clms[1]]) == nrow(mask) ){
    stop("The first column does not observe missing entries")
  }


  plots = analyse_missingness_plots(data = data,
                                    clms = clms,
                                    mask = mask,
                                    alpha_set = alpha,
                                    error.groups = NULL,
                                    first.clms.entry.missing = TRUE,
                                    title = title,
                                    only.first.mask.missingness = only.first.entry,
                                    jitter = jitter)

  # "scatter" %in% names(plots)
  if(identical(type, "scatter")){
    if("scatter" %in% names(plots)){
      plot = plots$scatter
    } else{
      plot = plots
    }
  } else if(identical(type, "box")){
    if("box" %in% names(plots)){
      plot = plots$box
    } else{
      stop("Type box can only be used for one discrete and one continuous variable")
    }
  } else if(identical(type, "facets")){
    if("facets" %in% names(plots)){
      plot = plots$facets
    } else{
      stop("Type facets can only be used for two discrete variables")
    }
  } else{
    stop("Type is not eligible.")
  }
  return(plot)
}

