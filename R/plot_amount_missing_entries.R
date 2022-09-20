
#' Amount of Missing Entries Plot
#'
#' @description Plot the amount of missing entries as a box- or pointplot over groups of columns
#'
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries.
#' @param groups A list of column groups. If provided, the output is given group-wise, rather than variable-wise. Default is `NULL`,
#' i.e. variable-wise.
#' @param y.axis A character string, either `"percent"` or `"amount"` controlling the labeling of the y-axis either report
#' relative or total amount of missing entries. Default is `"percent"`.
#' @param thres_name A positive numeric, threshold (percentage of missing entries) whose name of the variables should be displayed?
#' Only applicable if `groups = NULL`. Default is `0.05`.
#' @param jitter A positive numeric controlling the jitter in horizontal direction. Only applicable if `groups = NULL`. Default
#' is `0.4`.
#' @param alpha A numeric between 0 and 1 controlling the transparency of the points. Only applicatlbe if `groups = NULL`. Default
#' is 1.
#' @param seed An integer to fix a seed of the jitter. ONly applicable if `groups = NULL`. Default is `NULL`.
#' @param type A character string, one of the following: `"box"`, `"total"` and `"mean"`, controlling the type of plot.
#' Only applicable if `groups` are provided. Default is `"box"`.
#' @param title A character string controlling the title of the plot. Default is `NULL`.
#'
#' @details Plotting the relative/total amount of missing entries in each column/variable of the given data set or in each group of columns
#' using ggplot2.
#'
#' ## Without Column Grouping (`groups = NULL`)
#' Plotting a point-plot (with jitter in the horizontal direction). Labels and ticks on the y-axis are determined by `y.axis`.
#' Which variables are labeled is determined by `thres_name`. Jitter can be controlled via the parameters `jitter` and `seed`,
#' transparency can be controlled via `alpha`. The parameter `type` is discarded.
#'
#' ## With Column Grouping
#' For `type="box"` plotting a box-plot graphic of the distributions of missing values (relative or total depending on `y.axis`) over the
#' variables of the groups specified in `groups`. For `type="mean"` plotting a point-plot of the mean values of missing values (relative
#' or total depending on `y.axis`) over the variables of the groups specified in `groups`. For `type="total"` plotting a point-plot of the
#' total amount of missing entries in each group (not compatible with `y.axis="percent"`). The parameters `jitter`, `alpha`, `thres_name` and
#' `seed` are discarded.
#'
#' @return A ggplot2 plot.
#'
#' @export
#'

plot_amount_missing_entries = function(data,
                                       groups = NULL,
                                       thres_name = 0.05,
                                       jitter = 0.4,
                                       alpha = 1,
                                       seed = NULL,
                                       y.axis = "percent",# "amount" except for type = "total" where the y.axis is always "amount".
                                       type = "box", # c("total", "mean")
                                       title = NULL){
  if(type %in% c("box", "total", "mean")){
    if(y.axis %in% c("percent","amount")){

      if(identical(type, "total")){
        y.axis = "amount"
      }
      if(is.null(seed)){
        seed = sample(size = 1, x = 1:100000)
      }

      plots = analyse_missingness_plots(data = data,
                                        clms = NULL,
                                        mask = NULL,
                                        title = title,
                                        seed = seed,
                                        error.groups = groups,
                                        missing.label = y.axis,
                                        first.clms.entry.missing = FALSE,
                                        only.first.mask.missingness = FALSE,
                                        thres_name = thres_name,
                                        alpha_set = alpha,
                                        jitter = jitter)

      if(!is.null(groups)){
        if(identical(type, "box")){
          plot = plots$box
        } else if(identical(type, "total")){
          plot = plots$tot
        } else if(identical(type, "mean")){
          plot = plots$mean
        } else{
          stop("Not a valid type.")
        }
      } else{
        plot = plots$variable
      }
    } else{
      stop("y.axis must be either 'percent' or 'amount'.")
    }
  } else{
    stop("type must be one of 'box', 'mean' or 'total'." )
  }
  return(plot)
}
