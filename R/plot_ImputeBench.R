

#' Plotting the Results of ImputeBench
#'
#' @description Plotting with ggplot2 the results of `data_ImputeBench` and `simulation_ImputeBench`.
#'
#' @param Evaluation The results of `data_ImputeBench` or `simulation_ImputeBench` given in the data frame `$evaluation`.
#' @param title A title for the plot. Default is `"Imputation Performance"`.
#' @param norm_by_baseline Boolean. If `FALSE` the total error is reported. If `TRUE` the relative error (divided by the baseline
#' imputation method error) is reported. Default is `TRUE`.
#' @param y_axis A character string, title of the y-axis. Default is `NULL`.
#' @param color_axis_labels A vector of character strings, labeling the color axis labels. Default is `NULL`.
#' @param group A character string. Can be used in the case that `Evaluation` is the result of running `data_ImputeBench` with given
#' error grouping (see `data_ImputeBench` for details): Specifying `group` one can select the imputation performance over an error
#' group to be plotted by passing its name. Default is `NULL` (plotting the overall error).
#' @param x_axis A character string. Can be used in the case that `Evaulation` is the result of running `simulation_ImputeBench`.
#' One can choose a data or missingness parameter to be used to label the x-axis. For the choices see the accompanying vignette.
#' Default is \code{NULL}.
#' @param error_type A character string. Can be used in the case that `Evaulation` is the result of running `simulation_ImputeBench`.
#' If `error_type = "overall"` the usual error is plotted, if one of the following `"gauss"`, `"pois"`, `"binary.mse"`,
#' `"binary.log.odds"`, `"t"`, `"sine"`, `"spline"` or `"continuous"` is passed via `error_type` the errors on the respective
#' data types are plotted. Default is `"overall"`.
#'
#' @details This function plots a boxplot over the different missingness scenarios or data/missingness parameter choices reporting
#' on the y-axis the imputation error of all imputation methods compared which are distinguished by color.
#'
#' @return A ggplot2 plot.
#'
#' @import ggplot2
#' @import stringr
#'
#' @export
#'

plot_ImputeBench = function(Evaluation,
                            title = "Imputation Performance",
                            group = NULL,
                            x_axis = NULL,
                            y_axis = NULL,
                            norm_by_baseline = TRUE,
                            color_axis_labels = NULL,
                            error_type = "overall"){

  # Get rid of notes:
  descriptor = NULL; error = NULL; Method = NULL

  if(norm_by_baseline){
    Evaluation = divide_by_baseline(Evaluation)
    normed = TRUE
    if(base::is.null(y_axis)){
      yAxis.name =  "(Loss of Method) / (Loss of Baseline)"
    } else{
      yAxis.name = y_axis
    }
  } else{
    normed = FALSE
    if(base::is.null(y_axis)){
      yAxis.name =  "Loss of Method"
    } else{
      yAxis.name = y_axis
    }
  }

  if(!is.null(group)){
    if(!(group %in% colnames(Evaluation))){
      stop("Group name not found.")
    }
    error.clm = which(colnames(Evaluation) %in% group)

  } else{
    if(is.null(stringr::str_which(names(Evaluation), pattern = base::paste("imputation.error.", error_type, sep = "")))){
      stop("Error type not found.")
    }
    error.clm = stringr::str_which(names(Evaluation), pattern = base::paste("imputation.error.", error_type, sep = ""))
  }

  # View(Results.4$evaluation)

  if(!("Setting" %in% names(Evaluation)) ){
    Evaluation[,c(base::ncol(Evaluation)+1,base::ncol(Evaluation)+2)] =
      stringr::str_split_fixed(string = Evaluation$ID, pattern = "\\.", n = 2)
    names(Evaluation)[c(base::ncol(Evaluation)-1,base::ncol(Evaluation))] = c("Setting","Run")
  }

  nbr.competing = base::length(base::which(Evaluation$ID == "1.1"))
  nbr.runs = base::max(base::as.numeric(Evaluation$Run))
  nbr.settings = base::max(base::as.numeric(Evaluation$Setting))

  if(normed){
    df = data.frame("Setting" = base::rep(1:nbr.settings, each = (nbr.competing - 1)*nbr.runs),
                    "Method" = base::rep(Evaluation$method[1:(nbr.competing-1)], times = nbr.settings*nbr.runs),
                    "Run" = base::rep(1:nbr.runs, each = nbr.competing-1, times = nbr.settings),
                    "error" = base::rep(NA, times = nbr.settings*nbr.runs*(nbr.competing-1)))
  } else{
    df = data.frame("Setting" = base::rep(1:nbr.settings, each = (nbr.competing)*nbr.runs),
                    "Method" = base::rep(Evaluation$method[1:(nbr.competing)], times = nbr.settings*nbr.runs),
                    "Run" = base::rep(1:nbr.runs, each = nbr.competing, times = nbr.settings),
                    "error" = base::rep(NA, times = nbr.settings*nbr.runs*nbr.competing))
  }

  for(k in 1:base::nrow(df)){
    df$error[k] = Evaluation[Evaluation$Setting == df$Setting[k]
                             & Evaluation$method == df$Method[k]
                             & Evaluation$Run == df$Run[k], error.clm]
  }

  if(base::is.null(x_axis)){
    if(normed){
    if("parameters.name" %in% names(Evaluation)){
      df$descriptor = as.factor(Evaluation$parameters.name[which((1:nrow(Evaluation) %% nbr.competing) != 0)])
      xAxis.name = "Parameter Choice"
    } else{
      df$descriptor = as.factor(Evaluation$scenario[which((1:nrow(Evaluation) %% nbr.competing) != 0)])
      xAxis.name = "Scenario"
    }
    } else{
      if("parameters.name" %in% names(Evaluation)){
        df$descriptor = as.factor(Evaluation$parameters.name)
        xAxis.name = "Parameter Choice"
      } else{
        df$descriptor = as.factor(Evaluation$scenario)
        xAxis.name = "Scenario"
      }
    }
  } else{
    desc.vec =  rep(0, times = nrow(df))
    for(k in 1:base::nrow(df)){
      desc.vec[k] = switch(x_axis,
                           "n" = Evaluation$nbr.rows[Evaluation$Setting == df$Setting[k]
                                                     & Evaluation$method == df$Method[k]
                                                     & Evaluation$Run == df$Run[k]],
                           "p_gauss" = Evaluation$gaussian.columns[Evaluation$Setting == df$Setting[k]
                                                                   & Evaluation$method == df$Method[k]
                                                                   & Evaluation$Run == df$Run[k]],
                           "p_poisson" = Evaluation$poisson.columns[Evaluation$Setting == df$Setting[k]
                                                                    & Evaluation$method == df$Method[k]
                                                                    & Evaluation$Run == df$Run[k]],
                           "p_binary" = Evaluation$binary.columns[Evaluation$Setting == df$Setting[k]
                                                                  & Evaluation$method == df$Method[k]
                                                                  & Evaluation$Run == df$Run[k]],
                           "p_t" = Evaluation$t.columns[Evaluation$Setting == df$Setting[k]
                                                        & Evaluation$method == df$Method[k]
                                                        & Evaluation$Run == df$Run[k]],
                           "p_sine" = Evaluation$sine.columns[Evaluation$Setting == df$Setting[k]
                                                              & Evaluation$method == df$Method[k]
                                                              & Evaluation$Run == df$Run[k]],
                           "p_spline" = Evaluation$spline.columns[Evaluation$Setting == df$Setting[k]
                                                                  & Evaluation$method == df$Method[k]
                                                                  & Evaluation$Run == df$Run[k]],
                           "size_MCAR" = Evaluation$mask.MCAR.columns[Evaluation$Setting == df$Setting[k]
                                                                      & Evaluation$method == df$Method[k]
                                                                      & Evaluation$Run == df$Run[k]],
                           "size_MAR" = Evaluation$mask.MAR.columns[Evaluation$Setting == df$Setting[k]
                                                                    & Evaluation$method == df$Method[k]
                                                                    & Evaluation$Run == df$Run[k]],
                           "size_MNAR" = Evaluation$mask.MMAR.columns[Evaluation$Setting == df$Setting[k]
                                                                      & Evaluation$method == df$Method[k]
                                                                      & Evaluation$Run == df$Run[k]],
                           "probability_MCAR" = Evaluation$mask.MCAR.probability[Evaluation$Setting == df$Setting[k]
                                                                                 & Evaluation$method == df$Method[k]
                                                                                 & Evaluation$Run == df$Run[k]],
                           "p" = Evaluation$nbr.columns[Evaluation$Setting == df$Setting[k]
                                                        & Evaluation$method == df$Method[k]
                                                        & Evaluation$Run == df$Run[k]],
                           "sparsity" = Evaluation$data.spars[Evaluation$Setting == df$Setting[k]
                                                              & Evaluation$method == df$Method[k]
                                                              & Evaluation$Run == df$Run[k]],
                           "q" = Evaluation$data.low.dim[Evaluation$Setting == df$Setting[k]
                                                         & Evaluation$method == df$Method[k]
                                                         & Evaluation$Run == df$Run[k]],
                           "lowdim_error" = Evaluation$data.low.dim.errorStD[Evaluation$Setting == df$Setting[k]
                                                                             & Evaluation$method == df$Method[k]
                                                                             & Evaluation$Run == df$Run[k]],
                           "groups" = Evaluation$data.groups[Evaluation$Setting == df$Setting[k]
                                                             & Evaluation$method == df$Method[k]
                                                             & Evaluation$Run == df$Run[k]],
                           "non_linearity" = Evaluation$data.non.linearity[Evaluation$Setting == df$Setting[k]
                                                                           & Evaluation$method == df$Method[k]
                                                                           & Evaluation$Run == df$Run[k]],
                           "low_dim_groups" = Evaluation$data.low.dim.groups[Evaluation$Setting == df$Setting[k]
                                                                             & Evaluation$method == df$Method[k]
                                                                             & Evaluation$Run == df$Run[k]],
                           "transl_factor" = Evaluation$data.groups.distance[Evaluation$Setting == df$Setting[k]
                                                                             & Evaluation$method == df$Method[k]
                                                                             & Evaluation$Run == df$Run[k]])
    }
    nm = switch(x_axis,
                "n" = "Sample Size",
                "p_gauss" = "Number of Gaussian Columns",
                "p_poisson" = "Number of Poisson Columns",
                "p_binary" = "Number of Binary Columns",
                "p_t" = "Number of t-distributed Columns",
                "p_sine" = "Number of sine-transformed Columns",
                "p_spline" = "Number of spline-transformed Columns",
                "size_MCAR" = "Number of MCAR-masked Columns",
                "size_MAR" = "Number of MAR-masked Columns",
                "size_MNAR" = "Number of MNAR-masked Columns",
                "probability_MCAR" = "Amount MCAR missing",
                "p" = "Number of Columns",
                "sparsity" = "Sparsity of the Covariate Matrix of the Latent Gaussian Model",
                "q" = "Latent Dimension",
                "lowdim_error" = "Standard Deviation of the Error in the Latent Gaussian Model",
                "groups" = "Tranlsation Groups",
                "low_dim_groups" = "Regression Groups",
                "non_linearity" = "Non Linearity Parameter",
                "transl_factor" = "Translation Factor")


    df$descriptor = desc.vec

    xAxis.name = nm

  }

  if(!(error_type == "binary.logodds")){


    plot = ggplot2::ggplot(data = df) +
      ggplot2::geom_boxplot(ggplot2::aes(x = as.factor(descriptor), y = error, color = Method)) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 0.5, vjust = 0.5)) +
      ggplot2::labs(title = title,
                    y = yAxis.name,
                    x = xAxis.name)
    if(normed){
      plot = plot + ggplot2::geom_hline(yintercept = 1 , color = "black")
    }
  } else{

    plot = ggplot2::ggplot(data = df) +
      ggplot2::geom_boxplot(ggplot2::aes(x = as.factor(descriptor), y = error, color = Method)) +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(trans = "log") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 0.5, vjust = 0.5)) +
      ggplot2::labs(title = title,
                    y = yAxis.name,
                    x = xAxis.name)
  }

  if(!is.null(color_axis_labels)){
    plot = plot + ggplot2::scale_color_discrete(labels = color_axis_labels )
  }

  return(plot)
}
