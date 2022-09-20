

#' ImputeBench Internal Plot Function
#'
#' @param data A numeric matrix (possibly with missing entries given by `NA`).
#' @param imputed.data A numeric matrix (the imputed counterpart to `data`). Default is `NULL`.
#' @param clms The columns/variables that should be plotted. Default is `NULL`.
#' @param mask A binary matrix indicating additional missing entries in `data` (0 = missing, 1 = present). Default is `NULL`.
#' @param error.groups A list of column indices yielding a grouping of the columns. Default is NULL.
#' @param missing.label A character sting. How to report the amount missing, as `"percent"` or `"amount"`?
#' @param first.clms.entry.missing Boolean, should the data distribution be displayed or the missingness of the first entry of `clms` with respect
#' to the data distributions of the remaining entries in `clms`? Default is `FALSE`.
#' @param only.first.mask.missingness Boolean, should only the missingness of the first entry in clms be considered or the missingness in all entries of `clms`?
#' Default is `TRUE`.
#' @param jitter Numeric controlling the jitter added to count variables. Default is `0.4`.
#' @param alpha_set Numeric controlling the alpha/transparency of the data points (or the existing data points). Default is `0.25`.
#' @param alpha_line Numeric controlling the alpha/transparency of the lines connecting missing and imputed data points. Default is `1`.
#' @param seed Integer controlling the seed in the jitter of the labels when plotting amount of missing entries per column. Default is `1`.
#' @param thres_name Numeric controlling the threshold over which labels of the column names should be plotted when plotting amount of
#' of missing entires per column. Default is `0.05`.
#' @param binwidth Integer controlling the bin width for histogram plots. Default is `NULL`, resulting in `ncol(data)/30`.
#' @param multiple.imputations List of numeric matrices: Do we compare in the 1D plots multiple imputation methods on ground truth data?
#' Then `multiple.imputations` lists the additional imputed matrices (additional to `imputed.data`). Default is `NULL`.
#' @param imp.names A vector of character strings. In the case a list of imputation matrices is provided in `multiple.imputations` this
#' vector can provide names for the imputation methods used, i.e. controls the color legend. Default is `NULL`.
#' @param title A character string: User specified title of the resulting plot. Default is `NULL`.
#'
#'
#' @details Internal function for plotting data, missing data, imputed data and missingness patterns.
#'
#' @import matrixStats
#' @import ggplot2
#' @import plotly
#' @import stats
#'
#' @return A plot or a list of plots.
#'


analyse_missingness_plots = function(data,
                                     imputed.data = NULL,
                                     clms = NULL,
                                     mask = NULL,
                                     error.groups = NULL,
                                     missing.label = "percent", # or "amount"
                                     first.clms.entry.missing = FALSE,
                                     only.first.mask.missingness = TRUE,
                                     jitter = 0.4,
                                     alpha_set = 0.25,
                                     alpha_line = 1,
                                     seed = 1,
                                     title = NULL,
                                     thres_name = 0.05,
                                     binwidth = NULL,
                                     multiple.imputations = NULL,
                                     imp.names = NULL){

  # Get rid of notes in check package:
  x = NULL; quest = NULL; group = NULL; missing_normalized = NULL; missy = NULL; totalnonmissing = NULL
  totalmissing = NULL; value = NULL; missz = NULL; missa = NULL; alpha2 = NULL;  missx = NULL;  missxyz = NULL
  missboth = NULL;  impx = NULL;  imputed = NULL;  imputation = NULLvalueimp = NULL;  valuemiss = NULL;  valueimp = NULL
  impy = NULL;  valueimp = NULL;  grp = NULL; imp = NULL

  data = as.data.frame(data)

  n = base::nrow(data)
  p = base::ncol(data)
  count.binary =  determine_count_binary(data)
  count.clms = count.binary[[1]]
  length.un.fun = function(x){length(unique(x))}
  how.many.levels = apply(data[,count.clms], MARGIN = 2 , FUN = length.un.fun)
  count.clms = count.clms[which(how.many.levels <= 15)]
  cont.clms = setdiff(1:p, count.clms)


  if(is.null(mask)){
    miss.pattern = base::matrix(as.numeric(is.na(data)),
                                ncol = ncol(data),
                                nrow = nrow(data))
    nbr.missing = data.frame("missing" = matrixStats::colSums2(miss.pattern, na.rm = TRUE),
                             "quest" = names(data))
    rownames(nbr.missing) = names(data)
    nbr.missing$quest = factor(nbr.missing$quest, levels = nbr.missing$quest)
    nbr.missing$x = "Variable"
  } else{
    # To be consistent with the "is.null(mask)-case" we are going to define 0 as complete and 1 as missing
    mask = abs(mask - 1)
    nbr.missing = as.data.frame(matrixStats::colSums2(mask, na.rm = TRUE))
    names(nbr.missing) = "missing"
    nbr.missing$quest = names(data)
    rownames(nbr.missing) = names(data)
    nbr.missing$quest = factor(nbr.missing$quest, levels = nbr.missing$quest)
    nbr.missing$x = "Variable"
    mask = abs(mask - 1)
  }

  if(is.null(imputed.data)){
    if(!is.null(error.groups)){
      if(is.null(mask)){

        nbr.missing.in.group = as.data.frame(matrix(base::unlist(
          base::lapply(error.groups, FUN = function(x){c(length(x),base::sum(nbr.missing$missing[x]))})),byrow = TRUE ,
          nrow = length(error.groups)))
        names(nbr.missing.in.group) = c("questions", "missing")
        if(is.null(names(error.groups))){
          nbr.missing.in.group$name = paste0("Group ", 1:length(error.groups))
          nbr.missing.in.group$name = factor(nbr.missing.in.group$name, levels = nbr.missing.in.group$name )
        } else{
          nbr.missing.in.group$name = names(error.groups)
          # names(error.groups)
          # rownames(nbr.missing.in.group) = names(error.groups)
          # amount.missing = nbr.missing / base::nrow(miss.pattern)
          nbr.missing.in.group$name = factor(nbr.missing.in.group$name, levels = nbr.missing.in.group$name )
        }

      } else{

        nbr.missing.in.group = as.data.frame(matrix(base::unlist(
          base::lapply(error.groups, FUN = function(x){c(length(x),base::sum(nbr.missing$missing[x]))})),byrow = TRUE , nrow = 15))
        names(nbr.missing.in.group) = c("questions", "missing")
        if(is.null(names(error.groups))){
          nbr.missing.in.group$name = paste0("Group ", 1:length(error.groups))
          nbr.missing.in.group$name = factor(nbr.missing.in.group$name, levels = nbr.missing.in.group$name )
        } else{
          nbr.missing.in.group$name = names(error.groups)
          # rownames(nbr.missing.in.group) = names(error.groups)
          # amount.missing = nbr.missing / base::nrow(miss.pattern)
          nbr.missing.in.group$name = factor(nbr.missing.in.group$name, levels = nbr.missing.in.group$name )
        }
      }

      which_list = function(obj, list){
        llist = length(list)
        ret = 0
        for(t in 1:llist){
          if(obj %in% list[[t]]){
            ret = t
          }
        }
        if(is.null(names(list))){
          return(paste0("Group ",ret))
        } else{
          return(names(list)[[ret]])
        }
      }
      # View(nbr.missing)


      #which_list(15, error.groups)

      group.labels = unlist(sapply(1:p, FUN = function(x){return(which_list(x,error.groups))}))

      nbr.missing = cbind(nbr.missing, group.labels)
      names(nbr.missing)[ncol(nbr.missing)] = "group"
      nbr.missing$group = factor(nbr.missing$group, levels = nbr.missing.in.group$name)
    }


    if(is.null(mask) & is.null(error.groups) & is.null(clms)){
      if(base::identical(missing.label,"percent")){

        if(is.null(title)){
          title = "Percentage of Missing Entries"
          switch.back = TRUE
        } else{
          switch.back = FALSE
        }

        var.missing = ggplot2::ggplot(data = nbr.missing, ggplot2::aes(x = x, y = 100*(missing/n))) + #, x = as.factor(quest))) +
          ggplot2::geom_jitter(alpha = alpha_set,position = ggplot2::position_jitter(seed = seed + 100, h = 0, w = jitter)) +
          ggplot2::theme_bw() +
          ggplot2::labs(title = title ,
                        x = "",
                        y = "Percentage of Missing Entries") +
          ggplot2::geom_text(position = ggplot2::position_jitter(seed = seed + 100, h = 0, w = jitter), size = 3,
                             ggplot2::aes(label = ifelse(missing >= (thres_name*n) ,as.character(quest), "")), vjust = -1,
                             alpha = 0.7) +
          ggplot2::ylim(0,NA) +
          # ggplot2::geom_hline(yintercept = alpha*100, color = "red", alpha = 0.5) +
          # ggplot2::geom_hline(yintercept = alpha2*100, color = "black", alpha = 0.5) +
          ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                         axis.ticks.x = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank())

        # var.missing
        if(switch.back){
          title = NULL
        }

        if(is.null(title)){
          title = "Histogram over the Amount of Missing Entries"
          switch.back = TRUE
        } else{
          switch.back = FALSE
        }
        if(is.null(binwidth)){
          binwidth = 1/30
        }
        missing.hist = ggplot2::ggplot(data = nbr.missing, ggplot2::aes(x = (missing/nrow(data)))) +
          ggplot2::theme_bw() +
          ggplot2::geom_histogram(binwidth = binwidth) +
          ggplot2::labs(title = title,
                        y = "",
                        x = "Percentage of Missing Entries") +
          ggplot2::xlim(0,1)

        if(switch.back){
          title = NULL
        }

      } else if(base::identical(missing.label, "amount")){

        if(is.null(title)){
          title = "Amount of Missing Entries"
          switch.back = TRUE
        } else{
          switch.back = FALSE
        }

        var.missing = ggplot2::ggplot(data = nbr.missing, ggplot2::aes(x = x, y = missing)) + #, x = as.factor(quest))) +
          ggplot2::geom_jitter(alpha = alpha_set,position = ggplot2::position_jitter(seed = seed + 100, h = 0, w = jitter)) +
          ggplot2::theme_bw() +
          ggplot2::labs(title = title ,
                        x = "",
                        y = "Amount of Missing Entries") +
          ggplot2::geom_text(position = ggplot2::position_jitter(seed = seed + 100, h =0, w = jitter), size = 3,
                             ggplot2::aes(label = ifelse(missing > (alpha*n) ,as.character(quest), "")), vjust = -1,
                             alpha = 0.7) +
          ggplot2::ylim(0,NA) +
          # ggplot2::geom_hline(yintercept = alpha*n, color = "red", alpha = 0.5) +
          # ggplot2::geom_hline(yintercept = alpha2*n, color = "black", alpha = 0.5) +
          ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                         axis.ticks.x = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank())

        if(switch.back){
          title = NULL
        }

        if(is.null(title)){
          title = "Histogram over the Amount of Missing Entries"
          switch.back = TRUE
        } else{
          switch.back = FALSE
        }
        if(is.null(binwidth)){
          binwidth = nrow(data)/30
        }
        missing.hist = ggplot2::ggplot(data = nbr.missing, ggplot2::aes(x = missing)) +
          ggplot2::theme_bw() +
          ggplot2::geom_histogram(binwidth = binwidth) +
          ggplot2::labs(title = title,
                        y = "",
                        x = "Missing Entries") +
          ggplot2::xlim(0,nrow(data))
        if(switch.back){
          title = NULL
        }
      }
    }

    if(is.null(mask) & !is.null(error.groups) & is.null(clms)){

      if(is.null(title)){
        title = "Histogram over the Amount of Missing Entries per Group"
        switch.back = TRUE
      } else{
        switch.back = FALSE
      }
      if(base::identical(missing.label, "percent")){
        if(is.null(binwidth)){
          binwidth = 1/30
        }
        missing.hist.group = ggplot2::ggplot(data = nbr.missing, ggplot2::aes(x = (missing/nrow(data)))) +
          ggplot2::facet_wrap(.~ group) +
          ggplot2::theme_bw() +
          ggplot2::geom_histogram(binwidth = binwidth) +
          ggplot2::labs(title = title,
                        y = "",
                        x = "Percentage of Missing Entries") +
          ggplot2::xlim(0,1)

      } else if(base::identical(missing.label, "amount")){
        if(is.null(binwidth)){
          binwidth = nrow(data)/30
        }
        missing.hist.group = ggplot2::ggplot(data = nbr.missing, ggplot2::aes(x = (missing))) +
          ggplot2::facet_wrap(.~ group) +
          ggplot2::theme_bw() +
          ggplot2::geom_histogram(binwidth = binwidth) +
          ggplot2::labs(title = title,
                        y = "",
                        x = "Missing Entries") +
          ggplot2::xlim(0,nrow(data))
      }

      if(switch.back){
        title = NULL
      }

      if(is.null(title)){
        title = "Number of Missing Entries per Group"
        switch.back = TRUE
      } else{
        switch.back = FALSE
      }

      group.missing.tot = ggplot2::ggplot(data = nbr.missing.in.group, ggplot2::aes(y = missing, x = name)) +
        ggplot2::geom_point() +
        ggplot2::theme_bw() +
        ggplot2::labs(title = title ,
                      x = "Groups",
                      y = "#Missing") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        ggplot2::ylim(0,NA)

      if(switch.back){
        title = NULL
      }

      if(is.null(title)){
        title = "Distribution of Missing Entries in a Variable for each Group"
        switch.back = TRUE
      } else{
        switch.back = FALSE
      }
      if(base::identical(missing.label,"percent")){

        group.missing.box =  ggplot2::ggplot(data = nbr.missing, ggplot2::aes(y = 100*(missing/n), x = group)) +
          ggplot2::theme_bw() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))   +
          ggplot2::labs(title = title,
                        x = "Groups",
                        y = "Percentage of Missing Entries") +
          ggplot2::geom_boxplot() +
          ggplot2::ylim(0,NA)
        # group.missing.box
      } else  if(base::identical(missing.label,"amount")){

        group.missing.box = ggplot2::ggplot(data = nbr.missing, ggplot2::aes(y = missing, x = group)) +
          ggplot2::theme_bw() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))   +
          ggplot2::labs(title = title,
                        x = "Groups",
                        y = "#Missing") +
          ggplot2::geom_boxplot() +
          ggplot2::ylim(0,NA)
      }

      if(switch.back){
        title = NULL
      }

      nbr.missing.in.group$missing_normalized = nbr.missing.in.group$missing / nbr.missing.in.group$questions

      if(is.null(title)){
        title = "Mean Amount of Missing Entries in a Variable for each Group"
        switch.back = TRUE
      } else{
        switch.back = FALSE
      }

      if(base::identical(missing.label,"percent")){

        group.missing.mean =  ggplot2::ggplot(data = nbr.missing.in.group, ggplot2::aes(y = 100*(missing_normalized/n), x = name)) +
          ggplot2::geom_point() +
          ggplot2::theme_bw() +
          ggplot2::labs(title = title,
                        x = "Groups",
                        y = "Mean Percentage of the Amount of Missing Entries per Variable") +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)) +
          ggplot2::ylim(0,NA)

      } else  if(base::identical(missing.label,"amount")){

        group.missing.mean = ggplot2::ggplot(data = nbr.missing.in.group, ggplot2::aes(y = missing_normalized, x = name)) +
          ggplot2::geom_point() +
          ggplot2::theme_bw() +
          ggplot2::labs(title = title,
                        x = "Groups",
                        y = "#Missing / #Variables") +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)) +
          ggplot2::ylim(0,NA)
      }

      if(switch.back){
        title = NULL
      }
    }

    # clms = c(2,5)
    # clms = c(2,26,15, 37)
    # clms = c(32,2, 4)

    # !is.null(clms) & is.null(mask)

    if(!is.null(clms) & is.null(mask)){

      if(length(clms) == 1){

        if(is.null(title)){
          title = "Data Visualization"
          switch.back = TRUE
        } else{
          switch.back = FALSE
        }

        data.name = names(data)[clms[1]]
        d1.plot.data = as.data.frame(data[,clms[1]])
        names(d1.plot.data) = c("x")


        if(clms[1] %in% cont.clms){

          if(is.null(binwidth)){
            binwidth = matrixStats::iqr(d1.plot.data$x, na.rm = TRUE)/30
          }

          d1.plot = ggplot2::ggplot(data = d1.plot.data, ggplot2::aes(x = x)) +
            ggplot2::geom_histogram(binwidth = binwidth) +
            ggplot2::theme_bw() +
            ggplot2::labs(title = title,
                          x = data.name[1])

        } else if(!(clms[1] %in% cont.clms)){
          d1.plot = ggplot2::ggplot(data = d1.plot.data, ggplot2::aes(x = x)) +
            ggplot2::geom_histogram() +
            ggplot2::theme_bw() +
            ggplot2::labs(title = title,
                          x = data.name[1])
          #  d1.plot
        }
        if(switch.back){
          title = NULL
        }
        #  length(clms) == 2
      } else if(length(clms) == 2){

        if(first.clms.entry.missing){



          data.names = names(data)[c(clms[2],clms[1])]
          miss.first = as.numeric(!is.na(data[clms[1]]))
          d1.plot.data = as.data.frame(matrix(c(data[,clms[2]],miss.first), ncol = 2))
          names(d1.plot.data) = c("x","missy")
          if(is.null(title)){
            title = paste0("Missing Entries of ", data.names[2], " in Relation to the Value of ", data.names[1])
            switch.back = TRUE
          } else{
            switch.back = FALSE
          }

          if(clms[2] %in% cont.clms){

            d1.plot.miss = ggplot2::ggplot(data = d1.plot.data, ggplot2::aes(y = x, color = factor(missy))) +
              ggplot2::geom_boxplot() +
              ggplot2::theme_bw() +
              ggplot2::labs(title = title,
                            x = "",
                            y = data.names[1]) +
              ggplot2::scale_color_discrete(labels = c(paste(data.names[2], " missing"),
                                                       paste(data.names[2], "complete")),
                                            name = "") +
              ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                             axis.ticks.x = ggplot2::element_blank() )

          } else if(!(clms[2] %in% cont.clms )){

            extra.df = as.data.frame(unique(d1.plot.data$x))
            extra.df = as.data.frame(extra.df[!is.na(extra.df)])
            names(extra.df) = "x"
            for(t in 1:nrow(extra.df)){
              extra.df$totalnonmissing[t] = length(which((d1.plot.data$x == extra.df$x[t]) & (d1.plot.data$missy == 1)))
            }
            for(t in 1:nrow(extra.df)){
              extra.df$totalmissing[t] = length(which((d1.plot.data$x == extra.df$x[t]) & (d1.plot.data$missy == 0)))
            }

            d1.plot.miss = ggplot2::ggplot(data = d1.plot.data, ggplot2::aes(x = x, fill = factor(missy))) +
              ggplot2::geom_bar( position = "fill") +
              ggplot2::theme_bw() +
              ggplot2::labs(title = title,
                            x = data.names[1],
                            y = "") +
              ggplot2::scale_fill_discrete(labels = c(paste(data.names[2], " missing"),
                                                      paste(data.names[2], "complete")),
                                           name = "") +
              ggplot2::geom_text(inherit.aes = FALSE, data = extra.df, ggplot2::aes(x = x, y = 0.025, label = totalnonmissing)) +
              ggplot2::geom_text(inherit.aes = FALSE, data = extra.df, ggplot2::aes(x = x, y = 0.975, label = totalmissing))
          }

          if(switch.back){
            title = NULL
          }

        } else{

          if(is.null(title)){
            title = "Data Visualization"
            switch.back = TRUE
          } else{
            switch.back = FALSE
          }

          data.names = names(data)[c(clms[1], clms[2])]
          d2.plot.data = as.data.frame(data[,c(clms[1], clms[2])])
          names(d2.plot.data) = c("x","y")

          if((clms[1] %in% cont.clms) & (clms[2] %in% count.clms)){

            d2.plot = ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = x, y = y)) +
              ggplot2::geom_point(alpha = alpha_set, position = ggplot2::position_jitter(w = 0, h = jitter)) +
              ggplot2::theme_bw() +
              ggplot2::labs(title = title,
                            x = data.names[1],
                            y = data.names[2])
          } else if((clms[2] %in% cont.clms) & (clms[1] %in% count.clms)){

            d2.plot =  ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = y, y = x)) +
              ggplot2::geom_point(alpha = alpha_set, position = ggplot2::position_jitter(w = 0, h = jitter)) +
              ggplot2::theme_bw() +
              ggplot2::labs(title = title,
                            x = data.names[2],
                            y = data.names[1])
          } else if((clms[1] %in% cont.clms) & (clms[2] %in% cont.clms)){

            d2.plot =   ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = x, y = y)) +
              ggplot2::geom_point(alpha = alpha_set) +
              ggplot2::theme_bw() +
              ggplot2::labs(title = title,
                            x = data.names[1],
                            y = data.names[2])
          } else if((clms[1] %in% count.clms) & (clms[2] %in% count.clms)){

            d2.plot =  ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = x, y = y)) +
              ggplot2::geom_point(alpha = alpha_set, position = ggplot2::position_jitter(w = jitter, h = jitter)) +
              ggplot2::theme_bw() +
              ggplot2::labs(title = title,
                            x = data.names[1],
                            y = data.names[2])
            # d2.plot
            y = stats::na.omit(unique(d2.plot.data$x))
            d2.plot.tile.data = data.frame(x = rep(stats::na.omit(unique(d2.plot.data$x)), times = length(stats::na.omit(unique(d2.plot.data$y)))),
                                           y = rep(stats::na.omit(unique(d2.plot.data$y)), each = length(stats::na.omit(unique(d2.plot.data$x)))))

            d2.plot.tile.data$value = rep(0, times = nrow(d2.plot.tile.data))
            for(tau in 1:nrow(d2.plot.tile.data)){
              d2.plot.tile.data$value[tau] = length(which((d2.plot.data$x == d2.plot.tile.data$x[tau] )& (d2.plot.data$y == d2.plot.tile.data$y[tau]) ))
            }
            d2.plot.tile = ggplot2::ggplot(data = d2.plot.tile.data, ggplot2::aes(x = x, y = y, fill = value)) +
              ggplot2::geom_tile() +
              ggplot2::theme_bw() +
              ggplot2::labs(title = title,
                            x = data.names[1],
                            y = data.names[2]) +
              ggplot2::geom_text(ggplot2::aes(label = value), size = 5, col = "white")

          }

          if(switch.back){
            title = NULL
          }

        }

        #  length(clms) >= 3
      } else if(length(clms) >= 3){
        # first.clms.entry.missing
        if(first.clms.entry.missing){

          # length(clms) == 3

          if(length(clms) == 3){
            data.names = names(data)[c(clms[2], clms[3], clms[1])]
            miss.first = as.numeric(!is.na(data[clms[1]]))
            d2.plot.data = as.data.frame(cbind(data[,c(clms[2], clms[3])], miss.first))
            names(d2.plot.data) = c("x","y", "missz")

            if(is.null(title)){
              title = paste0("Missing Entries of ", data.names[3], " in Relation to the Values of ", data.names[1], " and ", data.names[2])
              switch.back = TRUE
            } else{
              switch.back = FALSE
            }

            if((clms[2] %in% cont.clms) & (clms[3] %in% count.clms)){
              d2.plot.miss = ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = x, y = y, color = as.factor(missz), alpha = as.factor(missz))) +
                ggplot2::geom_point( position = ggplot2::position_jitter(w = 0, h = jitter)) +
                ggplot2::theme_bw() +
                ggplot2::labs(title = title,
                              x = data.names[1],
                              y = data.names[2]) +
                ggplot2::scale_alpha_manual(values = c(1,alpha_set)) +
                ggplot2::scale_color_discrete(labels = c(paste(data.names[3], " missing"),
                                                         paste(data.names[3], "complete")),
                                              name = "") +
                ggplot2::guides(alpha = "none",
                                color = ggplot2::guide_legend(override.aes = list(alpha = c(1,alpha_set))))

              d2.plot.data = d2.plot.data[!is.na(d2.plot.data$y),]

              d2.plot.miss.box = ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = as.factor(y), y = x, color = as.factor(missz))) +
                ggplot2::geom_boxplot() +
                ggplot2::theme_bw() +
                ggplot2::labs(title = title,
                              x = data.names[2],
                              y = data.names[1]) +
                ggplot2::scale_color_discrete(labels = c(paste(data.names[3], " missing"),
                                                         paste(data.names[3], "complete")),
                                              name = "")

            } else if((clms[2] %in% count.clms) & (clms[3] %in% cont.clms)){
              d2.plot.miss = ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = x, y = y, color = as.factor(missz), alpha = as.factor(missz))) +
                ggplot2::geom_point( position = ggplot2::position_jitter(h = 0, w = jitter)) +
                ggplot2::theme_bw() +
                ggplot2::labs(title = title,
                              x = data.names[1],
                              y = data.names[2]) +
                ggplot2::scale_alpha_manual(values = c(1,alpha_set)) +
                ggplot2::scale_color_discrete(labels = c(paste(data.names[3], " missing"),
                                                         paste(data.names[3], "complete")),
                                              name = "") +
                ggplot2::guides(alpha =  "none",
                                color = ggplot2::guide_legend(override.aes = list(alpha = c(1,alpha_set))))

              d2.plot.data = d2.plot.data[!is.na(d2.plot.data$x),]

              d2.plot.miss.box = ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = as.factor(x), y = y, color = as.factor(missz))) +
                ggplot2::geom_boxplot() +
                ggplot2::theme_bw() +
                ggplot2::labs(title = title,
                              x = data.names[1],
                              y = data.names[2]) +
                ggplot2::scale_color_discrete(labels = c(paste(data.names[3], " missing"),
                                                         paste(data.names[3], "complete")),
                                              name = "")


            } else  if((clms[2] %in% cont.clms) & (clms[3] %in% cont.clms)){
              d2.plot.miss = ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = x, y = y, color = as.factor(missz), alpha = as.factor(missz))) +
                ggplot2::geom_point( position = ggplot2::position_jitter(h = 0, w = 0)) +
                ggplot2::theme_bw() +
                ggplot2::labs(title = title,
                              x = data.names[1],
                              y = data.names[2]) +
                ggplot2::scale_alpha_manual(values = c(1,alpha_set)) +
                ggplot2::scale_color_discrete(labels = c(paste(data.names[3], " missing"),
                                                         paste(data.names[3], "complete")),
                                              name = "") +
                ggplot2::guides(alpha = "none",
                                color = ggplot2::guide_legend(override.aes = list(alpha = c(1,alpha_set))))


            } else if((clms[2] %in% count.clms) & (clms[3] %in% count.clms)){
              d2.plot.miss = ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = x, y = y, color = as.factor(missz), alpha = as.factor(missz))) +
                ggplot2::geom_point( position = ggplot2::position_jitter(h= jitter, w = jitter)) +
                ggplot2::theme_bw() +
                ggplot2::labs(title = title,
                              x = data.names[1],
                              y = data.names[2]) +
                ggplot2::scale_color_discrete(labels = c(paste(data.names[3], " missing"),
                                                         paste(data.names[3], "complete")),
                                              name = "") +
                ggplot2::scale_alpha_manual(values = c(1, alpha_set)) +
                ggplot2::guides(alpha =  "none",
                                color = ggplot2::guide_legend(override.aes = list(alpha = c(1,alpha_set))))

              panel_labels = paste0(data.names[2], ": ", unique(d2.plot.data$y))
              names(panel_labels) = (paste0(unique(d2.plot.data$y)))
              # panel_labels

              d2.plot.miss.facet = ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = x, fill = factor(missz))) +
                ggplot2::geom_bar( position = "fill") +
                ggplot2::theme_bw() +
                ggplot2::scale_fill_discrete(labels = c(paste(data.names[3], " missing"),
                                                        paste(data.names[3], "complete")),
                                             name = "") +
                ggplot2::facet_wrap(. ~ y, labeller = ggplot2::labeller(y = panel_labels)) +
                ggplot2::labs(title = title,
                              x = data.names[1],
                              y = "")

            }

            if(switch.back){
              title = NULL
            }

          } else if(length(clms) >= 4){

            if(length(clms) > 4){
              warning("Too many input columns, only the first four are considered.")
            }
            data.names = names(data)[c(clms[2], clms[3], clms[4], clms[1])]
            miss.first = as.numeric(!is.na(data[clms[1]]))
            d3.plot.data = as.data.frame(cbind(data[,c(clms[2], clms[3], clms[4])], miss.first))
            names(d3.plot.data) = c("x","y", "z", "missa")


            if(is.null(title)){
              title = paste0("Missing Entries of ", data.names[4],
                             " in Relation to the Values of ", data.names[1], ", ",
                             data.names[2], " and ", data.names[3])
              switch.back = TRUE
            } else{
              switch.back = FALSE
            }

            # Adding some jitter:
            if(!(clms[2] %in% cont.clms) ){
              d3.plot.data$x = d3.plot.data$x + stats::runif(n = nrow(d3.plot.data),
                                                             min = -jitter,
                                                             max = jitter)
            }
            if(!(clms[3] %in% cont.clms) ){
              d3.plot.data$y = d3.plot.data$y + stats::runif(n = nrow(d3.plot.data),
                                                             min = -jitter,
                                                             max = jitter)
            }
            # The Facet Plot:
            if(clms[4] %in% count.clms){

              d3.plot.facet.data = d3.plot.data
              # clms[4] %in% count.clms
              panel_labels = paste0(data.names[3], ": ", unique(d3.plot.facet.data$z))
              names(panel_labels) = (paste0(unique(d3.plot.facet.data$z)))

              d3.plot.miss.facet = ggplot2::ggplot(data = d3.plot.facet.data, ggplot2::aes(x = x, y = y, color = as.factor(missa), alpha = as.factor(missa))) +
                ggplot2::geom_point() +
                ggplot2::theme_bw() +
                ggplot2::guides(alpha = "none",
                                color = ggplot2::guide_legend(override.aes = list(alpha = c(1, alpha_set)))) +
                ggplot2::scale_alpha_manual(values = c(1, alpha_set)) +
                ggplot2::scale_color_discrete(labels = c(paste(data.names[4], " missing"),
                                                         paste(data.names[4], "complete")),
                                              name = "") +
                ggplot2::facet_wrap(. ~ z, labeller = ggplot2::labeller(z = panel_labels)) +
                ggplot2::labs(title = title,
                              x = data.names[1],
                              y = data.names[2])

            }

            # The 3D Plot:
            if(!(clms[4] %in% cont.clms) ){
              d3.plot.data$z = d3.plot.data$z + stats::runif(n = nrow(d3.plot.data),
                                                             min = -jitter,
                                                             max = jitter)
            }

            d3.plot.data$missa = paste0(d3.plot.data$missa)
            d3.plot.data$missa[d3.plot.data$missa == "0"] = "missing"
            d3.plot.data$missa[d3.plot.data$missa == "1"] = "complete"


            d3.plot.miss = plotly::plot_ly(type = "scatter3d", mode = "markers") %>%
              add_trace(
                x = d3.plot.data$x[d3.plot.data$missa == "complete"],
                y = d3.plot.data$y[d3.plot.data$missa == "complete"],
                z = d3.plot.data$z[d3.plot.data$missa == "complete"],
                marker = list(
                  color = "blue",
                  size = 2.5,
                  opacity = alpha_set,
                  line = list(width = 0)
                ),
                name = paste0(data.names[4], " is complete")
              ) %>%
              add_trace(
                x = d3.plot.data$x[d3.plot.data$missa == "missing"],
                y = d3.plot.data$y[d3.plot.data$missa == "missing"],
                z = d3.plot.data$z[d3.plot.data$missa == "missing"],
                marker = list(
                  color = "red",
                  size = 2.5,
                  opacity = 1,
                  line = list(width = 0)
                ),
                name = paste0(data.names[4], " is missing")
              ) %>%
              plotly::layout(scene = list(xaxis = list(title = data.names[1]),
                                          yaxis = list(title = data.names[2]),
                                          zaxis = list(title = data.names[3])),
                             legend = list(title = list(text = paste0("Missing ", data.names[4], "?")),
                                           y = 0.5),
                             title = list(text = title,
                                          y = 0.95))
            if(switch.back){
              title = NULL
            }


          }
        } else{
          if(length(clms) > 3){
            warning("Too many input columns, only the first three are considered.")
          }

          if(is.null(title)){
            title = "Data Visualization"
            switch.back = TRUE
          } else{
            switch.back = FALSE
          }

          data.names = names(data)[c(clms[1], clms[2], clms[3])]
          d3.plot.data = as.data.frame(data[,c(clms[1], clms[2], clms[3])])
          names(d3.plot.data) = c("x","y", "z")

          # Adding some jitter:
          if(!(clms[1] %in% cont.clms) ){
            d3.plot.data$x = d3.plot.data$x + stats::runif(n = nrow(d3.plot.data),
                                                           min = -jitter,
                                                           max = jitter)
          }
          if(!(clms[2] %in% cont.clms) ){
            d3.plot.data$y = d3.plot.data$y + stats::runif(n = nrow(d3.plot.data),
                                                           min = -jitter,
                                                           max = jitter)
          }
          if(clms[3] %in% count.clms){

            d3.plot.facet.data = d3.plot.data
            # clms[4] %in% count.clms
            panel_labels = paste0(data.names[3], ": ", unique(d3.plot.facet.data$z))
            names(panel_labels) = (paste0(unique(d3.plot.facet.data$z)))

            d3.plot.facet = ggplot2::ggplot(data = d3.plot.facet.data, ggplot2::aes(x = x, y = y)) +
              ggplot2::geom_point(alpha = alpha_set) +
              ggplot2::theme_bw() +
              ggplot2::facet_wrap(. ~ z, labeller = ggplot2::labeller(z = panel_labels)) +
              ggplot2::labs(title = title,
                            x = data.names[1],
                            y = data.names[2])
          }
          if(!(clms[3] %in% cont.clms) ){
            d3.plot.data$z = d3.plot.data$z + stats::runif(n = nrow(d3.plot.data),
                                                           min = -jitter,
                                                           max = jitter)
          }


          d3.plot = plotly::plot_ly(d3.plot.data, x =~ x, y =~ y, z =~ z, size = 0.3, opacity = alpha_set) %>%
            plotly::add_markers() %>%
            plotly::layout(title = list(text = title,
                                        y = 0.95),
                           scene = list(xaxis = list(title = data.names[1]),
                                        yaxis = list(title = data.names[2]),
                                        zaxis = list(title = data.names[3])))

          if(switch.back){
            title = NULL
          }
        }
      }
    }

    if(!is.null(mask)){
      if(is.null(clms) & is.null(error.groups)){

        if(base::identical(missing.label,"percent")){
          var.missing.mask = ggplot2::ggplot(data = nbr.missing, ggplot2::aes(x = x, y = 100*(missing/n))) + #, x = as.factor(quest))) +
            ggplot2::geom_jitter(position = ggplot2::position_jitter(seed = 101, h = 0, w = jitter)) +
            ggplot2::theme_bw() +
            ggplot2::labs(title = "Percentage of Missing Entries" ,
                          x = "",
                          y = "Percentage of Answers Missing") +
            ggplot2::geom_text(position = ggplot2::position_jitter(seed = 101, w = jitter), size = 3,
                               ggplot2::aes(label = ifelse(missing > (alpha*n) ,as.character(quest), "")), vjust = -1,
                               alpha = 0.7) +
            # ggplot2::scale_y_continuous(labels = "percent") +
            ggplot2::geom_hline(yintercept = alpha*100, color = "red", alpha = 0.5) +
            ggplot2::geom_hline(yintercept = alpha2*100, color = "black", alpha = 0.5)# +
          #  ggplot2::geom_label(x = 2, y = (alpha+0.1)*n, label = "works?" )
        } else if(base::identical(missing.label, "amount")){
          var.missing.mask = ggplot2::ggplot(data = nbr.missing, ggplot2::aes(x = x, y = missing)) + #, x = as.factor(quest))) +
            ggplot2::geom_jitter(position = ggplot2::position_jitter(seed = 101, h = 0, w = jitter)) +
            ggplot2::theme_bw() +
            ggplot2::labs(title = "Amount of Answers Missing per Variable" ,
                          x = "",
                          y = "#Missing") +
            ggplot2::geom_text(position = ggplot2::position_jitter(seed = 101, w= jitter), size = 3,
                               ggplot2::aes(label = ifelse(missing > (alpha*n) ,as.character(quest), "")), vjust = -1,
                               alpha = 0.7) +
            ggplot2::geom_hline(yintercept = alpha*n, color = "red", alpha = 0.5) +
            ggplot2::geom_hline(yintercept = alpha2*n, color = "black", alpha = 0.5)# +
        }

      } else if(is.null(clms) & !is.null(error.groups)){

        group.missing.mask.tot = ggplot2::ggplot(data = nbr.missing.in.group, ggplot2::aes(y = missing, x = name)) +
          ggplot2::geom_point() +
          ggplot2::theme_bw() +
          ggplot2::labs(title = "Number of Answers Missing per Group" ,
                        x = "Groups",
                        y = "#Missing") +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))

        if(base::identical(missing.label,"percent")){
          group.missing.mask.box = ggplot2::ggplot(data = nbr.missing, ggplot2::aes(y = 100*(missing/n), x = group)) +
            ggplot2::theme_bw() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))   +
            ggplot2::labs(title = "Distribution of Missing Answers in a Variable for each Group",
                          x = "Groups",
                          y = "Percentage of Missing Entries") +
            ggplot2::geom_boxplot()
        } else  if(base::identical(missing.label,"amount")){

          group.missing.mask.box = ggplot2::ggplot(data = nbr.missing, ggplot2::aes(y = missing, x = group)) +
            ggplot2::theme_bw() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))   +
            ggplot2::labs(title = "Distribution of Missing Answers in a Variable for each Group",
                          x = "Groups",
                          y = "#Missing") +
            ggplot2::geom_boxplot()
        }

        nbr.missing.in.group$missing_normalized = nbr.missing.in.group$missing / nbr.missing.in.group$questions

        if(base::identical(missing.label,"percent")){

          group.missing.mask.mean = ggplot2::ggplot(data = nbr.missing.in.group, ggplot2::aes(y = 100*(missing_normalized/n), x = name)) +
            ggplot2::geom_point() +
            ggplot2::theme_bw() +
            ggplot2::labs(title = "Average Percentage of Answers Missing in a Variable for each Group",
                          x = "Groups",
                          y = "Mean of the Percentage of Missingness over the Group") +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
        } else  if(base::identical(missing.label,"amount")){
          group.missing.mask.mean = ggplot2::ggplot(data = nbr.missing.in.group, ggplot2::aes(y = missing_normalized, x = name)) +
            ggplot2::geom_point() +
            ggplot2::theme_bw() +
            ggplot2::labs(title = "Average Number of Answers Missing in a Variable for each Group",
                          x = "Groups",
                          y = "#Missing / #Variables") +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
        }


      } else if(!is.null(clms)){
        # clms = clms[c(2,1,3,4)]
        if(length(clms) == 1){

          data.name = names(data)[c(clms[1])]
          miss.first = mask[,clms[1]]
          d1.plot.data = as.data.frame(matrix(c(data[,clms[1]],miss.first), ncol = 2))
          names(d1.plot.data) = c("x","missx")

          if(is.null(title)){
            title = paste0("Missing Entries of ", data.name[1])
            switch.back = TRUE
          } else{
            switch.back = FALSE
          }

          if(clms[1] %in% cont.clms){

            d1.plot.miss.mask = ggplot2::ggplot(data = d1.plot.data, ggplot2::aes(y = x, color = factor(missx))) +
              ggplot2::geom_boxplot() +
              ggplot2::theme_bw() +
              ggplot2::labs(title = title,
                            x = data.name[1],
                            y = "") +
              ggplot2::scale_color_discrete(labels = c(paste(data.name[1], " missing"),
                                                       paste(data.name[1], "complete")),
                                            name = "")

          } else if(!(clms[1] %in% cont.clms )){
            extra.df = as.data.frame(unique(d1.plot.data$x))
            extra.df = as.data.frame(extra.df[!is.na(extra.df)])
            names(extra.df) = "x"
            for(t in 1:nrow(extra.df)){
              extra.df$totalnonmissing[t] = length(which((d1.plot.data$x == extra.df$x[t]) & (d1.plot.data$missx == 1)))
            }
            for(t in 1:nrow(extra.df)){
              extra.df$totalmissing[t] = length(which((d1.plot.data$x == extra.df$x[t]) & (d1.plot.data$missx == 0)))
            }
            d1.plot.miss.mask = ggplot2::ggplot(data = d1.plot.data, ggplot2::aes(x = x, fill = factor(missx))) +
              ggplot2::geom_bar( position = "fill") +
              ggplot2::theme_bw() +
              ggplot2::labs(title = title,
                            x = data.name[1],
                            y = "") +
              ggplot2::scale_fill_discrete(labels = c(paste(data.name[1], " missing"),
                                                      paste(data.name[1], "complete")),
                                           name = "") +
              ggplot2::geom_text(inherit.aes = FALSE, data = extra.df, ggplot2::aes(x = x, y = 0.025, label = totalnonmissing)) +
              ggplot2::geom_text(inherit.aes = FALSE, data = extra.df, ggplot2::aes(x = x, y = 0.975, label = totalmissing))

          }

          if(switch.back){
            title = NULL
          }
        }

        if(length(clms) == 2){
          # clms = c(10,12,3,5)

          data.names = names(data)[c(clms[1], clms[2])]
          miss.first = mask[,c(clms[1],clms[2])]
          d2.plot.data = as.data.frame(cbind(data[,c(clms[1], clms[2])], miss.first))
          names(d2.plot.data) = c("x","y", "missx", "missy")

          if((clms[1] %in% cont.clms) & (clms[2] %in% count.clms)){

            if(only.first.mask.missingness){

              if(is.null(title)){
                title = paste0("Missing Entries of ", data.names[1], " in Relation to the Values of ", data.names[1], " and ", data.names[2])
                switch.back = TRUE
              } else{
                switch.back = FALSE
              }

              d2.plot.miss.mask = ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = x, y = y,
                                                                                    color = as.factor(missx),
                                                                                    alpha = as.factor(missx))) +
                ggplot2::geom_point( position = ggplot2::position_jitter(w = 0, h = jitter)) +
                ggplot2::theme_bw() +
                ggplot2::labs(title = title,
                              x = data.names[1],
                              y = data.names[2]) +
                ggplot2::scale_alpha_manual(values = c(1, alpha_set)) +
                ggplot2::scale_color_discrete(labels = c(paste(data.names[1], " missing"),
                                                         paste(data.names[1], "complete")),
                                              name = "")  +
                ggplot2::guides(alpha = "none",
                                color = ggplot2::guide_legend(override.aes = list(alpha = c(1, alpha_set))))

              d2.plot.data = d2.plot.data[!is.na(d2.plot.data$y),]

              d2.plot.miss.mask.box = ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = as.factor(y), y = x, color = as.factor(missx))) +
                ggplot2::geom_boxplot() +
                ggplot2::theme_bw() +
                ggplot2::labs(title = title,
                              x = data.names[2],
                              y = data.names[1]) +
                ggplot2::scale_color_discrete(labels = c(paste(data.names[2], " missing"),
                                                         paste(data.names[2], "complete")),
                                              name = "")
              if(switch.back){
                title = NULL
              }
            }  else{

              if(is.null(title)){
                title = paste0("Missing Entries of ", data.names[1]," and ", data.names[2])
                switch.back = TRUE
              } else{
                switch.back = FALSE
              }

              d2.plot.data$missboth = d2.plot.data$missy + 2*d2.plot.data$missx
              d2.plot.data$missboth = factor(d2.plot.data$missboth, levels = c(0,1,2,3) )

              d2.plot.miss.mask = ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = x, y = y, color = missboth, alpha = missboth)) +
                ggplot2::geom_point( position = ggplot2::position_jitter(w = 0, h = jitter)) +
                ggplot2::theme_bw() +
                ggplot2::labs(title = title,
                              x = data.names[1],
                              y = data.names[2]) +
                ggplot2::scale_alpha_manual(values = c(1, 1, 1, alpha_set),
                                            drop = FALSE) +
                ggplot2::scale_color_discrete(labels = c(paste("both missing"),
                                                         paste(data.names[1], "missing"),
                                                         paste(data.names[2], "missing"),
                                                         paste("both complete")),
                                              drop = FALSE,
                                              name = "") +
                ggplot2::guides(alpha = "none",
                                color = ggplot2::guide_legend(override.aes = list(alpha =  c(1,1,1,alpha_set))))

              # d2.plot.miss.mask
              d2.plot.data = d2.plot.data[!is.na(d2.plot.data$x),]
              d2.plot.data = d2.plot.data[!is.na(d2.plot.data$y),]

              d2.plot.miss.mask.box = ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = as.factor(y), y = x, color = as.factor(missboth))) +
                ggplot2::geom_boxplot() +
                ggplot2::theme_bw() +
                ggplot2::labs(title = title,
                              x = data.names[2],
                              y = data.names[1]) +
                ggplot2::scale_color_discrete(labels = c(paste("both missing"),
                                                         paste(data.names[2], "missing"),
                                                         paste(data.names[1], "missing"),
                                                         paste("both complete")),
                                              name = "")

              if(switch.back){
                title = NULL
              }

            }

          } else if((clms[1] %in% count.clms) & (clms[2] %in% cont.clms)){

            if(only.first.mask.missingness){

              if(is.null(title)){
                title = paste0("Missing Entries of ", data.names[1], " in Relation to the Values of ", data.names[1], " and ", data.names[2])
                switch.back = TRUE
              } else{
                switch.back = FALSE
              }

              d2.plot.miss.mask = ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = x, y = y, color = as.factor(missx), alpha = as.factor(missx))) +
                ggplot2::geom_point( position = ggplot2::position_jitter(h = 0, w = jitter)) +
                ggplot2::theme_bw() +
                ggplot2::labs(title = title,
                              x = data.names[1],
                              y = data.names[2]) +
                ggplot2::scale_alpha_manual(values = c(1, alpha_set)) +
                ggplot2::scale_color_discrete(labels = c(paste(data.names[1], " missing"),
                                                         paste(data.names[1], "complete")),
                                              name = "") +
                ggplot2::guides(alpha =  "none",
                                color = guide_legend(override.aes = list(alpha = c(1,alpha_set))))

              d2.plot.data = d2.plot.data[!is.na(d2.plot.data$x),]

              d2.plot.miss.mask.box = ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = as.factor(x), y = y, color = as.factor(missx))) +
                ggplot2::geom_boxplot() +
                ggplot2::theme_bw() +
                ggplot2::labs(title = title,
                              x = data.names[1],
                              y = data.names[2]) +
                ggplot2::scale_color_discrete(labels = c(paste(data.names[1], " missing"),
                                                         paste(data.names[1], "complete")),
                                              name = "")
              if(switch.back){
                title = NULL
              }
            } else{

              if(is.null(title)){
                title = paste0("Missing Entries of ", data.names[1]," and ", data.names[2])
                switch.back = TRUE
              } else{
                switch.back = FALSE
              }

              d2.plot.data$missboth = d2.plot.data$missy + 2*d2.plot.data$missx
              d2.plot.data$missboth = factor(d2.plot.data$missboth, levels = c(0,1,2,3) )

              d2.plot.miss.mask = ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = x, y = y, color = missboth, alpha = missboth)) +
                ggplot2::geom_point( position = ggplot2::position_jitter(h = 0, w = jitter)) +
                ggplot2::theme_bw() +
                ggplot2::labs(title = title,
                              x = data.names[1],
                              y = data.names[2]) +
                ggplot2::scale_alpha_manual(values = c(1, 1, 1, alpha_set),
                                            drop = FALSE) +
                ggplot2::scale_color_discrete(labels = c(paste("both missing"),
                                                         paste(data.names[1], "missing"),
                                                         paste(data.names[2], "missing"),
                                                         paste("both complete")),
                                              drop = FALSE,
                                              name = "") +
                ggplot2::guides(alpha = "none",
                                color = guide_legend(override.aes = list(alpha = c(1,1,1,alpha_set))))

              d2.plot.data = d2.plot.data[!is.na(d2.plot.data$x),]
              d2.plot.data = d2.plot.data[!is.na(d2.plot.data$y),]

              d2.plot.miss.mask.box = ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = as.factor(x), y = y, color = as.factor(missboth))) +
                ggplot2::geom_boxplot() +
                ggplot2::theme_bw() +
                ggplot2::labs(title = title,
                              x = data.names[1],
                              y = data.names[2]) +
                ggplot2::scale_color_discrete(labels = c(paste("both missing"),
                                                         paste(data.names[1], "missing"),
                                                         paste(data.names[2], "missing"),
                                                         paste("both complete")),
                                              name = "")
              if(switch.back){
                title = NULL
              }

            }

          } else  if((clms[1] %in% cont.clms) & (clms[2] %in% cont.clms)){

            if(only.first.mask.missingness){

              if(is.null(title)){
                title = paste0("Missing Entries of ", data.names[1], " in Relation to the Values of ", data.names[1], " and ", data.names[2])
                switch.back = TRUE
              } else{
                switch.back = FALSE
              }

              d2.plot.miss.mask = ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = x, y = y, color = as.factor(missx), alpha = as.factor(missx))) +
                ggplot2::geom_point( position = ggplot2::position_jitter(h = 0, w = 0)) +
                ggplot2::theme_bw() +
                ggplot2::labs(title = title,
                              x = data.names[1],
                              y = data.names[2]) +
                ggplot2::scale_alpha_manual(values = c(1, alpha_set)) +
                ggplot2::scale_color_discrete(labels = c(paste(data.names[1], " missing"),
                                                         paste(data.names[1], "complete")),
                                              name = "")+
                ggplot2::guides(alpha =  "none",
                                color = guide_legend(override.aes = list(alpha = c(1,alpha_set))))

              if(switch.back){
                title = NULL
              }

            } else{

              if(is.null(title)){
                title = paste0("Missing Entries of ", data.names[1]," and ", data.names[2])
                switch.back = TRUE
              } else{
                switch.back = FALSE
              }

              d2.plot.data$missboth = d2.plot.data$missy + 2*d2.plot.data$missx
              d2.plot.data$missboth = factor(d2.plot.data$missboth, levels = c(0,1,2,3) )

              d2.plot.miss.mask = ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = x, y = y, color = missboth, alpha = missboth)) +
                ggplot2::geom_point( position = ggplot2::position_jitter(h = 0, w = 0)) +
                ggplot2::theme_bw() +
                ggplot2::labs(title = title,
                              x = data.names[1],
                              y = data.names[2]) +
                ggplot2::scale_alpha_manual(values = c(1, 1, 1, alpha_set),
                                            drop = FALSE) +
                ggplot2::scale_color_discrete(labels = c(paste("both missing"),
                                                         paste(data.names[1], "missing"),
                                                         paste(data.names[2], "missing"),
                                                         paste("both complete")),
                                              drop = FALSE,
                                              name = "")+
                ggplot2::guides(alpha =  "none",
                                color = guide_legend(override.aes = list(alpha = c(1,1,1,alpha_set))))
              if(switch.back){
                title = NULL
              }
            }

            #  ((clms[1] %in% count.clms) & (clms[2] %in% count.clms))
          } else if((clms[1] %in% count.clms) & (clms[2] %in% count.clms)){
            # only.first.mask.missingness
            if(only.first.mask.missingness){

              if(is.null(title)){
                title = paste0("Missing Entries of ", data.names[1], " in Relation to the Values of ", data.names[1], " and ", data.names[2])
                switch.back = TRUE
              } else{
                switch.back = FALSE
              }

              d2.plot.miss.mask = ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = x, y = y, color = as.factor(missx), alpha = as.factor(missx))) +
                ggplot2::geom_point( position = ggplot2::position_jitter(h = jitter, w = jitter)) +
                ggplot2::theme_bw() +
                ggplot2::labs(title = title,
                              x = data.names[1],
                              y = data.names[2]) +
                ggplot2::scale_color_discrete(labels = c(paste(data.names[1], " missing"),
                                                         paste(data.names[1], "complete")),
                                              name = "") +
                ggplot2::guides(alpha =  "none",
                                color = guide_legend(override.aes = list(alpha = c(1,alpha_set)))) +
                ggplot2::scale_alpha_manual(values = c(1, alpha_set))
              # d2.plot.miss.mask

              panel_labels = paste0(data.names[2], ": ", unique(d2.plot.data$y))
              names(panel_labels) = (paste0(unique(d2.plot.data$y)))

              d2.plot.miss.mask.facet = ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = x, fill = as.factor(missx))) +
                ggplot2::geom_bar( position = "fill") +
                ggplot2::theme_bw() +
                ggplot2::scale_fill_discrete(labels = c(paste(data.names[1], " missing"),
                                                        paste(data.names[1], "complete")),
                                             name = "") +
                ggplot2::facet_wrap(. ~ y, labeller = ggplot2::labeller(y = panel_labels)) +
                ggplot2::labs(title = title,
                              x = data.names[1],
                              y = "")
              if(switch.back){
                title = NULL
              }
            } else{

              if(is.null(title)){
                title = paste0("Missing Entries of ", data.names[1]," and ", data.names[2])
                switch.back = TRUE
              } else{
                switch.back = FALSE
              }

              d2.plot.data$missboth = d2.plot.data$missy + 2*d2.plot.data$missx
              d2.plot.data$missboth = factor(d2.plot.data$missboth, levels = c(0,1,2,3) )

              d2.plot.miss.mask = ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = x, y = y, color = missboth, alpha = missboth)) +
                ggplot2::geom_point( position = ggplot2::position_jitter(h = jitter, w = jitter)) +
                ggplot2::theme_bw() +
                ggplot2::labs(title = title,
                              x = data.names[1],
                              y = data.names[2]) +
                ggplot2::scale_color_discrete(labels =  c(paste("both missing"),
                                                          paste(data.names[1], "missing"),
                                                          paste(data.names[2], "missing"),
                                                          paste("both complete")),
                                              drop = FALSE,
                                              name = "") +
                ggplot2::guides(alpha =  "none",
                                color = guide_legend(override.aes = list(alpha = c(1,1,1,alpha_set)))) +
                ggplot2::scale_alpha_manual(values = c(1,1,1, alpha_set),
                                            drop = FALSE)

              panel_labels = paste0(data.names[2], ": ", unique(d2.plot.data$y))
              names(panel_labels) = (paste0(unique(d2.plot.data$y)))

              d2.plot.miss.mask.facet = ggplot2::ggplot(data = d2.plot.data, ggplot2::aes(x = x, fill = missboth)) +
                ggplot2::geom_bar( position = "fill") +
                ggplot2::theme_bw() +
                ggplot2::scale_fill_discrete(labels =  c(paste("both missing"),
                                                         paste(data.names[1], "missing"),
                                                         paste(data.names[2], "missing"),
                                                         paste("both complete")),
                                             drop = FALSE,
                                             name = "") +
                ggplot2::facet_wrap(. ~ y, labeller = ggplot2::labeller(y = panel_labels)) +
                ggplot2::labs(title = title,
                              x = data.names[1],
                              y = "")
              if(switch.back){
                title = NULL
              }
            }
          }
        }

        if(length(clms) >= 3){

          # clms = c(2,5,12)

          if(length(clms) > 3){
            warning("Too many input columns, only the first three are considered.")
          }

          data.names = names(data)[c(clms[1], clms[2], clms[3])]
          miss.first = mask[,c(clms[1], clms[2], clms[3])]
          d3.plot.data = as.data.frame(cbind(data[,c(clms[1], clms[2], clms[3])], miss.first))
          names(d3.plot.data) = c("x","y", "z", "missx", "missy", "missz")


          # Adding some jitter:
          if(!(clms[1] %in% cont.clms) ){
            d3.plot.data$x = d3.plot.data$x + stats::runif(n = nrow(d3.plot.data),
                                                           min = -jitter,
                                                           max = jitter)
          }
          if(!(clms[2] %in% cont.clms) ){
            d3.plot.data$y = d3.plot.data$y + stats::runif(n = nrow(d3.plot.data),
                                                           min = -jitter,
                                                           max = jitter)
          }

          if(only.first.mask.missingness){

            if(is.null(title)){
              title = paste0("Missing Entries of ", data.names[1],
                             " in Relation to the Values of ", data.names[1], ", ",
                             data.names[2], " and ", data.names[3])
              switch.back = TRUE
            } else{
              switch.back = FALSE
            }
            # The Facet Plot:
            if(clms[3] %in% count.clms){

              panel_labels = paste0(data.names[3], ": ", unique(d3.plot.data$z))
              names(panel_labels) = (paste0(unique(d3.plot.data$z)))

              d3.plot.miss.mask.facet = ggplot2::ggplot(data = d3.plot.data, ggplot2::aes(x = x, y = y, color = as.factor(missx), alpha = as.factor(missx))) +
                ggplot2::geom_point() +
                ggplot2::theme_bw() +
                ggplot2::scale_color_discrete(labels = c(paste(data.names[1], " missing"),
                                                         paste(data.names[1], "complete")),
                                              name = "") +
                ggplot2::facet_wrap(. ~ z, labeller = ggplot2::labeller(z = panel_labels)) +
                ggplot2::labs(title = title,
                              x = data.names[1],
                              y = data.names[2]) +
                ggplot2::guides(alpha = "none",
                                color = ggplot2::guide_legend(override.aes = list(alpha = c(1,alpha_set)))) +
                ggplot2::scale_alpha_manual(values = c(1,alpha_set))
            }
            # The 3D Plot:
            if(!(clms[3] %in% cont.clms) ){
              d3.plot.data$z = d3.plot.data$z + stats::runif(n = nrow(d3.plot.data),
                                                             min = -jitter,
                                                             max = jitter)
            }

            d3.plot.data$missx = paste0(d3.plot.data$missx)
            d3.plot.data$missx[d3.plot.data$missx == "0"] = "missing"
            d3.plot.data$missx[d3.plot.data$missx == "1"] = "complete"


            d3.plot.miss.mask = plotly::plot_ly(type = "scatter3d", mode = "markers") %>%
              add_trace(
                x = d3.plot.data$x[d3.plot.data$missx == "complete"],
                y = d3.plot.data$y[d3.plot.data$missx == "complete"],
                z = d3.plot.data$z[d3.plot.data$missx == "complete"],
                marker = list(
                  color = "blue",
                  size = 2.5,
                  opacity = alpha_set,
                  line = list(width = 0)
                ),
                name = paste0(data.names[1], " is complete")
              ) %>%
              add_trace(
                x = d3.plot.data$x[d3.plot.data$missx == "missing"],
                y = d3.plot.data$y[d3.plot.data$missx == "missing"],
                z = d3.plot.data$z[d3.plot.data$missx == "missing"],
                marker = list(
                  color = "red",
                  size = 2.5,
                  opacity = 1,
                  line = list(width = 0)
                ),
                name = paste0(data.names[1], " is missing")
              ) %>%
              plotly::layout(scene = list(xaxis = list(title = data.names[1]),
                                          yaxis = list(title = data.names[2]),
                                          zaxis = list(title = data.names[3])),
                             title = list(text = title,
                                          y = 0.95),
                             legend = list(y = 0.5))

            if(switch.back){
              title = NULL
            }

          } else{

            if(is.null(title)){
              title = paste0("Missing Entries of ", data.names[1], ", ", data.names[2], " and ", data.names[3])
              switch.back = TRUE
            } else{
              switch.back = FALSE
            }

            d3.plot.data$missxyz = d3.plot.data$missz + 2*d3.plot.data$missy + 2.5*d3.plot.data$missx

            if(clms[3] %in% count.clms){
              d3.plot.data.facet = d3.plot.data
              d3.plot.data.facet$missxyz = factor(d3.plot.data.facet$missxyz, levels = c(0,1,2,2.5,3,3.5,4.5,5.5) )

              panel_labels = paste0(data.names[3], ": ", unique(d3.plot.data.facet$z))
              names(panel_labels) = (paste0(unique(d3.plot.data.facet$z)))

              d3.plot.miss.mask.facet = ggplot2::ggplot(data = d3.plot.data.facet, ggplot2::aes(x = x, y = y, color = missxyz, alpha = missxyz)) +
                ggplot2::geom_point() +
                ggplot2::theme_bw() +
                ggplot2::scale_color_discrete(labels = c(paste("all missing"),
                                                         paste(data.names[1], " and ", data.names[2], "missing"),
                                                         paste(data.names[1], " and ", data.names[3], "missing"),
                                                         paste(data.names[2], " and ", data.names[3], "missing"),
                                                         paste(data.names[1], "missing"),
                                                         paste(data.names[2], "missing"),
                                                         paste(data.names[3], "missing"),
                                                         paste("all complete")),
                                              drop = FALSE,
                                              name = "") +
                ggplot2::facet_wrap(. ~ z, labeller = ggplot2::labeller(z = panel_labels)) +
                ggplot2::labs(title = title,
                              x = data.names[1],
                              y = data.names[2]) +
                ggplot2::guides(alpha = "none",
                                color = ggplot2::guide_legend(override.aes = list(alpha = c(1,1,1,1,1,1,1,alpha_set)))) +
                ggplot2::scale_alpha_manual(values = c(1,1,1,1,1,1,1,alpha_set),
                                            drop = FALSE)
            }
            # The 3D Plot:
            if(!(clms[3] %in% cont.clms) ){
              d3.plot.data$z = d3.plot.data$z + stats::runif(n = nrow(d3.plot.data),
                                                             min = -jitter,
                                                             max = jitter)
            }

            d3.plot.data$missxyz = paste0(d3.plot.data$missxyz)
            d3.plot.data$missxyz[d3.plot.data$missxyz == "0"] = "all_missing"
            d3.plot.data$missxyz[d3.plot.data$missxyz == "1"] = "xy_missing"
            d3.plot.data$missxyz[d3.plot.data$missxyz == "2"] = "xz_missing"
            d3.plot.data$missxyz[d3.plot.data$missxyz == "2.5"] = "yz_missing"
            d3.plot.data$missxyz[d3.plot.data$missxyz == "3"] = "x_missing"
            d3.plot.data$missxyz[d3.plot.data$missxyz == "3.5"] = "y_missing"
            d3.plot.data$missxyz[d3.plot.data$missxyz == "4.5"] = "z_missing"
            d3.plot.data$missxyz[d3.plot.data$missxyz == "5.5"] = "all_complete"

            d3.plot.miss.mask = plotly::plot_ly(type = "scatter3d", mode = "markers") %>%
              add_trace(
                x = d3.plot.data$x[d3.plot.data$missxyz == "all_missing"],
                y = d3.plot.data$y[d3.plot.data$missxyz == "all_missing"],
                z = d3.plot.data$z[d3.plot.data$missxyz == "all_missing"],
                marker = list(
                  color = "#CC0033",
                  size = 2.5,
                  opacity = 1,
                  line = list(width = 0)
                ),
                name = paste0("all missing")
              ) %>%
              add_trace(
                x = d3.plot.data$x[d3.plot.data$missxyz == "xy_missing"],
                y = d3.plot.data$y[d3.plot.data$missxyz == "xy_missing"],
                z = d3.plot.data$z[d3.plot.data$missxyz == "xy_missing"],
                marker = list(
                  color = "#3333CC",
                  size = 2.5,
                  opacity = 1,
                  line = list(width = 0)
                ),
                name = paste0(data.names[1], " and ", data.names[2], " missing")
              ) %>%
              add_trace(
                x = d3.plot.data$x[d3.plot.data$missxyz == "xz_missing"],
                y = d3.plot.data$y[d3.plot.data$missxyz == "xz_missing"],
                z = d3.plot.data$z[d3.plot.data$missxyz == "xz_missing"],
                marker = list(
                  color = "#3366FF",
                  size = 2.5,
                  opacity = 1,
                  line = list(width = 0)
                ),
                name = paste0(data.names[1], " and ", data.names[3], " missing")
              ) %>%
              add_trace(
                x = d3.plot.data$x[d3.plot.data$missxyz == "yz_missing"],
                y = d3.plot.data$y[d3.plot.data$missxyz == "yz_missing"],
                z = d3.plot.data$z[d3.plot.data$missxyz == "yz_missing"],
                marker = list(
                  color = "#3399CC",
                  size = 2.5,
                  opacity = 1,
                  line = list(width = 0)
                ),
                name = paste0(data.names[2], " and ", data.names[3], " missing")
              ) %>%
              add_trace(
                x = d3.plot.data$x[d3.plot.data$missxyz == "x_missing"],
                y = d3.plot.data$y[d3.plot.data$missxyz == "x_missing"],
                z = d3.plot.data$z[d3.plot.data$missxyz == "x_missing"],
                marker = list(
                  color = "#66FFCC",
                  size = 2.5,
                  opacity = 1,
                  line = list(width = 0)
                ),
                name = paste0(data.names[1], " missing")
              ) %>%
              add_trace(
                x = d3.plot.data$x[d3.plot.data$missxyz == "y_missing"],
                y = d3.plot.data$y[d3.plot.data$missxyz == "y_missing"],
                z = d3.plot.data$z[d3.plot.data$missxyz == "y_missing"],
                marker = list(
                  color = "#00CC66",
                  size = 2.5,
                  opacity = 1,
                  line = list(width = 0)
                ),
                name = paste0(data.names[2], " missing")
              ) %>%
              add_trace(
                x = d3.plot.data$x[d3.plot.data$missxyz == "z_missing"],
                y = d3.plot.data$y[d3.plot.data$missxyz == "z_missing"],
                z = d3.plot.data$z[d3.plot.data$missxyz == "z_missing"],
                marker = list(
                  color = "#33FF66",
                  size = 2.5,
                  opacity = 1,
                  line = list(width = 0)
                ),
                name = paste0(data.names[3], " missing")
              ) %>%
              add_trace(
                x = d3.plot.data$x[d3.plot.data$missxyz == "all_complete"],
                y = d3.plot.data$y[d3.plot.data$missxyz == "all_complete"],
                z = d3.plot.data$z[d3.plot.data$missxyz == "all_complete"],
                marker = list(
                  color = "#999999",
                  size = 2.5,
                  opacity = alpha_set,
                  line = list(width = 0)
                ),
                name = paste0("all complete")
              ) %>%
              plotly::layout(scene = list(xaxis = list(title = data.names[1]),
                                          yaxis = list(title = data.names[2]),
                                          zaxis = list(title = data.names[3])),
                             title = list(text = title,
                                          y = 0.95),
                             legend = list(y = 0.5))

            if(switch.back){
              title = NULL
            }


          }





        }

      }

    }




    #######################################################################
    # Now if imputed data is not NULL
  } else{

    if(is.null(clms)){
      stop("If imputed data is not NULL the variable clms can not be NULL")
    }

    # clms = c(1,2,3,4)
    # data = as.data.frame(missing.data)

    if(length(clms) == 1){
      if(is.null(mask)){

        data.name = names(data)[c(clms[1])]
        miss.first = !is.na(data[, clms[1]])
        d1.plot.data = as.data.frame(matrix(cbind( imputed.data[,clms[1]], as.numeric(miss.first)), ncol = 2))
        names(d1.plot.data) = c("impx","imputed")

        if(is.null(title)){
          title = paste0("Values and Imputed Values of ", data.name[1])
          switch.back = TRUE
        } else{
          switch.back = FALSE
        }

        if(clms[1] %in% cont.clms){

          if(is.null(binwidth)){
            binwidth = matrixStats::iqr(d1.plot.data$impx, na.rm = TRUE)/30
            setbinback = TRUE
          } else{
            setbinback = FALSE
          }

          d1.plot.imp = ggplot2::ggplot(data = d1.plot.data[d1.plot.data$imputed == 1,], ggplot2::aes(x = impx, color = "No")) +
            ggplot2::geom_histogram(binwidth = binwidth) +
            ggplot2::geom_histogram(data = d1.plot.data[d1.plot.data$imputed == 0,], ggplot2::aes(x = impx, color = "Yes"), alpha = 0 ,
                                    binwidth = binwidth) +
            ggplot2::theme_bw() +
            ggplot2::labs(title = title,
                          x = data.name[1],
                          y = "") +
            ggplot2::scale_color_manual(name = "Imputed?",
                                        breaks = c("Yes", "No"),
                                        values = c("red","grey")) +
            ggplot2::guides(color = guide_legend(override.aes = list(alpha = c(0,1))))

          if(setbinback){
            binwidth = NULL
          }

        } else if(!(clms[1] %in% cont.clms )){

          d1.plot.imp = ggplot2::ggplot(data = d1.plot.data, ggplot2::aes(x = as.factor(impx), fill = as.factor(imputed)))+
            ggplot2::geom_bar(position = "dodge") +
            ggplot2::theme_bw() +
            ggplot2::labs(title = title,
                          x = data.name[1],
                          y = "") +
            ggplot2::scale_fill_discrete(labels = c("Yes","No"),
                                         name = "Imputed?")

        }
        if(switch.back){
          title = NULL
        }

      } else{

        data.name = names(data)[c(clms[1])]
        miss.first = mask[,clms[1]] == 0
        d1.plot.data = as.data.frame(cbind(data[miss.first,clms[1]], imputed.data[miss.first,clms[1]]))
        names(d1.plot.data) = c("x","impx")
        lofdf = nrow(d1.plot.data)

        if(is.null(title)){
          title = paste0("Missing Values vs. Imputed Values of ", data.name[1])
          switch.back = TRUE
        } else{
          switch.back = FALSE
        }

        if(!is.null(multiple.imputations)){
          if(is.null(imp.names)){
            imp.names = paste0("Imputation ",1:(length(multiple.imputations) + 1))
          }
          d1.plot.data$imputation = rep(imp.names[1], times = lofdf)
          for(t in 1:length(multiple.imputations)){
            # t = 1
            get.imp = as.data.frame(cbind(data[miss.first,clms[1]], multiple.imputations[[t]][miss.first,clms[1]]))
            get.imp$imputation = rep(imp.names[t+1], times = lofdf)
            names(get.imp) = c("x","impx", "imputation")
            d1.plot.data = rbind.data.frame(d1.plot.data, get.imp)
          }
          if(!(clms[1] %in% cont.clms)){
            d1.plot.data$x = d1.plot.data$x + stats::runif(n = nrow(d1.plot.data),
                                                           min = -jitter,
                                                           max = jitter)
            d1.plot.data$impx = d1.plot.data$impx + stats::runif(n = nrow(d1.plot.data),
                                                                 min = -jitter,
                                                                 max = jitter)
          }
          d1.plot.imp = ggplot2::ggplot(data = d1.plot.data, ggplot2::aes(x = x, y = impx, color = imputation)) +
            ggplot2::geom_point(alpha = alpha_set) +
            ggplot2::theme_bw() +
            ggplot2::labs(title = title,
                          x = paste0("Missing Value of ",data.name[1]),
                          y = paste0("Imputed Value of ", data.name[1])) +
            ggplot2::geom_abline(intercept = 0, slope = 1)
        } else{

          if(clms[1] %in% cont.clms){

            d1.plot.imp = ggplot2::ggplot(data = d1.plot.data, ggplot2::aes(x = x, y = impx)) +
              ggplot2::geom_point(alpha = alpha_set) +
              ggplot2::theme_bw() +
              ggplot2::labs(title = title,
                            x = paste0("Missing Value of ",data.name[1]),
                            y = paste0("Imputed Value of ", data.name[1])) +
              ggplot2::geom_abline(intercept = 0, slope = 1)


          } else if(!(clms[1] %in% cont.clms )){

            extra.df = as.data.frame(unique(d1.plot.data$x))
            min_val = min(c(unique(d1.plot.data$x[!is.na(d1.plot.data$x)]), unique(d1.plot.data$impx[!is.na(d1.plot.data$impx)])))
            max_val = max(c(unique(d1.plot.data$x[!is.na(d1.plot.data$x)]), unique(d1.plot.data$impx[!is.na(d1.plot.data$impx)])))
            # (min_val != round(min_val)) | (max_val != round(max_val))
            if((min_val != round(min_val)) | (max_val != round(max_val))  ){
              stop("Discrete Values are not integers.")
            }
            tile.df = data.frame("x" = rep(min_val:max_val, each = (max_val - min_val + 1)),
                                 "impx" = rep(min_val:max_val, times = (max_val - min_val + 1)))
            tile.df$value = 0
            tile.df$diag = 0

            for(t in 1:nrow(tile.df)){
              tile.df$value[t] = length(which((d1.plot.data$x == tile.df$x[t]) & (d1.plot.data$impx == tile.df$impx[t])))
              if(tile.df$x[t] == tile.df$impx[t]){
                tile.df$diag[t] = 1
              }
            }

            d1.plot.imp = ggplot2::ggplot(data = tile.df, ggplot2::aes(x = x, y = impx, fill = value)) +
              ggplot2::geom_tile() +
              ggplot2::theme_bw() +
              ggplot2::labs(title =  title,
                            x = paste0("Missing Value of ",data.name[1]),
                            y = paste0("Imputed Value of ", data.name[1])) +
              ggplot2::geom_tile(data = tile.df[(tile.df$diag == 1),], aes(color = as.factor(diag)), size = 1) +
              ggplot2::scale_color_manual(guide = "none", values = c('1' = "red")) +
              ggplot2::geom_text(ggplot2::aes(label = value), size = 5, col = "white")
            #  d1.plot.imp
          }
        }
        if(switch.back){
          title = NULL
        }
      }

    }

    if(length(clms) == 2){

      if(is.null(mask)){

        data.names = names(data)[c(clms[1], clms[2])]
        miss.first = is.na(data[,clms[1]])
        miss.second = is.na(data[,clms[2]])
        miss.both = miss.first*1 + miss.second*2

        d2.plot.data = as.data.frame(cbind(imputed.data[ ,c(clms[1], clms[2])], miss.both))
        names(d2.plot.data) = c("x","y", "imp")

        if(is.null(title)){
          title = paste0("Values and Imputed Values of ", data.names[1], "and", data.names[2])
          switch.back = TRUE
        } else{
          switch.back = FALSE
        }

        if(!(clms[1] %in% cont.clms)){
          d2.plot.data$x = d2.plot.data$x + stats::runif(n = nrow(d2.plot.data),
                                                         min = -jitter,
                                                         max = jitter)
        }
        if(!(clms[2] %in% cont.clms)){
          d2.plot.data$y = d2.plot.data$y + stats::runif(n = nrow(d2.plot.data),
                                                         min = -jitter,
                                                         max = jitter)
        }


        d2.plot.imp = ggplot2::ggplot() +
          ggplot2::geom_point( data = d2.plot.data, ggplot2::aes(x = x, y = y,
                                                                 color = factor(imp,
                                                                                levels = c("0","1","2","3")),
                                                                 alpha = factor(imp,
                                                                                levels = c("0","1","2","3")))) +
          ggplot2::theme_bw() +
          ggplot2::labs(title = title,
                        x = data.names[1],
                        y = data.names[2]) +
          ggplot2::scale_color_discrete(name = "Imputed?",
                                        labels = c(paste0("No"),
                                                   paste0("Yes, ", data.names[1]),
                                                   paste0("Yes, ", data.names[2]),
                                                   "Yes, both"),
                                        drop = FALSE) +
          ggplot2::guides(alpha = "none",
                          color = ggplot2::guide_legend(override.aes = list(alpha = c(alpha_set, 1,1,1)))) +
          ggplot2::scale_alpha_manual(values = c(alpha_set,1,1,1))

        if(switch.back){
          title = NULL
        }

      } else{

        # clms = c(13,14,15,16)
        # sum(as.numeric(miss))
        data.names = names(data)[c(clms[1], clms[2])]
        miss.first = (mask[,clms[1]] == 0 )
        miss.second = (mask[,clms[2]] == 0 )
        miss = (mask[,clms[1]] == 0 ) | (mask[, clms[2]] == 0)
        already.miss = (is.na(data[,clms[1]])) | (is.na(data[, clms[2]]))
        miss = miss & !(already.miss)
        d2.plot.data.exist = as.data.frame(cbind(data[!miss ,c(clms[1], clms[2])]))
        names(d2.plot.data.exist) = c("x","y")
        d2.plot.data.miss = as.data.frame(cbind(data[miss ,c(clms[1], clms[2])], as.numeric(miss.first[miss]), as.numeric(miss.second[miss])))
        names(d2.plot.data.miss) = c("x","y", "missx", "missy")
        d2.plot.data.imp = as.data.frame(cbind(imputed.data[miss ,c(clms[1], clms[2])], as.numeric(miss.first[miss]), as.numeric(miss.second[miss])))
        names(d2.plot.data.imp) = c("impx","impy", "missx", "missy")

        if(is.null(title)){
          title = paste0(data.names[1], " vs. ",data.names[2], " with Imputed and Missing Values")
          switch.back = TRUE
        } else{
          switch.back = FALSE
        }

        if((clms[1] %in% count.clms) & (clms[2] %in% count.clms)){

          min_val_1 = min(c(unique(d2.plot.data.miss$x[!is.na(d2.plot.data.miss$x)]), unique(d2.plot.data.imp$impx[!is.na(d2.plot.data.miss$impx)])))
          max_val_1 = max(c(unique(d2.plot.data.miss$x[!is.na(d2.plot.data.miss$x)]), unique(d2.plot.data.imp$impx[!is.na(d2.plot.data.miss$impx)])))

          min_val_2 = min(c(unique(d2.plot.data.miss$y[!is.na(d2.plot.data.miss$y)]), unique(d2.plot.data.imp$impy[!is.na(d2.plot.data.miss$impy)])))
          max_val_2 = max(c(unique(d2.plot.data.miss$y[!is.na(d2.plot.data.miss$y)]), unique(d2.plot.data.imp$impy[!is.na(d2.plot.data.miss$impy)])))

          if(((min_val_1 != round(min_val_1)) | (max_val_1 != round(max_val_1))) | ((min_val_2 != round(min_val_2)) | (max_val_2 != round(max_val_2))) ){
            stop("Discrete Values are not integers.")
          }
          tile.df = data.frame("x" = rep(min_val_1:max_val_1, each = (max_val_2 - min_val_2 + 1)),
                               "y" = rep(min_val_2:max_val_2, times = (max_val_1 - min_val_1 + 1)))
          vtk.vls = (max_val_2 - min_val_2) + 1
          tile.df$value = 0
          tile.df$valuemiss = 0
          tile.df$valueimp = 0


          for(t in 1:nrow(tile.df)){
            tile.df$valuemiss[t] = length(which((d2.plot.data.miss$x == tile.df$x[t]) & (d2.plot.data.miss$y == tile.df$y[t])))
            tile.df$valueimp[t] = length(which((d2.plot.data.imp$impx == tile.df$x[t]) & (d2.plot.data.imp$impy == tile.df$y[t])))
            tile.df$value[t] = (tile.df$valueimp[t] - tile.df$valuemiss[t]) / tile.df$valuemiss[t]
          }


          d2.plot.imp.tile = ggplot2::ggplot(data = tile.df, ggplot2::aes(x = x, y = y, fill = value)) +
            ggplot2::geom_tile() +
            ggplot2::theme_bw() +
            ggplot2::theme(legend.position = "right", legend.title = element_text(vjust = 0.25 , angle = 270)) +
            ggplot2::labs(title =  title,
                          x = data.names[1],
                          y = data.names[2],
                          fill = "Relative Change") +
            ggplot2::scale_fill_gradient2() +
            ggplot2::geom_text(ggplot2::aes(label = valuemiss), size = ((0.25*(10 - vtk.vls)) + 3), col = "black",
                               vjust = 1.2) +
            ggplot2::geom_text(ggplot2::aes(label = valueimp), size = ((0.25*(10 - vtk.vls)) + 3), col = "red",
                               vjust = -0.5)

        }

        if(!(clms[1] %in% cont.clms)){
          d2.plot.data.exist$x = d2.plot.data.exist$x + stats::runif(n = nrow(d2.plot.data.exist),
                                                                     min = -jitter,
                                                                     max = jitter)
          d2.plot.data.miss$x = d2.plot.data.miss$x + stats::runif(n = nrow(d2.plot.data.miss),
                                                                   min = -jitter,
                                                                   max = jitter)
          d2.plot.data.imp$impx = d2.plot.data.imp$impx + stats::runif(n = nrow(d2.plot.data.imp),
                                                                       min = -jitter,
                                                                       max = jitter)
        }
        if(!(clms[2] %in% cont.clms)){
          d2.plot.data.exist$y = d2.plot.data.exist$y + stats::runif(n = nrow(d2.plot.data.exist),
                                                                     min = -jitter,
                                                                     max = jitter)
          d2.plot.data.miss$y = d2.plot.data.miss$y + stats::runif(n = nrow(d2.plot.data.miss),
                                                                   min = -jitter,
                                                                   max = jitter)
          d2.plot.data.imp$impy = d2.plot.data.imp$impy + stats::runif(n = nrow(d2.plot.data.imp),
                                                                       min = -jitter,
                                                                       max = jitter)
        }

        d2.plot.data.missimp = data.frame("x" = c(d2.plot.data.miss$x, d2.plot.data.imp$impx),
                                          "y" = c(d2.plot.data.miss$y, d2.plot.data.imp$impy),
                                          "grp" = rep(1:nrow(d2.plot.data.miss), times = 2))

        d2.plot.imp = ggplot2::ggplot() +
          ggplot2::geom_point( data = d2.plot.data.exist,ggplot2::aes(x = x, y = y, color =  "Complete"), alpha = alpha_set ,shape = 16) +
          ggplot2::theme_bw() +
          ggplot2::labs(title = title,
                        x = data.names[1],
                        y = data.names[2]) +
          ggplot2::geom_point( data = d2.plot.data.miss ,ggplot2::aes(x = x, y = y, color =  "Missing"), alpha = 1, shape = 16) +
          ggplot2::geom_point( data = d2.plot.data.imp ,ggplot2::aes(x = impx, y = impy, color =  "Imputed"), alpha = 1, shape = 4 ) +
          ggplot2::geom_line(data = d2.plot.data.missimp, ggplot2::aes(x = x, y = y, group = grp), color = "#999999", alpha = alpha_line,
                             linetype = "twodash") +
          ggplot2::scale_color_manual(name = "Data Points",
                                      breaks = c("Complete",
                                                 "Missing",
                                                 "Imputed"),
                                      values = c("#0072B2",
                                                 "#D55E00",
                                                 "#D55E00")) +
          ggplot2::guides(color = guide_legend(override.aes = list(shape = c(16,16,4),
                                                                   alpha = c(alpha_set ,1 , 1))))



        if(switch.back){
          title = NULL
        }
      }
    }

    if(length(clms) >= 3){
      # clms = c(2,5,12)

      if(length(clms) > 3){
        warning("Too many input columns, only the first three are considered.")
      }

      if(is.null(mask)){

        data.names = names(data)[c(clms[1], clms[2], clms[3])]
        miss.first = is.na(data[,clms[1]])
        miss.second = is.na(data[,clms[2]])
        miss.third = is.na(data[,clms[3]])
        miss =  miss.first*1 + miss.second*2 + miss.third*4

        d3.plot.data = as.data.frame(cbind(imputed.data[ ,c(clms[1], clms[2],clms[3])], miss))
        names(d3.plot.data) = c("x","y","z", "imp")

        if(!(clms[1] %in% cont.clms)){
          d3.plot.data$x = d3.plot.data$x + stats::runif(n = nrow(d3.plot.data),
                                                         min = -jitter,
                                                         max = jitter)
        }
        if(!(clms[2] %in% cont.clms)){
          d3.plot.data$y = d3.plot.data$y + stats::runif(n = nrow(d3.plot.data),
                                                         min = -jitter,
                                                         max = jitter)
        }
        if(!(clms[3] %in% cont.clms)){
          d3.plot.data$z = d3.plot.data$z + stats::runif(n = nrow(d3.plot.data),
                                                         min = -jitter,
                                                         max = jitter)
        }

        if(is.null(title)){
          title = paste0("Values and Imputed Values of ", data.names[1], ", ", data.names[2], " and ", data.names[3])
          switch.back = TRUE
        } else{
          switch.back = FALSE
        }

        d3.plot.imp = plotly::plot_ly(type = "scatter3d", mode = "markers") %>%
          add_trace(
            x = d3.plot.data$x[d3.plot.data$imp == 0],
            y = d3.plot.data$y[d3.plot.data$imp == 0],
            z = d3.plot.data$z[d3.plot.data$imp == 0],
            marker = list(
              color = "#999999",
              size = 2.5,
              opacity = alpha_set,
              line = list(width = 0)
            ),
            name = paste0("Not imputed")
          )  %>%
          add_trace(
            x = d3.plot.data$x[d3.plot.data$imp == 1],
            y = d3.plot.data$y[d3.plot.data$imp == 1],
            z = d3.plot.data$z[d3.plot.data$imp == 1],
            marker = list(
              color = "#33FF66",
              size = 2.5,
              opacity = 1,
              line = list(width = 0)
            ),
            name = paste0(data.names[1], " imputed")
          )  %>%
          add_trace(
            x = d3.plot.data$x[d3.plot.data$imp == 2],
            y = d3.plot.data$y[d3.plot.data$imp == 2],
            z = d3.plot.data$z[d3.plot.data$imp == 2],
            marker = list(
              color = "#00CC66",
              size = 2.5,
              opacity = 1,
              line = list(width = 0)
            ),
            name = paste0(data.names[2], " imputed")
          )  %>%
          add_trace(
            x = d3.plot.data$x[d3.plot.data$imp == 4],
            y = d3.plot.data$y[d3.plot.data$imp == 4],
            z = d3.plot.data$z[d3.plot.data$imp == 4],
            marker = list(
              color = "#66FFCC",
              size = 2.5,
              opacity = 1,
              line = list(width = 0)
            ),
            name = paste0(data.names[3], " imputed")
          )  %>%
          add_trace(
            x = d3.plot.data$x[d3.plot.data$imp == 3],
            y = d3.plot.data$y[d3.plot.data$imp == 3],
            z = d3.plot.data$z[d3.plot.data$imp == 3],
            marker = list(
              color = "#3333CC",
              size = 2.5,
              opacity = 1,
              line = list(width = 0)
            ),
            name = paste0(data.names[1], " and ", data.names[2], " imputed")
          )  %>%
          add_trace(
            x = d3.plot.data$x[d3.plot.data$imp == 5],
            y = d3.plot.data$y[d3.plot.data$imp == 5],
            z = d3.plot.data$z[d3.plot.data$imp == 5],
            marker = list(
              color = "#3366FF",
              size = 2.5,
              opacity = 1,
              line = list(width = 0)
            ),
            name = paste0(data.names[1], " and ", data.names[3], " imputed")
          )  %>%
          add_trace(
            x = d3.plot.data$x[d3.plot.data$imp == 6],
            y = d3.plot.data$y[d3.plot.data$imp == 6],
            z = d3.plot.data$z[d3.plot.data$imp == 6],
            marker = list(
              color = "#3399CC",
              size = 2.5,
              opacity = 1,
              line = list(width = 0)
            ),
            name = paste0(data.names[2], " and ", data.names[3], " imputed")
          )  %>%
          add_trace(
            x = d3.plot.data$x[d3.plot.data$imp == 7],
            y = d3.plot.data$y[d3.plot.data$imp == 7],
            z = d3.plot.data$z[d3.plot.data$imp == 7],
            marker = list(
              color = "#CC0033",
              size = 2.5,
              opacity = 1,
              line = list(width = 0)
            ),
            name = paste0("All imputed")
          )  %>%
          plotly::layout(scene = list(xaxis = list(title = data.names[1]),
                                      yaxis = list(title = data.names[2]),
                                      zaxis = list(title = data.names[3])),
                         title = list(text = title,
                                      y = 0.95),
                         legend = list(y = 0.5))

        if(switch.back){
          title = NULL
        }

      } else{


        data.names = names(data)[c(clms[1], clms[2], clms[3])]
        miss.first = (mask[,clms[1]] == 0 )
        miss.second = (mask[,clms[2]] == 0 )
        miss = ((mask[,clms[1]] == 0 ) | (mask[, clms[2]] == 0)) | (mask[,clms[3]] == 0 )
        already.miss = ((is.na(data[,clms[1]])) | (is.na(data[, clms[2]]))) | is.na(data[,clms[3]])
        miss = miss & !(already.miss)
        d3.plot.data.exist = as.data.frame(cbind(data[!miss ,c(clms[1], clms[2],clms[3])]))
        names(d3.plot.data.exist) = c("x","y","z")
        d3.plot.data.miss = as.data.frame(cbind(data[miss ,c(clms[1], clms[2],clms[3])], as.numeric(miss.first[miss]), as.numeric(miss.second[miss])))
        names(d3.plot.data.miss) = c("x","y", "z", "missx", "missy")
        d3.plot.data.imp = as.data.frame(cbind(imputed.data[miss ,c(clms[1], clms[2],clms[3])], as.numeric(miss.first[miss]), as.numeric(miss.second[miss])))
        names(d3.plot.data.imp) = c("impx","impy", "impz", "missx", "missy")

        if(is.null(title)){
          title = paste0("Imputed vs. Missing values of ", data.names[1], ", ", data.names[2], " and ", data.names[3])
          switch.back = TRUE
        } else{
          switch.back = FALSE
        }


        if(!(clms[1] %in% cont.clms)){
          d3.plot.data.exist$x = d3.plot.data.exist$x + stats::runif(n = nrow(d3.plot.data.exist),
                                                                     min = -jitter,
                                                                     max = jitter)
          d3.plot.data.miss$x = d3.plot.data.miss$x + stats::runif(n = nrow(d3.plot.data.miss),
                                                                   min = -jitter,
                                                                   max = jitter)
          d3.plot.data.imp$impx = d3.plot.data.imp$impx + stats::runif(n = nrow(d3.plot.data.imp),
                                                                       min = -jitter,
                                                                       max = jitter)
        }
        if(!(clms[2] %in% cont.clms)){
          d3.plot.data.exist$y = d3.plot.data.exist$y + stats::runif(n = nrow(d3.plot.data.exist),
                                                                     min = -jitter,
                                                                     max = jitter)
          d3.plot.data.miss$y = d3.plot.data.miss$y + stats::runif(n = nrow(d3.plot.data.miss),
                                                                   min = -jitter,
                                                                   max = jitter)
          d3.plot.data.imp$impy = d3.plot.data.imp$impy + stats::runif(n = nrow(d3.plot.data.imp),
                                                                       min = -jitter,
                                                                       max = jitter)
        }
        if(!(clms[3] %in% cont.clms)){
          d3.plot.data.exist$z = d3.plot.data.exist$z + stats::runif(n = nrow(d3.plot.data.exist),
                                                                     min = -jitter,
                                                                     max = jitter)
          d3.plot.data.miss$z = d3.plot.data.miss$z + stats::runif(n = nrow(d3.plot.data.miss),
                                                                   min = -jitter,
                                                                   max = jitter)
          d3.plot.data.imp$impz = d3.plot.data.imp$impz + stats::runif(n = nrow(d3.plot.data.imp),
                                                                       min = -jitter,
                                                                       max = jitter)
        }

        d3.plot.data.missimp = data.frame("x" = c(d3.plot.data.miss$x, d3.plot.data.imp$impx),
                                          "y" = c(d3.plot.data.miss$y, d3.plot.data.imp$impy),
                                          "z" = c(d3.plot.data.miss$z, d3.plot.data.imp$impz),
                                          "grp" = rep(1:nrow(d3.plot.data.miss), times = 2))

        d3.plot.imp = plotly::plot_ly(type = "scatter3d", mode = "markers") %>%
          add_trace(
            x = d3.plot.data.exist$x,
            y = d3.plot.data.exist$y,
            z = d3.plot.data.exist$z,
            marker = list(
              color = "blue",
              size = 2.5,
              opacity = alpha_set,
              line = list(width = 0)
            ),
            name = paste0("Complete datapoint")
          ) %>%
          add_trace(
            x = d3.plot.data.miss$x,
            y = d3.plot.data.miss$y,
            z = d3.plot.data.miss$z,
            marker = list(
              color = "red",
              size = 2.5,
              opacity = 1,
              line = list(width = 0)
            ),
            name = paste0("Datapoint with missing entry")
          ) %>%
          add_trace(
            x = d3.plot.data.imp$impx,
            y = d3.plot.data.imp$impy,
            z = d3.plot.data.imp$impz,
            marker = list(
              color = "goldenrod",
              size = 2.5,
              opacity = 1,
              line = list(width = 0)
            ),
            name = paste0("Imputed datapoint")
          ) %>%
          add_trace(
            x = d3.plot.data.missimp$x,
            y = d3.plot.data.missimp$y,
            z = d3.plot.data.missimp$z,
            split = d3.plot.data.missimp$grp,
            mode = "lines",
            name = "lines",
            opacity = alpha_line,
            line = list(dash = "dash",
                        color = "grey",
                        width = 3),
            showlegend = FALSE
          ) %>%
          plotly::layout(scene = list(xaxis = list(title = data.names[1]),
                                      yaxis = list(title = data.names[2]),
                                      zaxis = list(title = data.names[3])),
                         title = list(text = title,
                                      y = 0.95),
                         legend = list(y = 0.5))# add_trace(


        if(switch.back){
          title = NULL
        }
      }

    }


  }


  # We need to collect the plots

  if(is.null(imputed.data)){
    if(is.null(mask)){
      if(is.null(clms)){
        if(is.null(error.groups)){
          plots = list("variable" = var.missing,
                       "hist" = missing.hist)
        } else{
          plots = list("total" = group.missing.tot,
                       "box" = group.missing.box,
                       "mean" = group.missing.mean,
                       "hist" = missing.hist.group)
        }
      } else{
        if(length(clms) == 1){
          plots = d1.plot
        } else if(length(clms) == 2){
          if(first.clms.entry.missing){
            plots = d1.plot.miss
          } else{
            if((clms[1] %in% count.clms) & (clms[2] %in% count.clms)){
              plots = list("scatter" = d2.plot,
                           "tile" = d2.plot.tile)
            } else{
              plots = d2.plot
            }
          }
        } else if(length(clms) >= 3){
          if(first.clms.entry.missing){
            if(length(clms) == 3){
              if((clms[2] %in% count.clms) & (clms[3] %in% count.clms)){
                plots = list("scatter" = d2.plot.miss,
                             "facets" = d2.plot.miss.facet)
              } else if(((clms[2] %in% count.clms) & (clms[3] %in% cont.clms)) | ((clms[3] %in% count.clms) & (clms[2] %in% cont.clms))){
                plots = list("scatter" = d2.plot.miss,
                             "box" = d2.plot.miss.box)
              } else{
                plots = d2.plot.miss
              }
            } else{
              if(clms[4] %in% count.clms){
                plots = list("scatter" = d3.plot.miss,
                             "facets" = d3.plot.miss.facet)
              } else{
                plots = d3.plot.miss
              }
            }
          } else{
            if(clms[3] %in% count.clms){
              plots = list("scatter" = d3.plot,
                           "facets" = d3.plot.facet)
            } else{
              plots = d3.plot
            }
          }
        }
      }
    } else{
      if(is.null(clms)){
        if(is.null(error.groups)){
          plots = var.missing.mask
        } else{
          plots = list("total" = group.missing.mask.tot,
                       "box" = group.missing.mask.box,
                       "mean" = group.missing.mask.mean)}
      } else{

        if(length(clms) == 1){
          plots = d1.plot.miss.mask

        } else if(length(clms) == 2){

          if((clms[1] %in% count.clms) & (clms[2] %in% count.clms)){
            plots = list("scatter" = d2.plot.miss.mask,
                         "facets" = d2.plot.miss.mask.facet)
          } else if(((clms[1] %in% count.clms) & (clms[2] %in% cont.clms)) | ((clms[2] %in% count.clms) & (clms[1] %in% cont.clms))){
            plots = list("scatter" = d2.plot.miss.mask,
                         "box" = d2.plot.miss.mask.box)
          } else{
            plots = d2.plot.miss.mask
          }
        } else{
          if(clms[3] %in% count.clms){
            plots = list("scatter" = d3.plot.miss.mask,
                         "facets" = d3.plot.miss.mask.facet)
          } else{
            plots = d3.plot.miss.mask
          }
        }
      }
    }
  } else{
    if(length(clms) == 1){
      plots = d1.plot.imp
    } else if(length(clms) == 2){
      if(((clms[1] %in% count.clms) & (clms[2] %in% count.clms) )& !is.null(mask) ){
        plots = list("scatter" = d2.plot.imp,
                     "tile" = d2.plot.imp.tile)
      } else{
        plots = d2.plot.imp
      }
    } else{
      plots = d3.plot.imp
    }
  }


  # plots$facet
  return(plots)
}




