
#'
#' Benchmarking Imputation Methods on Simulated Data
#'
#' @description Simulation Study to compare imputation methods on simulated data and missingness pattern according to the
#' benchmarking protocol of RMTBM22.
#'
#' @param methods A list of functions, taking as an input a numeric matrix with missing entries via an argument `data` and a list of
#' parameters via an argument `args`. Default is `NULL`, i.e. no additional methods added to the benchmarking.
#' @param training A list of functions - which need to be of the same length as the `methods` list -, that take as an input
#' a data matrix with missing entries via an argument `data` and outputs the parameters for the respective method via as a list `args`.
#' Default is `NULL`.
#' @param data_parameters A (named) list of named lists of data parameters, for details see below and the accompanying vignette.
#' Default is `NULL`.
#' @param missingness_parameters A (named) list of named lists of parameters defining the way missingness scenarios are drawn, for details see below
#' and the accompanying vignette. Default is `NULL`.
#' @param repetitions Integer determining how often an additional missingness pattern according to one entry of `scenarios` or  `missingness_parameters`
#' is drawn. Default is `5`.
#' @param default_methods `NULL` or vector with four possible character string entries: `"knn"`, `"MICE"`, `"missF"` and `"softImpute"`. Determines
#' which default imputation method is included in the comparison. Default is \code{c("knn","MICE","missF","softImpute")}.
#' @param baseline_method Character string, either `"median"` (default) or `"mean"`. Defining the baseline imputation method.
#' @param error_measure Character string that must be `"l2"` (up to now only error measure supported). Default is `"l2"`.
#' @param scaling_robust Positive numeric, robustness parameter avoiding division by close to zero when the columns are scaled by the standard deviation
#' when computing the imputation error. Default is `0.01`.
#' @param retrain_always Boolean, should parameters for each imputation method be learned for every run or only every scenario/parameter choice?
#' Default is \code{FALSE}.
#' @param verbose Boolean, determines if an output to the console is made each time a run is finished. Default is \code{FALSE}.
#' @param seed Integer or `NULL`, sets a seed. Default is \code{NULL}.
#' @param max_time The maximal amount of seconds an imputation method is allowed to take. Default is \code{2592000}, i.e. 30 days.
#' @param output_data Boolean, should additional data be collected, i.e. drawn missingness pattern? Default is `TRUE`.
#'
#' @details  This function benchmarks imputation methods on simulated data. A detailed description of the functionality and
#' arguments of `simulation_ImputeBench()` can be found in the accompanying vignette. For the exact definition of the benchmarking
#' protocol and the data and missingness pattern simulation protocols we refer to the paper RMTBM22, see below.
#' In the following a brief overview over the arguments and example inputs.
#'
#' ## Customizing the Compared Imputation Methods
#'
#' To add to the default imputation methods user specified ones `simulation_ImputeBench()` provides the argument `methods` which
#' expects a list of functions to be passed. These functions have to have exactly two inputs: `data` (a numeric matrix with missing
#' entries denoted by `NA`) and `args` (a list with parameters for the imputation method, this can be left `NULL`). The output
#' needs to be a numeric matrix without missing entries (by default still missing entries are filled by the baseline imputation
#' method). For details see the accompanying vignette. Customizing names of the additional methods is done by naming list entries.
#'
#' Example of `methods`:
#' ```
#' methods = list("Random Imputation" =  function(data,
#'                          args = NULL){
#'     clms.with.missing.entries =
#'        which(matrixStats::colSums2(matrix(as.numeric(is.na(data)), nrow = nrow(data))) != 0)
#'     imputed.matrix = data
#'     for(t in clms.with.missing.entries){
#'        imputed.matrix[is.na(data[,t]),t] =
#'        sample(data[!is.na(data[,t]),t], replace = TRUE, size = sum(as.numeric(is.na(data[,t]))))
#'     }
#'     return(imputed.matrix)
#'   },
#'                 "User Candidate 2" = function(data, args = ...){ ... return(imputed.matrix)})
#' ```
#'
#' Via the argument `training` parameter training functions can be passed to `simulation_ImputeBench()` (note, that this is necessary
#' if methods are added via `methods` which do not except `args = NULL` as an input). The argument `training` expects as an
#' input similar to `methods` a list of functions, which take as an input only one argument `data` and which output the argumnet
#' `args`, a list of parameter choices. The output of functions included in `training` must be compatible with the input `args`
#' of the index-wise corresponding functions in `methods`.
#'
#' Example of `training`:
#' ```
#' methods = list(  function(data){
#'     args = NULL
#'     return(args)
#'    },
#'                  function(data){ ... return(args)})
#' ```
#'
#' By default the parameter `default_methods` is set to `c("knn", "MICE", "missF", "softImpute")` including all four supported
#' default imputation methods in the benchmarking. To remove the default parameters remove them from the vector and supply the
#' resulting vector to `default_methods` (`NULL` in case all default methods should be removed).
#'
#' By default parameter of the imputation methods are trained (default training is supported for k-nn imputation (`k`) and soft
#' impute (`lambda`)) once every scenario by the default argument `retrain_always = FALSE`. To have parameters being retrained
#' for every run set `retrain_always = TRUE`.
#'
#' The baseline imputation method is median imputation set via the default `baseline_method = "median"`. To switch to the second
#' supported baseline imputation method pass `baseline_method = "mean"` for mean imputation.
#'
#' ## Customizing the Data Simulation Protocol
#'
#' The parameter `data_parameters` expects a named list of named lists as an entry. Each list entry represents a choice of data
#' parameters for the data simulation protocol and must have a specific form as detailed in the accompanying vignette, find an
#' example also further below. Note, that missing entries in a list of data parameters will lead to default choices (not to an
#' error(!)). For the default choices we refer again to the vignette.
#'
#' Example for `data_parameters`:
#'
#' ```
#' data_parameters = list("Default" =  list(data.size = 100,
#'                                          data.type = c(5,5,5,5,5,5),
#'                                          data.shape = list(gauss.mean = c(0,1),
#'                                                            gauss.sd = c(1,2),
#'                                                            pois.lambda = c(1),
#'                                                            binary.limits = c(0.2,0.8),
#'                                                            t.mean = c(0,1),
#'                                                            t.limits = c(1,20),
#'                                                            nl.mean = c(0,1),
#'                                                            nl.limits = c(10,30),
#'                                                            nl.freq = c(10,20,15,1),
#'                                                            sp.points = 5,
#'                                                            incl.group = 0),
#'                                          data.groups = 1,
#'                                          data.groups.distance = c(0.5,1.5),
#'                                          data.non.linearity = 0,
#'                                          data.spars = 0.4,
#'                                          data.low.dim = 5,
#'                                          data.low.dim.groups = 1,
#'                                          data.low.dim.parameters = list(trunc.parameter = c(0.5,10,2,0.5),
#'                                                                         mean = 1,
#'                                                                         error.std = 0.05)),
#'                         "Sphere" = list(data.size = 2000,
#'                                         ata.type = c(10,0,0,0,0,0),
#'                                         data.low.dim = 3,
#'                                         data.non.linearity = 1,
#'                                         data.low.dim.parameters = list(error.std = 0.8)),
#'                         "Many Groups" = list(data.size = 2000,
#'                                              data.type = c(10,0,0,0,0,0),
#'                                              data.low.dim = 5,
#'                                              data.groups = 8,
#'                                              data.groups.distance = c(1,5)),
#'                         "Many Low-Dim Groups" = list(data.size = 2000,
#'                                                      data.type = c(10,0,0,0,0,0),
#'                                                      data.low.dim = 1,
#'                                                      data.low.dim.groups = 10,
#'                                                      data.low.dim.parameters = list(mean = 6,
#'                                                                                     error.std = 0.5)),
#'                         "Poisson non-linear" = list(data.size = 2000,
#'                                                    data.type = c(0,10,0,0,0,0,0),
#'                                                    data.low.dim = 2,
#'                                                    data.non.linearity = 0.6),
#'                         ...    )
#' ```
#'
#' ## Customizing the Missingness Pattern Simulation Protocol
#'
#' The parameter `missingness_parameters` expects a named list of named lists where each list entry is comprised of at least one
#' and at most three of lists named `"MCAR"`, `"MAR"` and/or `"MNAR"`, which themselves contain the parameters for the respective
#' missingness mechanisms. Again for more details we refer to the accompanying vignette.
#'
#' Example for `missingness_parameters`:
#' ```
#' missingness_parameters = list("Parameters 1" = list("MCAR" = list(columns = 1:10,
#'                                                                   size = 10,
#'                                                                   probability = 0.25)),
#'                               "Parameters 2" = list("MCAR" = list(columns = 1:10,
#'                                                                   size = 10,
#'                                                                   probability = 0.05),
#'                                                      "MAR" = list(columns = 2:4,
#'                                                                   size = 3,
#'                                                                   size.regressors = c(1,2),
#'                                                                   probability.upper = 0.9,
#'                                                                   probability.lower = 0.1,
#'                                                                   probability.midpoint = c(0.5,0.2)),
#'                                                       "MNAR" = list(columns = c(5,10),
#'                                                                     size = 2,
#'                                                                     probability.upper = 0.05,
#'                                                                     probability.lower = 0.95,
#'                                                                     probability.midpoint = c(0.5,0.3))),
#'                               "Parameters 3" = list("MAR" = list(columns = 6:10,
#'                                                                  size = 5,
#'                                                                  size.regressors = c(1,2),
#'                                                                  probability.upper = 0.9,
#'                                                                  probability.lower = 0.1,
#'                                                                  probability.midpoint = c(0.5,0.2)),
#'                                                     "MNAR" = list(columns = 1:5,
#'                                                                   size = 5,
#'                                                                   probability.upper = 0.05,
#'                                                                   probability.lower = 0.95,
#'                                                                   probability.midpoint = c(0.5,0.3))),
#'                               "Parameters 4" = ...                                                      )
#' ```
#'
#' ## Customization of the Benchmarking Protocol
#'
#' At the moment the argument `error_measure` can not be changed as only its default `"l2"` is supported.
#'
#' The argument `scaling_robust` expects a positive numeric. It prevents division by 0 when scaling with the error by the standard
#' deviation. Default of `scaling_robust` is `0.1`.
#'
#' The argument `repetitions`, by default set to `5`, controls how imputation performance on a scenario is computed. It expects a
#' positive integer as an input.
#'
#' ## Technical Arguments
#'
#' If `TRUE` the argument `verbose` lets the consolue put out a statement after each run of each scenario. Default is `FALSE`.
#'
#' Via `seed` one can set a seed for the benchmarking. Consider that this seed is not passed to imputation methods and thus for
#' methods using randomness such as missForest the benchmarking performance does not need to be the same even if the seeds are
#' equal.
#'
#' The parameter `max_time`, set by default to `2592000` (or 30 days), controls the maximal time which imputation methods are
#' allowed to compute their results.
#'
#' The parameter `output_data` controls whether additional data (to the benchmarking results) should be stored and returned. Default
#' is `TRUE`. Details on the outputs can be found in the vignette and further below.
#'
#' @return A list is returned by `simulation_ImputeBench()` with one or two entries. The first is `$evaluation`
#' under which one finds a data frame listing imputation performance per run and method and which can be plugged into
#' `plot_ImputeBench()` for a plot of performance. The second entry is included if `output_data = TRUE` and is called `$data.list`.
#' One can find here all drawn data matrices, missingness pattern and all trained parameter of the imputation methods, for details
#' see the vignette.
#'
#' @references R. Richter, A. Miloschewski, J.F. Tavares, M.M.B. Breteler, S. Mukherjee (2012), Benchmarking Single
#' Imputation Methods. in preparation.
#'
#' @seealso `data_ImputeBench`, `plot_ImputeBench`, `missingness_scenario_from_parameters`, `simulate_mask`, `simulate_data`
#'
#' @export
#'



simulation_ImputeBench = function(methods = NULL,
                                  training = NULL,
                                  data_parameters = NULL,
                                  missingness_parameters = NULL,
                                  repetitions = 5,  # 10
                                  default_methods = c("knn","MICE","missF","softImpute"),
                                  baseline_method = "median",
                                  error_measure = "l2",
                                  scaling_robust = 0.01,
                                  retrain_always = FALSE,
                                  verbose = FALSE, #FALSE
                                  seed = NULL,
                                  max_time = 2592000,
                                  output_data = TRUE){

  # Check the eligibility of the input
  if(!base::is.null(methods)){
    if(!base::is.null(names(methods))){
      method.names = names(methods)
    } else{
      method.names = NULL
    }

    if(!base::is.list(methods)){
      stop("methods is not a list object.")
    }
    for(t in 1:length(methods)){
      if(!base::is.function(methods[[t]])){
        stop(paste0("List entry ", t, " of 'methods' is not a function."))
      }
    }
    nbr.methods = base::length(methods)
    if(base::is.null(training)){
      training = list()
    } else{
      if(!is.list(training)){
        stop("training is not a list object.")
      }
      if(nbr.methods != base::length(training)){
        stop("Length of methods list must be equal to length of trainings list or training must be NULL.")
      }
      for(k in 1:length(training)){
        if(!is.function(training[[t]])){
          stop(paste0("List entry ", k, " of training is not a function."))
        }
      }
    }
    if(!base::is.null(method.names)){
      nbr.mnames = base::length(method.names)
      for(t in 1:nbr.mnames){
        if(identical(method.names[t], "")){
          method.names[t] = paste0("Method ", as.character(t))
        }
      }
    } else{
      method.names = rep("generic", times = nbr.methods)
      for(k in 1:nbr.methods){
        method.names[k] = base::paste("Method ", base::as.character(k), sep = "" )
      }
    }
  } else{
    if(!is.null(training)){
      stop("Lenght of methods list must be equal to length of training or training must be NULL.")
    }
  }

  if(!is.null(missingness_parameters)){

    # Check for not used entries
    for(t in 1:length(missingness_parameters)){
      chk = length(setdiff(names(missingness_parameters[[t]]),c("MCAR","MAR","MNAR")))
      if(chk != 0){
        stop(paste0("The ",t," entry of missingness_parameters seems to have an unknown entry."))
      }
      if(!is.null(missingness_parameters[[t]]$MCAR)){
        chk = length(setdiff(names(missingness_parameters[[t]]$MCAR), c("columns", "size", "probability")))
        if(chk != 0){
          stop(paste0("The ",t," entry of missingness_parameters seems to have an unknown entry in its MCAR component."))
        }
      }
      if(!is.null(missingness_parameters[[t]]$MAR)){
        chk = length(setdiff(names(missingness_parameters[[t]]$MAR), c("columns", "size", "size.regressors" ,"probability.upper", "probability.lower", "probability.midpoint")))
        if(chk != 0){
          stop(paste0("The ",t," entry of missingness_parameters seems to have an unknown entry in its MAR component."))
        }
      }
      if(!is.null(missingness_parameters[[t]]$MNAR)){
        chk = length(setdiff(names(missingness_parameters[[t]]$MNAR), c("columns", "size" ,"probability.upper", "probability.lower", "probability.midpoint")))
        if(chk != 0){
          stop(paste0("The ",t," entry of missingness_parameters seems to have an unknown entry in its MNAR component."))
        }
      }
    }

    if(("MCAR" %in% names(missingness_parameters) | "MAR" %in% names(missingness_parameters)) | "MNAR" %in% names(missingness_parameters)){
      stop("It seems like you only have one missingness parameter and you forgot to make it a list of one entry, i.e. missingness_parameters = list(list(MCAR = list(...))).")
    }
    for(k in 1:length(missingness_parameters)){
      miss.p = missingness_parameters[[k]]
      if((!is.null(miss.p$MCAR) & !is.null(miss.p$MCAR$columns) )  & !is.vector(miss.p$MCAR$columns) ){
        stop("MCAR$columns is not a vector.")
      }
      if((!is.null(miss.p$MCAR) & !is.null(miss.p$MCAR$size) )  & (miss.p$MCAR$size != round(miss.p$MCAR$size)) ){
        stop("MCAR$size is not an integer")
      }
      if(!is.null(miss.p$MCAR) & !is.null(miss.p$MCAR$probability)){
        if((!is.numeric(miss.p$MCAR$probability) |miss.p$MCAR$probability < 0) | miss.p$MCAR$probability > 1){
          stop("MCAR$probability is not a numeric between 0 and 1.")
        }
      }
      if((!is.null(miss.p$MAR) & !is.null(miss.p$MAR$columns) )  & !is.vector(miss.p$MAR$columns) ){
        stop("MAR$columns is not a vector.")
      }
      if((!is.null(miss.p$MAR) & !is.null(miss.p$MAR$size.regressors) )  & !is.vector(miss.p$MAR$size.regressors) ){
        stop("MAR$size.regressors is not a vector.")
      }
      if((!is.null(miss.p$MAR) & !is.null(miss.p$MAR$size) )  & (miss.p$MAR$size != round(miss.p$MAR$size)) ){
        stop("MAR$size is not an integer")
      }
      if(!is.null(miss.p$MAR) & !is.null(miss.p$MAR$probability.upper) ){
        if( ((!is.numeric(miss.p$MAR$probability.upper) | miss.p$MAR$probability.upper < 0) | miss.p$MAR$probability.upper > 1)){
          stop("MAR$probability.upper is not a numeric between 0 and 1.")
        }
      }
      if(!is.null(miss.p$MAR) & !is.null(miss.p$MAR$probability.lower) ){
        if((!is.numeric(miss.p$MAR$probability.lower) |  miss.p$MAR$probability.lower < 0) | miss.p$MAR$probability.lower > 1){
          stop("MAR$probability.lower is not a numeric between 0 and 1.")
        }
      }
      if(!is.null(miss.p$MAR) & !is.null(miss.p$MAR$probability.midpoint) ){
        if((!is.numeric(miss.p$MAR$probability.midpoint[1]) | miss.p$MAR$probability.midpoint[1] < 0) | miss.p$MAR$probability.midpoint[1] > 1){
          stop("MAR$probability.midpoint[1] is not a numeric between 0 and 1.")
        }
      }
      if(!is.null(miss.p$MAR) & !is.null(miss.p$MAR$probability.midpoint)){
        if((!is.numeric(miss.p$MAR$probability.midpoint[2]) |miss.p$MAR$probability.midpoint[2] < 0) | miss.p$MAR$probability.midpoint[2] > 1){
          stop("MAR$probability.midpoint[2] is not a numeric between 0 and 1.")
        }
      }
      if((!is.null(miss.p$MNAR) & !is.null(miss.p$MNAR$columns) )  & !is.vector(miss.p$MNAR$columns) ){
        stop("MNAR$columns is not a vector.")
      }
      if((!is.null(miss.p$MNAR) & !is.null(miss.p$MNAR$size) )  & (miss.p$MNAR$size != round(miss.p$MNAR$size)) ){
        stop("MNAR$size is not an integer")
      }
      if(!is.null(miss.p$MNAR) & !is.null(miss.p$MNAR$probability.upper)){
        if((!is.numeric(miss.p$MNAR$probability.upper) |miss.p$MNAR$probability.upper < 0) | miss.p$MNAR$probability.upper > 1){
          stop("MNAR$probability.upper is not a numeric between 0 and 1.")
        }
      }
      if(!is.null(miss.p$MNAR) & !is.null(miss.p$MNAR$probability.lower)){
        if((!is.numeric(miss.p$MNAR$probability.lower) | miss.p$MNAR$probability.lower < 0) | miss.p$MNAR$probability.lower > 1){
          stop("MNAR$probability.lower is not a numeric between 0 and 1.")
        }
      }
      if(!is.null(miss.p$MNAR) & !is.null(miss.p$MNAR$probability.midpoint) ) {
        if((!is.numeric(miss.p$MNAR$probability.midpoint[1]) | miss.p$MNAR$probability.midpoint[1] < 0) | miss.p$MNAR$probability.midpoint[1] > 1){
          stop("MNAR$probability.midpoint[1] is not a numeric between 0 and 1.")
        }
      }
      if(!is.null(miss.p$MNAR) & !is.null(miss.p$MNAR$probability.midpoint)){
        if((!is.numeric(miss.p$MNAR$probability.midpoint[2]) | miss.p$MNAR$probability.midpoint[2] < 0) | miss.p$MNAR$probability.midpoint[2] > 1){
          stop("MNAR$probability.midpoint[2] is not a numeric between 0 and 1.")
        }
      }
    }
  }
  if(!is.null(data_parameters)){
    # Check that there are no additional or missspelled entries
    for(t in 1:length(data_parameters)){
      chk = length(setdiff(names(data_parameters[[t]]),c("data.size", "data.type", "data.shape", "data.groups", "data.non.linearity", "data.spars",
                                                         "data.groups.distance", "data.low.dim", "data.low.dim.groups", "data.low.dim.parameters")))
      if(chk != 0){
        stop(paste0("The entry ", t, " in data_parameters seems to have an unknown entry."))
      }
      if(!is.null(data_parameters[[t]]$data.shape)){
        chk = length(setdiff(names(data_parameters[[t]]$data.shape),c("gauss.mean","gauss.sd","pois.lambda","binary.limits","t.mean","t.limits", "nl.mean",
                                                                      "nl.limits","nl.freq","sp.points","incl.group")))
        if(chk != 0){
          stop(paste0("The entry ", t, " in data_parameters seems to have an unknown entry in data.shape."))
        }
      }
      if(!is.null(data_parameters[[t]]$data.low.dim.parameters)){
        chk = length(setdiff(names(data_parameters[[t]]$data.low.dim.parameters),c("trunc.parameter","mean","error.std")))
        if(chk != 0){
          stop(paste0("The entry ", t, " in data_parameters seems to have an unknown entry in data.low.dim.parameters."))
        }
      }
    }

    if(((((((("data.size" %in% names(data_parameters) | "data.type" %in% names(data_parameters)) | "data.shape" %in% names(data_parameters)) |
            "data.non.linearity" %in% names(data_parameters)) |"data.spars" %in% names(data_parameters) ) |"data.low.dim" %in% names(data_parameters))|
         "data.groups" %in% names(data_parameters)) | "data.low.dim.groups" %in% names(data_parameters))|
       "data.low.dim.parameters" %in% names(data_parameters)){
      stop("It seems like you only have one data parameter and you forgot to make it a list of one entry, i.e. data_parameters = list(list(data.size = ...)).")
    }
    for(k in 1:length(data_parameters)){
      data.p = data_parameters[[k]]

      if(!is.null(data.p$data.size) & (data.p$data.size != round(data.p$data.size))){
        stop("data.size is not an integer.")
      }
      if(!is.null(data.p$data.non.linearity) & !is.numeric(data.p$data.non.linearity)){
        stop("data.non.linearity is not numeric.")
      }
      if(!is.null(data.p$data.spars)){
        if( ((!is.numeric(data.p$data.spars) | data.p$data.spars < 0)) | data.p$data.spars > 1){
          stop("data.spars is not a numeric between 0 and 1.")
        }
      }
      if(!is.null(data.p$data.low.dim)){
        if(data.p$data.low.dim != round(data.p$data.low.dim)){
          stop("data.low.dim is not an integer.")
        }
      }
      if(!is.null(data.p$data.low.dim.groups)){
        if( data.p$data.low.dim.groups != round(data.p$data.low.dim.groups)){
          stop("data.low.dim.groups is not an integer.")
        }
      }
      if(!is.null(data.p$data.groups)){
        if(data.p$data.groups  != round(data.p$data.groups)){
          stop("data.groups is not an integer.")
        }
      }
      if(!is.null(data.p$data.low.dim.parameters) & !is.list(data.p$data.low.dim.parameters)){ # For now we do not check if the data.low.dim.parameters are ok.
        stop("data.low.dim.parameters is not a list.")
      }
      if(!is.null(data.p$data.shape) & !is.list(data.p$data.shape)){ # For now we do not check all data.shape parameters if ok.
        stop("data.shape is not a list.")
      }
    }
  }

  ## Adding the default methods and training functions

  if(base::is.null(methods)){
    methods = list()
    training = list()
    method.names = c("generic")
    k = 1
  } else{
    k = base::length(methods) + 1
  }
  # We need to add the default methods and their training functions if wanted
  if(!base::is.null(default_methods)){
    if("knn" %in% default_methods){
      methods[[k]] = wKNN
      training[[k]] = train_KNN
      method.names[k] = "k-nn (default)"
      k = k + 1
    }
    if("MICE" %in% default_methods){
      methods[[k]] = wMICE
      training[[k]] = train_MICE
      method.names[k] = "MICE (default)"
      k = k + 1
    }
    if("missF" %in% default_methods){
      methods[[k]] = wmissF
      training[[k]] = train_missF
      method.names[k] = "missForest (default)"
      k = k + 1
    }
    if("softImpute" %in% default_methods){
      methods[[k]] = wsImpute
      training[[k]] = train_sImpute
      method.names[k] = "soft impute (default)"
      k = k + 1
    }
  }
  # Finally we need to add the baseline impuation
  methods[[k]] = baseline_imputation
  training[[k]] = function(data){list(method = baseline_method)}
  method.names[k] = "Baseline"

  ### Setting default missingness and data parameters

  if(is.null(data_parameters)){
    if(!is.null(missingness_parameters)){
      names_of_choices = names(missingness_parameters)

      sz = 30
      miss.parameters = list()
      dt.parameters = list()
      for(t in 1:length(missingness_parameters)){

        miss.parameters[[t]] = list(MCAR = if(is.null(missingness_parameters[[t]]$MCAR)){NULL} else{
          list(columns = if(is.null((missingness_parameters[[t]]$MCAR$columns))){1:sz} else{missingness_parameters[[t]]$MCAR$columns},
               size = if(is.null((missingness_parameters[[t]]$MCAR$size))){floor(sz/2)} else{missingness_parameters[[t]]$MCAR$size},
               probability = if(is.null((missingness_parameters[[t]]$MCAR$probability))){0.25} else{missingness_parameters[[t]]$MCAR$probability})},
          MAR = if(is.null(missingness_parameters[[t]]$MAR)){NULL} else{
            list(columns = if(is.null((missingness_parameters[[t]]$MAR$columns))){1:sz} else{missingness_parameters[[t]]$MAR$columns},
                 size = if(is.null((missingness_parameters[[t]]$MAR$size))){floor(sz/2)} else{missingness_parameters[[t]]$MAR$size},
                 size.regressors = if(is.null((missingness_parameters[[t]]$MAR$size.regressors))){c(1,2,3)} else{missingness_parameters[[t]]$MAR$size.regressors},
                 probability.upper = if(is.null((missingness_parameters[[t]]$MAR$probability.upper))){0.8} else{missingness_parameters[[t]]$MAR$probability.upper},
                 probability.lower = if(is.null((missingness_parameters[[t]]$MAR$probability.lower))){0.2} else{missingness_parameters[[t]]$MAR$probability.lower},
                 probability.midpoint = if(is.null((missingness_parameters[[t]]$MAR$probability.midpoint))){c(0.5,0.5)} else{missingness_parameters[[t]]$MAR$probability.midpoint})},
          MNAR = if(is.null(missingness_parameters[[t]]$MNAR)){NULL} else{
            list(columns = if(is.null((missingness_parameters[[t]]$MNAR$columns))){1:sz} else{missingness_parameters[[t]]$MNAR$columns},
                 size = if(is.null((missingness_parameters[[t]]$MNAR$size))){floor(sz/2)} else{missingness_parameters[[t]]$MNAR$size},
                 probability.upper = if(is.null((missingness_parameters[[t]]$MNAR$probability.upper))){0.8} else{missingness_parameters[[t]]$MNAR$probability.upper},
                 probability.lower = if(is.null((missingness_parameters[[t]]$MNAR$probability.lower))){0.2} else{missingness_parameters[[t]]$MNAR$probability.lower},
                 probability.midpoint = if(is.null((missingness_parameters[[t]]$MNAR$probability.midpoint))){c(0.5,0.5)} else{missingness_parameters[[t]]$MNAR$probability.midpoint})})

        dt.parameters[[t]] = list(data.size = 100,
                                  data.type = c(5,5,5,5,5,5),
                                  data.shape = list(gauss.mean = c(0,1) ,
                                                    gauss.sd = c(1,2) ,
                                                    pois.lambda = c(1) ,
                                                    binary.limits = c(0.2,0.8) ,
                                                    t.mean = c(0,1),
                                                    t.limits = c(1,20) ,
                                                    nl.mean = c(0,1) ,
                                                    nl.limits = c(10,30),
                                                    nl.freq = c(10,20,15,1),
                                                    sp.points = 5,
                                                    incl.groups = 0),
                                  data.non.linearity = 0,
                                  data.spars    = 0.4,
                                  data.low.dim  = 5,
                                  data.groups = 1,
                                  data.groups.distance = c(0.5,1.5),
                                  data.low.dim.groups = 1,
                                  data.low.dim.parameters =   list(trunc.parameter = c(0.5,10,2,0.5),
                                                                   mean = 1,
                                                                   error.std = 0.05))
      }
    } else{
      sz = 30
      dt.parameters = list()
      miss.parameters = list()
      for(t in 1:4){

        miss.parameters[[t]] = list(MCAR = list(columns = {1:sz},
                                                size = floor(sz/2),
                                                probability = 0.25))

        dt.parameters[[t]] = list(data.size = if(t == 1){50} else if(t == 2){100} else if(t==3){200} else{400},
                                  data.type = c(5,5,5,5,5,5),
                                  data.shape = list(gauss.mean = c(0,1) ,
                                                    gauss.sd = c(1,2) ,
                                                    pois.lambda = c(1) ,
                                                    binary.limits = c(0.2,0.8) ,
                                                    t.mean = c(0,1),
                                                    t.limits = c(1,20) ,
                                                    nl.mean = c(0,1) ,
                                                    nl.limits = c(10,30),
                                                    nl.freq = c(10,20,15,1),
                                                    sp.points = 5,
                                                    incl.group = 0),
                                  data.non.linearity = 0,
                                  data.spars    = 0.4,
                                  data.low.dim  = 5,
                                  data.groups = 1,
                                  data.groups.distance = c(0.5,1.5),
                                  data.low.dim.groups = 1,
                                  data.low.dim.parameters =   list(trunc.parameter = c(0.5,10,2,0.5),
                                                                   mean = 1,
                                                                   error.std = 0.05))
      }
    }
  } else{
    if(is.null(names(data_parameters))){
      names_of_choices = names(missingness_parameters)
    } else{
      names_of_choices = names(data_parameters)
    }

    if(is.null(missingness_parameters)){
      dt.parameters = list()
      miss.parameters = list()
      for(t in 1:length(data_parameters)){
        if(is.null(data_parameters[[t]]$data.type)){
          sz = 30
        } else{
          sz = sum(data_parameters[[t]]$data.type)
        }

        miss.parameters[[t]] = list(MCAR = list(columns = {1:sz},
                                                size = floor(sz/2),
                                                probability = 0.25))

        if(!is.null(data_parameters[[t]]$data.shape)){
          data_parameters[[t]]$data.shape = list(gauss.mean = if(is.null(data_parameters[[t]]$data.shape$gauss.mean)){c(0,1)
          } else{data_parameters[[t]]$data.shape$gauss.mean},
          gauss.sd = if(is.null(data_parameters[[t]]$data.shape$gauss.sd)){c(1,2)
          } else{data_parameters[[t]]$data.shape$gauss.sd},
          pois.lambda = if(is.null(data_parameters[[t]]$data.shape$pois.lambda)){1
          } else{data_parameters[[t]]$data.shape$pois.lambda},
          binary.limits = if(is.null(data_parameters[[t]]$data.shape$binary.limits)){c(0.2,0.8)
          } else{data_parameters[[t]]$data.shape$binary.limits},
          t.mean = if(is.null(data_parameters[[t]]$data.shape$t.mean)){c(0,1)
          } else{data_parameters[[t]]$data.shape$t.mean},
          t.limits = if(is.null(data_parameters[[t]]$data.shape$t.limits)){c(1,20)
          } else{data_parameters[[t]]$data.shape$t.limits},
          nl.mean = if(is.null(data_parameters[[t]]$data.shape$nl.mean)){c(0,1)
          } else{data_parameters[[t]]$data.shape$nl.mean},
          nl.limits = if(is.null(data_parameters[[t]]$data.shape$nl.limits)){c(10,30)
          } else{data_parameters[[t]]$data.shape$nl.limits},
          nl.freq = if(is.null(data_parameters[[t]]$data.shape$nl.freq)){c(10,20,15,1)
          } else{data_parameters[[t]]$data.shape$nl.freq},
          sp.points = if(is.null(data_parameters[[t]]$data.shape$sp.points)){5
          } else{data_parameters[[t]]$data.shape$sp.points},
          incl.group = if(is.null(data_parameters[[t]]$data.shape$incl.group)){0
          } else{data_parameters[[t]]$data.shape$incl.group})
        }

        if(!is.null(data_parameters[[t]]$data.low.dim.parameters)){
          data_parameters[[t]]$data.low.dim.parameters = list(trunc.parameter = if(is.null(data_parameters[[t]]$data.low.dim.parameters$trunc.parameter)){ c(0.5,10,2,0.5)
          } else{data_parameters[[t]]$data.low.dim.parameters$trunc.parameter},
          mean = if(is.null(data_parameters[[t]]$data.low.dim.parameters$mean)){ 1
          } else{data_parameters[[t]]$data.low.dim.parameters$mean},
          error.std = if(is.null(data_parameters[[t]]$data.low.dim.parameters$error.std)){ 0.05
          } else{data_parameters[[t]]$data.low.dim.parameters$error.std})
        }

        dt.parameters[[t]] = list(data.size = if(is.null(data_parameters[[t]]$data.size)){100} else{data_parameters[[t]]$data.size},
                                  data.type = if(is.null(data_parameters[[t]]$data.type)){c(5,5,5,5,5,5)} else{data_parameters[[t]]$data.type},
                                  data.shape = if(is.null(data_parameters[[t]]$data.shape)){list(gauss.mean = c(0,1) ,
                                                                                                 gauss.sd = c(1,2) ,
                                                                                                 pois.lambda = c(1) ,
                                                                                                 binary.limits = c(0.2,0.8) ,
                                                                                                 t.mean = c(0,1),
                                                                                                 t.limits = c(1,20) ,
                                                                                                 nl.mean = c(0,1) ,
                                                                                                 nl.limits = c(10,30),
                                                                                                 nl.freq = c(10,20,15,1),
                                                                                                 sp.points = 5,
                                                                                                 incl.group = 0)} else{data_parameters[[t]]$data.shape},
                                  data.non.linearity = if(is.null(data_parameters[[t]]$data.non.linearity)){0} else{data_parameters[[t]]$data.non.linearity},
                                  data.spars = if(is.null(data_parameters[[t]]$data.spars)){0.4} else{data_parameters[[t]]$data.spars},
                                  data.low.dim = if(is.null(data_parameters[[t]]$data.low.dim)){5} else{data_parameters[[t]]$data.low.dim},
                                  data.groups = if(is.null(data_parameters[[t]]$data.groups)){1} else{data_parameters[[t]]$data.groups},
                                  data.groups.distance = if(is.null(data_parameters[[t]]$data.groups.distance)){c(0.5,1.5)} else{data_parameters[[t]]$data.groups.distance},
                                  data.low.dim.groups = if(is.null(data_parameters[[t]]$data.low.dim.groups)){1} else{data_parameters[[t]]$data.low.dim.groups},
                                  data.low.dim.parameters = if(is.null(data_parameters[[t]]$data.low.dim.parameters)){list(trunc.parameter = c(0.5,10,2,0.5),
                                                                                                                           mean = 1,
                                                                                                                           error.std = 0.05)} else{data_parameters[[t]]$data.low.dim.parameters})

      }
    } else{
      if(length(missingness_parameters) != length(data_parameters)){
        stop("missingness_parameters and data_parameters must be of equal length.")
      }
      dt.parameters = list()
      miss.parameters = list()
      for(t in 1:length(missingness_parameters)){

        if(is.null(data_parameters[[t]]$data.type)){
          sz = 30
        } else{
          sz = sum(data_parameters[[t]]$data.type)
        }

        miss.parameters[[t]] = list(MCAR = if(is.null(missingness_parameters[[t]]$MCAR)){NULL} else{
          list(columns = if(is.null((missingness_parameters[[t]]$MCAR$columns))){1:sz} else{missingness_parameters[[t]]$MCAR$columns},
               size = if(is.null((missingness_parameters[[t]]$MCAR$size))){floor(sz/2)} else{missingness_parameters[[t]]$MCAR$size},
               probability = if(is.null((missingness_parameters[[t]]$MCAR$probability))){0.25} else{missingness_parameters[[t]]$MCAR$probability})},
          MAR = if(is.null(missingness_parameters[[t]]$MAR)){NULL} else{
            list(columns = if(is.null((missingness_parameters[[t]]$MAR$columns))){1:sz} else{missingness_parameters[[t]]$MAR$columns},
                 size = if(is.null((missingness_parameters[[t]]$MAR$size))){floor(sz/2)} else{missingness_parameters[[t]]$MAR$size},
                 size.regressors = if(is.null((missingness_parameters[[t]]$MAR$size.regressors))){c(1,2,3)} else{missingness_parameters[[t]]$MAR$size.regressors},
                 probability.upper = if(is.null((missingness_parameters[[t]]$MAR$probability.upper))){0.8} else{missingness_parameters[[t]]$MAR$probability.upper},
                 probability.lower = if(is.null((missingness_parameters[[t]]$MAR$probability.lower))){0.2} else{missingness_parameters[[t]]$MAR$probability.lower},
                 probability.midpoint = if(is.null((missingness_parameters[[t]]$MAR$probability.midpoint))){c(0.5,0.5)} else{missingness_parameters[[t]]$MAR$probability.midpoint})},
          MNAR = if(is.null(missingness_parameters[[t]]$MNAR)){NULL} else{
            list(columns = if(is.null((missingness_parameters[[t]]$MNAR$columns))){1:sz} else{missingness_parameters[[t]]$MNAR$columns},
                 size = if(is.null((missingness_parameters[[t]]$MNAR$size))){floor(sz/2)} else{missingness_parameters[[t]]$MNAR$size},
                 probability.upper = if(is.null((missingness_parameters[[t]]$MNAR$probability.upper))){0.8} else{missingness_parameters[[t]]$MNAR$probability.upper},
                 probability.lower = if(is.null((missingness_parameters[[t]]$MNAR$probability.lower))){0.2} else{missingness_parameters[[t]]$MNAR$probability.lower},
                 probability.midpoint = if(is.null((missingness_parameters[[t]]$MNAR$probability.midpoint))){c(0.5,0.5)} else{missingness_parameters[[t]]$MNAR$probability.midpoint})})


        if(!is.null(data_parameters[[t]]$data.shape)){
          data_parameters[[t]]$data.shape = list(gauss.mean = if(is.null(data_parameters[[t]]$data.shape$gauss.mean)){c(0,1)
          } else{data_parameters[[t]]$data.shape$gauss.mean},
          gauss.sd = if(is.null(data_parameters[[t]]$data.shape$gauss.sd)){c(1,2)
          } else{data_parameters[[t]]$data.shape$gauss.sd},
          pois.lambda = if(is.null(data_parameters[[t]]$data.shape$pois.lambda)){1
          } else{data_parameters[[t]]$data.shape$pois.lambda},
          binary.limits = if(is.null(data_parameters[[t]]$data.shape$binary.limits)){c(0.2,0.8)
          } else{data_parameters[[t]]$data.shape$binary.limits},
          t.mean = if(is.null(data_parameters[[t]]$data.shape$t.mean)){c(0,1)
          } else{data_parameters[[t]]$data.shape$t.mean},
          t.limits = if(is.null(data_parameters[[t]]$data.shape$t.limits)){c(1,20)
          } else{data_parameters[[t]]$data.shape$t.limits},
          nl.mean = if(is.null(data_parameters[[t]]$data.shape$nl.mean)){c(0,1)
          } else{data_parameters[[t]]$data.shape$nl.mean},
          nl.limits = if(is.null(data_parameters[[t]]$data.shape$nl.limits)){c(10,30)
          } else{data_parameters[[t]]$data.shape$nl.limits},
          nl.freq = if(is.null(data_parameters[[t]]$data.shape$nl.freq)){c(10,20,15,1)
          } else{data_parameters[[t]]$data.shape$nl.freq},
          sp.points = if(is.null(data_parameters[[t]]$data.shape$sp.points)){5
          } else{data_parameters[[t]]$data.shape$sp.points},
          incl.group = if(is.null(data_parameters[[t]]$data.shape$incl.group)){0
          } else{data_parameters[[t]]$data.shape$incl.group})
        }

        if(!is.null(data_parameters[[t]]$data.low.dim.parameters)){
          data_parameters[[t]]$data.low.dim.parameters = list(trunc.parameter = if(is.null(data_parameters[[t]]$data.low.dim.parameters$trunc.parameter)){ c(0.5,10,2,0.5)
          } else{data_parameters[[t]]$data.low.dim.parameters$trunc.parameter},
          mean = if(is.null(data_parameters[[t]]$data.low.dim.parameters$mean)){ 1
          } else{data_parameters[[t]]$data.low.dim.parameters$mean},
          error.std = if(is.null(data_parameters[[t]]$data.low.dim.parameters$error.std)){ 0.05
          } else{data_parameters[[t]]$data.low.dim.parameters$error.std})
        }

        dt.parameters[[t]] = list(data.size = if(is.null(data_parameters[[t]]$data.size)){100} else{data_parameters[[t]]$data.size},
                                  data.type = if(is.null(data_parameters[[t]]$data.type)){c(5,5,5,5,5,5)} else{data_parameters[[t]]$data.type},
                                  data.shape = if(is.null(data_parameters[[t]]$data.shape)){list(gauss.mean = c(0,1) ,
                                                                                                 gauss.sd = c(1,2) ,
                                                                                                 pois.lambda = c(1) ,
                                                                                                 binary.limits = c(0.2,0.8) ,
                                                                                                 t.mean = c(0,1),
                                                                                                 t.limits = c(1,20) ,
                                                                                                 nl.mean = c(0,1) ,
                                                                                                 nl.limits = c(10,30),
                                                                                                 nl.freq = c(10,20,15,1),
                                                                                                 sp.points = 5,
                                                                                                 incl.group = 0)} else{data_parameters[[t]]$data.shape},
                                  data.non.linearity = if(is.null(data_parameters[[t]]$data.non.linearity)){0} else{data_parameters[[t]]$data.non.linearity},
                                  data.spars = if(is.null(data_parameters[[t]]$data.spars)){0.4} else{data_parameters[[t]]$data.spars},
                                  data.low.dim = if(is.null(data_parameters[[t]]$data.low.dim)){5} else{data_parameters[[t]]$data.low.dim},
                                  data.groups = if(is.null(data_parameters[[t]]$data.groups)){1} else{data_parameters[[t]]$data.groups},
                                  data.groups.distance = if(is.null(data_parameters[[t]]$data.groups.distance)){c(0.5,1.5)} else{data_parameters[[t]]$data.groups.distance},
                                  data.low.dim.groups = if(is.null(data_parameters[[t]]$data.low.dim.groups)){1} else{data_parameters[[t]]$data.low.dim.groups},
                                  data.low.dim.parameters = if(is.null(data_parameters[[t]]$data.low.dim.parameters)){list(trunc.parameter = c(0.5,10,2,0.5),
                                                                                                                           mean = 1,
                                                                                                                           error.std = 0.05)} else{data_parameters[[t]]$data.low.dim.parameters})
      }
    }
  }

  runs = length(dt.parameters)
  names(dt.parameters) = paste0("Parameter Choice ", 1:runs)
  data.list = vector(mode = "list", length = (runs + 1))

  if(is.null(data_parameters)){
    if(is.null(missingness_parameters)){
      names(data.list) = c("Overview_Parameters", paste0("Parameters_", 1:runs))
    } else{
      if(is.null(names(missingness_parameters))){
        names(data.list) = c("Overview_Parameters", paste0("Parameters_", 1:runs))
      } else{
        names(data.list)[1] = "Overview_Parameters"
        names(data.list)[2:(runs+1)] = names_of_choices
      }
    }
  } else{
    if(is.null(names(data_parameters))){
      names(data.list) = c("Overview_Parameters", paste0("Parameters_", 1:runs))
      if(is.null(missingness_parameters)){
        names(data.list) = c("Overview_Parameters", paste0("Parameters_", 1:runs))
      } else{
        if(is.null(names(missingness_parameters))){
          names(data.list) = c("Overview_Parameters", paste0("Parameters_", 1:runs))
        } else{
          names(data.list)[1] = "Overview_Parameters"
          names(data.list)[2:(runs+1)] = names_of_choices
        }
      }
    } else{
      names(data.list)[1] = "Overview_Parameters"
      names(data.list)[2:(runs+1)] = names_of_choices
    }
  }


  data.list[[1]]$data_parameters = dt.parameters
  data.list[[1]]$missingness_parameters = miss.parameters

  for(tau in 1:runs){
    # print(tau)
    # tau = 1
    # We need to set the defaults for parameters and vary.parameters
    if(verbose){
      print(paste("Starting parameter choice ", tau, " of ", runs, sep = ""))
    }
    # We need the parameters
    if(!is.null(seed)){
      seed = seed + (tau*10000)
    }
    Results = varying_simulation_study_oneRun(methods = methods,
                                              method.names = method.names,
                                              training = training,
                                              data_parameters = dt.parameters[[tau]],
                                              missingness_parameters = miss.parameters[[tau]],
                                              repetitions = repetitions,
                                              error_measure = error_measure,
                                              scaling_robust = scaling_robust,
                                              retrain.always = retrain_always,
                                              verbose = verbose,
                                              seed = seed,
                                              run.nbr = tau,
                                              max.time = max_time,
                                              output_data = output_data)
    Results$evaluation$parameters.name = names(data.list)[tau + 1]

    if(tau == 1){
      if(output_data){
        evaluation = Results$evaluation
        data.list[[tau+1]] = Results$data.list
      } else{
        Simulation.Results = Results
      }
    } else{
      if(output_data){

        evaluation = rbind(evaluation, Results$evaluation)
        data.list[[tau+1]] =  Results$data.list
      } else{
        Simulation.Results = rbind(Simulation.Results, Results)
      }
    }
  }
  if(output_data){
    Simulation.Results = list(evaluation = evaluation,
                              data.list = data.list)
  }
  return(Simulation.Results)
}
