

#' Benchmarking Imputation Methods on Real Data
#'
#' @description Simulation Study to compare imputation methods on a given data with additionally simulated missingness
#' according to the benchmarking protocol of RMTBM22.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries.
#' @param methods A (named) list of functions, taking as an input a numeric matrix with missing entries via an argument `data` and a list of
#' parameters via an argument `args`. Default is \code{NULL}, i.e. no additional methods added to the benchmarking.
#' @param training A list of functions - which need to be of the same length as the `methods` list -, that take as an input
#' a data matrix with missing entries via an argument `data` and outputs the parameters for the respective method via as a list `args`.
#' Default is \code{NULL}.
#' @param scenarios A (named) list of named lists of missingness scenarios (parameters thereof), for details and an example see below and the vignette.
#' If \code{NULL} scenarios are drawn by `missingness_parameters`. In the case also `missingness_parameters` is `NULL` a default missingness
#' scenario is used, see the vignette for details. Default is \code{NULL}.
#' @param missingness_parameters A (named) list of named lists of parameters defining the way missingness scenarios are drawn, for details see below
#' and the vignette. In the case `scenarios` are provided, `missingness_parameters` is ignored. Default is \code{NULL}.
#' @param repetitions Integer determining how often an additional missingness pattern according to one entry of `scenarios` or  `missingness_parameters`
#' is drawn. Default is `5`.
#' @param error_grouping A list of indice vectors defining column groups. Errors for each groups are collected and the overall error
#' is computed by the mean error over all groups (weighting all groups the same irrespective of the number of columns they contain).
#' Default is \code{NULL}.
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
#' @details This function benchmarks imputation methods on user-provided data. Via the argument `data` a numeric matrix must be passed
#' that may or may not contain missing values given as `NA`-entries. A detailed description of the functionality and arguments of
#' `data_ImputeBench()` can be found in the accompanying vignette. For the exact definition of the benchmarking protocol, the
#' missingness pattern simulation protocol and the definition of a missingness scenario we refer to the paper RMTBM22, see below.
#' In the following a brief overview over the arguments and example inputs.
#'
#' ## Customizing the Compared Imputation Methods
#'
#' To add to the default imputation methods user specified ones `data_ImputeBench()` provides the argument `methods` which expects a
#' list of functions to be passed. These functions have to have exactly two inputs: `data` (a numeric matrix with missing entries
#' denoted by `NA`) and `args` (a list with parameters for the imputation method, this can be left `NULL`). The output needs to be a
#' numeric matrix without missing entries (by default still missing entries are filled by the baseline imputation method). For details
#' see the accompanying vignette. Customizing names of the additional methods is done by naming list entries.
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
#' Via the argument `training` parameter training functions can be passed to `data_ImputeBench()` (note, that this is necessary
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
#' ## Customizing the Missingness Pattern Simulation Protocol
#'
#' The parameter `scenarios` expects a named list of named lists as an entry. Each entry represents a scenario under which imputation
#' methods are compared. Each such list entry contains at least one and up to three list entries named `"MCAR"`, `"MAR"` and/or `"MNAR"`,
#' each of which contain the parameters of the missingness pattern protocol for columns with MCAR, MAR and MNAR missingness, respectively.
#' For the definition of a missingness scenario and the protocol under which the missingness pattern is being drawn we refer to
#' RMTBM22 and the accompanying vignette.
#'
#' Example for `scenarios`:
#' ```
#' scenarios = list("Scenario 1" = list(MCAR = list(columns = c(1,2,4,6),
#'                                                  probability = c(0.4,0.4,0.5,0.6)),
#'                                      MAR = list(columns = c(5,7),
#'                                                 reg.columns = list(c(1,2,3),
#'                                                                    c(3,6)),
#'                                                 probability.upper = c(0.9,0.8),
#'                                                 probability.midpoint = list(c(0.8,0.25),
#'                                                                             c(0.75,0.3)),
#'                                                 probability.lower = c(0.1,0.2)),
#'                                      MNAR = list(columns = c(8,9),
#'                                                  probability.upper = c(0.9,0.95),
#'                                                  probability.midpoint = list(c(0.8,0.25),
#'                                                                              c(0.75,0.3)),
#'                                                  probability.lower = c(0.1,0.1))),
#'                  "Scenario 2" = list(MCAR = list(columns = c(1,2,3),
#'                                                  probability = c(0.5,0.7,0.1))),
#'                  "Scenario 3" = ...  )
#' ```
#'
#' In the case that `scenarios` is `NULL`, one can specify alternatively `missingness_parameters` (an argument otherwise ignored).
#' Via `missingness_parameters` one passes parameters for drawing missingness scenarios to `data_ImputeBench()`, for details see RMTBM22.
#' The syntax of `missingness_parameters` works very similar to `scenarios` as it expects a named list of named list where each
#' list entry is comprised of at least one and at most three of the lists named `"MCAR"`, `"MAR"` and/or `"MNAR"`, which itself
#' contain the parameters for the respective missingness mechanisms. Again for more details we refer to RMTBM22 and the
#' accompanying vignette.
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
#' Note: If `scenarios` and `missingness_parameters` are `NULL` a default scenario is used as specified in the vignette.
#'
#' ## Customization of the Benchmarking Protocol
#'
#' At the moment the argument `error_measure` can not be changed as only its default `"l2"` is supported.
#'
#' The argument `scaling_robust` expects a positive numeric. It prevents division by 0 when scaling with the error by the standard
#' deviation. Default of `scaling_robust` is `0.1`.
#'
#' To have the error computed for groups of variables one can specify groups via the argument `error_grouping` which should be a
#' list of column indices assigning all columns a group.
#'
#' Example of `error_grouping` (for a data matrix with 10 columns):
#' ```
#'grouping = list("Group 1" = c(1,2,3,4),
#'                "Group 2" = c(5,8,9),
#'                "Group 3" = c(6,7,10))
#' ```
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
#' @return A list is returned by `data_ImputeBench()` with one or two entries. The first is `$evaluation`
#' under which one finds a data frame listing imputation performance per run and method and which can be plugged into
#' `plot_ImputeBench()` for a plot of performance. The second entry is included if `output_data = TRUE` and is called `$data.list`. One can find here
#' all drawn missingness pattern and all trained parameter of the imputation methods, for details see the vignette.
#'
#' @references R. Richter, A. Miloschewski, J.F. Tavares, M.M.B. Breteler, S. Mukherjee (2012), Benchmarking Single
#' Imputation Methods. in preparation.
#'
#' @seealso `simulation_ImputeBench`, `plot_ImputeBench`, `missingness_scenario_from_parameters`, `simulate_mask`
#'
#' @import matrixStats
#'
#' @export
#'




data_ImputeBench = function(data,
                            methods = NULL,
                            training = NULL,
                            scenarios = NULL,
                            missingness_parameters = NULL,
                            repetitions = 5,
                            error_grouping = NULL,
                            default_methods = c("knn","MICE","missF","softImpute"),
                            baseline_method = "median",
                            error_measure = "l2",
                            scaling_robust = 0.01,
                            retrain_always = FALSE,
                            verbose = FALSE,
                            seed = NULL,
                            max_time = 2592000,
                            output_data = TRUE){

  # Check the eligibility of the input
  if(is.null(data)){

    stop("No data supplied.")
  } else{
    if(!is.matrix(data)){
      stop("Data is not a matrix.")
    }
    if(!is.numeric(data)){
      stop("Data is not numeric.")
    }
  }

  if(!is.null(scenarios)){

    # Checking for wrong entries
    for(t in 1:length(scenarios)){
      chk = length(setdiff(names(scenarios[[t]]),c("MCAR","MAR","MNAR")))
      if(chk != 0){
        stop(paste0("Scenario ",t," seems to have an unknown entry."))
      }
      if(!is.null(scenarios[[t]]$MCAR)){
        chk = length(setdiff(names(scenarios[[t]]$MCAR), c("columns", "probability")))
        if(chk != 0){
          stop(paste0("Scenario ",t," seems to have an unknown entry in its MCAR component."))
        }
      }
      if(!is.null(scenarios[[t]]$MAR)){
        chk = length(setdiff(names(scenarios[[t]]$MAR), c("columns", "reg.columns","probability.upper", "probability.lower", "probability.midpoint")))
        if(chk != 0){
          stop(paste0("Scenario ",t," seems to have an unknown entry in its MAR component."))
        }
      }
      if(!is.null(scenarios[[t]]$MNAR)){
        chk = length(setdiff(names(scenarios[[t]]$MNAR), c("columns","probability.upper", "probability.lower", "probability.midpoint")))
        if(chk != 0){
          stop(paste0("Scenario ",t," seems to have an unknown entry in its MNAR component."))
        }
      }
    }


    ln = length(scenarios)
    for(t in 1:ln){
      scenario = scenarios[[t]]
      if(!is.null(scenario$MCAR) & !is.null(scenario$MCAR$columns) & !is.vector(scenario$MCAR$columns)){
        stop("MCAR$columns is not a vector.")
      }
      if(!is.null(scenario$MCAR) & !is.null(scenario$MCAR$probability)){
        if(( !is.vector(scenario$MCAR$probability) | min(scenario$MCAR$probability) < 0 )| max(scenario$MCAR$probability) > 1 ){
          stop("MCAR$probability is not a numeric vector with entries between 0 and 1.")
        }
      }
      if(!is.null(scenario$MAR) & !is.null(scenario$MAR$probability.upper)){
        if(( !is.vector(scenario$MAR$probability.upper) | min(scenario$MAR$probability.upper) < 0 )| max(scenario$MAR$probability.upper) > 1 ){
          stop("MAR$probability.upper is not a vector with entries between 0 and 1.")
        }
      }
      if(!is.null(scenario$MAR) & !is.null(scenario$MAR$probability.lower) ){
        if(( !is.vector(scenario$MAR$probability.lower) |  min(scenario$MAR$probability.lower) < 0 )| max(scenario$MAR$probability.lower) > 1 ){
          stop("MAR$probability.lower is not a vector with entries between 0 and 1.")
        }
      }
      if(!is.null(scenario$MAR) & !is.null(scenario$MAR$probability.midpoint) &  !is.list(scenario$MAR$probability.midpoint)){ # I do not check each list entry
        stop("MAR$probability.midpoint is not a list.")
      }
      if(!is.null(scenario$MNAR) & !is.null(scenario$MNAR$probability.upper) ){
        if(( !is.vector(scenario$MNAR$probability.upper) | min(scenario$MNAR$probability.upper) < 0 )| max(scenario$MNAR$probability.upper) > 1 ){
          stop("MNAR$probability.upper is not a vector with entries between 0 and 1.")
        }
      }
      if(!is.null(scenario$MNAR) & !is.null(scenario$MNAR$probability.lower)){
        if(( !is.vector(scenario$MNAR$probability.lower) | min(scenario$MNAR$probability.lower) < 0 )| max(scenario$MNAR$probability.lower) > 1 ){
          stop("MNAR$probability.lower is not a vector with entries between 0 and 1.")
        }
      }
      if(!is.null(scenario$MNAR) & !is.null(scenario$MNAR$probability.midpoint) &  !is.list(scenario$MNAR$probability.midpoint)){ # I do not check each list entry
        stop("MNAR$probability.midpoint is not a list.")
      }
      if(!is.null(scenario$MAR) & !is.null(scenario$MAR$reg.columns) &  !is.list(scenario$MAR$reg.columns)){ # I do not check each list entry
        stop("MAR$reg.columns is not a list.")
      }
      if(((!is.null(scenario$MAR) & !is.null(scenario$MAR$reg.columns)) & !is.null(scenario$MAR$probability.upper)) & (length(scenario$MAR$reg.columns) !=
                                                                                                                       length(scenario$MAR$probability.upper))){
        stop("MAR$reg.columns must have the same length as MAR$probability.upper")
      }
      if(((!is.null(scenario$MAR) & !is.null(scenario$MAR$probability.upper)) & !is.null(scenario$MAR$probability.lower)) & (length(scenario$MAR$probability.upper) !=
                                                                                                                             length(scenario$MAR$probability.lower))){
        stop("MAR$probability.upper must have the same length as MAR$probability.lower")
      }
      if(((!is.null(scenario$MAR) & !is.null(scenario$MAR$probability.midpoint)) & !is.null(scenario$MAR$probability.lower)) & (length(scenario$MAR$probability.midpoint) !=
                                                                                                                                length(scenario$MAR$probability.lower))){
        stop("MAR$probability.midpoint must have the same length as MAR$probability.lower")
      }
      if(((!is.null(scenario$MNAR) & !is.null(scenario$MNAR$probability.upper)) & !is.null(scenario$MNAR$columns)) & (length(scenario$MNAR$probability.upper) !=
                                                                                                                      length(scenario$MNAR$columns))){
        stop("MNAR$probability.upper must have the same length as MNAR$columns")
      }
      if(((!is.null(scenario$MNAR) & !is.null(scenario$MNAR$probability.upper)) & !is.null(scenario$MNAR$probability.lower)) & (length(scenario$MNAR$probability.upper) !=
                                                                                                                                length(scenario$MNAR$probability.lower))){
        stop("MNAR$probability.upper must have the same length as MNAR$probability.lower")
      }
      if(((!is.null(scenario$MNAR) & !is.null(scenario$MNAR$probability.midpoint)) & !is.null(scenario$MNAR$probability.lower)) & (length(scenario$MNAR$probability.midpoint) !=
                                                                                                                                   length(scenario$MNAR$probability.lower))){
        stop("MNAR$probability.midpoint must have the same length as MNAR$probability.lower")
      }
    }
  } else{if(!is.null(missingness_parameters)){

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
      # k = 1
      miss.p = missingness_parameters[[k]]
      if((!is.null(miss.p$MCAR) & !is.null(miss.p$MCAR$columns) )  & !is.vector(miss.p$MCAR$columns) ){
        stop("MCAR$columns is not a vector.")
      }
      if((!is.null(miss.p$MCAR) & !is.null(miss.p$MCAR$size) ) ){
        if(!(round(miss.p$MCAR$size) == miss.p$MCAR$size) ){
          stop("MCAR$size is not an integer")
        }
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
      if(!is.null(miss.p$MAR) & !is.null(miss.p$MAR$size) ){
        if(!(round(miss.p$MAR$size) == miss.p$MAR$size) ){
          stop("MAR$size is not an integer")
        }
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
      if(!is.null(miss.p$MNAR) & !is.null(miss.p$MNAR$size) ){
        if( !(round(miss.p$MNAR$size) == miss.p$MNAR$size) ){
          stop("MNAR$size is not an integer")
        }
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
  }

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
        stop("training is not a list.")
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
  # We need to check if any methods are given otherwise lists have to be initialized
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


  if(is.null(scenarios)){
    if(is.null(missingness_parameters)){

      exists.miss = matrixStats::colSums2(matrix(as.numeric(is.na(data)), ncol = ncol(data)))
      if(sum(exists.miss) == 0){
        missingness_parameters = list(MCAR = list(columns = 1:ncol(data),
                                                  size = ncol(data)/2,
                                                  probability = 0.25))
        scenarios = list("Default Scenario 1" = missingness_scenario_from_parameters(nbr_columns = ncol(data),
                                                                             missingness_parameters = missingness_parameters,
                                                                             seed = seed),
                         "Default Scenario 2" = missingness_scenario_from_parameters(nbr_columns = ncol(data),
                                                                             missingness_parameters = missingness_parameters,
                                                                             seed =     if(!is.null(seed)){seed + 10} else{seed}),
                         "Default Scenario 3" = missingness_scenario_from_parameters(nbr_columns = ncol(data),
                                                                             missingness_parameters = missingness_parameters,
                                                                             seed =     if(!is.null(seed)){seed + 20} else{seed}),
                         "Default Scenario 4" = missingness_scenario_from_parameters(nbr_columns = ncol(data),
                                                                             missingness_parameters = missingness_parameters,
                                                                             seed =     if(!is.null(seed)){seed + 30} else{NULL}))
      } else{
        clms.with.miss = which(exists.miss != 0)
        how.much.miss = exists.miss[clms.with.miss]/nrow(data)
        scenarios = list("Default Scenario" = list(MCAR = list( columns =  clms.with.miss,
                                                          probability = how.much.miss)))
      }
    } else{
      names_of_choices = names(missingness_parameters)
      k = length(missingness_parameters)
      scenarios = list()
      for(t in 1:k){
        scenarios[[t]] = missingness_scenario_from_parameters(nbr_columns = ncol(data),
                                                              missingness_parameters = missingness_parameters[[t]],
                                                              seed = if(!is.null(seed)){seed + 10*t} else{NULL})
        names(scenarios)[t] = names(missingness_parameters)[t]
      }

    }
  } else{
    if(!is.null(missingness_parameters)){
      warning("Since scenarios are defined, missingness_parameters will be ignored.")
    }
    ## Set Defaults for Scenarios
    ln = length(scenarios)
    for(k in 1:ln){
      # k = 1
      scenarios[[k]] = list(MCAR = if(!is.null(scenarios[[k]]$MCAR)){
        list( columns = if(!is.null(scenarios[[k]]$MCAR$columns)){scenarios[[k]]$MCAR$columns} else{sample(1:ncol(data), size = floor(ncol(data)/2))},
              probability = if(!is.null(scenarios[[k]]$MCAR$probability)){scenarios[[k]]$MCAR$probability} else{0.25})} else{NULL},
        MAR = if(!is.null(scenarios[[k]]$MAR)){
          list( columns = if(!is.null(scenarios[[k]]$MAR$columns)){scenarios[[k]]$MAR$columns} else{sample(1:ncol(data), size = floor(ncol(data)/2))},
                reg.columns = if(!is.null(scenarios[[k]]$MAR$reg.columns)){scenarios[[k]]$MAR$reg.columns} else{NULL},
                probability.upper = if(!is.null(scenarios[[k]]$MAR$probability.upper)){scenarios[[k]]$MAR$probability.upper} else{NULL},
                probability.lower = if(!is.null(scenarios[[k]]$MAR$probability.lower)){scenarios[[k]]$MAR$probability.lower} else{NULL},
                probability.midpoint = if(!is.null(scenarios[[k]]$MAR$probability.midpoint)){scenarios[[k]]$MAR$probability.midpoint} else{NULL})} else{NULL},
        MNAR = if(!is.null(scenarios[[k]]$MNAR)){
          list( columns = if(!is.null(scenarios[[k]]$MNAR$columns)){scenarios[[k]]$MNAR$columns} else{sample(1:ncol(data), size = floor(ncol(data)/2))},
                probability.upper = if(!is.null(scenarios[[k]]$MNAR$probability.upper)){scenarios[[k]]$MNAR$probability.upper} else{NULL},
                probability.lower = if(!is.null(scenarios[[k]]$MNAR$probability.lower)){scenarios[[k]]$MNAR$probability.lower} else{NULL},
                probability.midpoint = if(!is.null(scenarios[[k]]$MNAR$probability.midpoint)){scenarios[[k]]$MNAR$probability.midpoint} else{NULL})} else{NULL})

      if(!is.null(scenarios[[k]]$MAR)){
        if(is.null(scenarios[[k]]$MAR$reg.columns) |  is.null(scenarios[[k]]$MAR$probability.upper) |
           is.null(scenarios[[k]]$MAR$probability.lower) | is.null(scenarios[[k]]$MAR$probability.midpoint)){
          sz = length(scenarios[[k]]$MAR$columns)
          if(is.null(scenarios[[k]]$MAR$reg.columns)){
            scenarios[[k]]$MAR$reg.columns = list()
            for(tau in 1:sz){
              how.many = sample(c(1,2,3), size = 1)
              scenarios[[k]]$MAR$reg.columns[[tau]] = sample(1:ncol(data), size = how.many, replace = FALSE )
            }
          }
          if(is.null(scenarios[[k]]$MAR$probability.upper)){
            scenarios[[k]]$MAR$probability.upper = 0.8
          }
          if(is.null(scenarios[[k]]$MAR$probability.lower)){
            scenarios[[k]]$MAR$probability.lower = 0.2
          }
          if(is.null(scenarios[[k]]$MAR$probability.midpoint)){
            scenarios[[k]]$MAR$probability.midpoint = list()
            for(tau in 1:sz){
              scenarios[[k]]$MAR$probability.midpoint[[tau]] = c(0.5,0.5)
            }
          }
        }
      }
      if(!is.null(scenarios[[k]]$MNAR)){
        if(is.null(scenarios[[k]]$MNAR$probability.upper) |
           is.null(scenarios[[k]]$MNAR$probability.lower) | is.null(scenarios[[k]]$MNAR$probability.midpoint)){
          sz = length(scenarios[[k]]$MNAR$columns)
          if(is.null(scenarios[[k]]$MNAR$probability.upper)){
            scenarios[[k]]$MNAR$probability.upper = 0.8
          }
          if(is.null(scenarios[[k]]$MNAR$probability.lower)){
            scenarios[[k]]$MNAR$probability.lower = 0.2
          }
          if(is.null(scenarios[[k]]$MNAR$probability.midpoint)){
            scenarios[[k]]$MNAR$probability.midpoint = list()
            for(tau in 1:sz){
              scenarios[[k]]$MNAR$probability.midpoint[[tau]] = c(0.5,0.5)
            }
          }
        }
      }

    }

  }

  # We also need a default for the error_grouping
  if(is.null(error_grouping)){
    error_grouping = list(1:base::ncol(data))
  }
  runs = length(scenarios)

  data.list = vector(mode = "list", (length = runs + 1))
  if(is.null(scenarios)){
    if(is.null(missingness_parameters)){
      names(data.list) = c("Overview_Scenarios", paste0("Scenario_", 1:runs))
    } else{
      if(is.null(names(missingness_parameters))){
        names(data.list) = c("Overview_Scenarios", paste0("Scenario_", 1:runs))
      } else{
        names(data.list)[1] = "Overview_Scenarios"
        names(data.list)[2:(runs+1)] = names(missingness_parameters)
      }
    }
  } else{
    if(is.null(names(scenarios))){
      names(data.list) = c("Overview_Scenarios", paste0("Scenario_", 1:runs))
    } else{
      names(data.list)[1] = "Overview_Scenarios"
      names(data.list)[2:(runs+1)] = names(scenarios)
    }
  }

  data.list[[1]] = scenarios

  for(tau in 1:runs){
    # tau = 1
    if(verbose){
      print(paste("Starting Scenario ", tau, " of ", runs, sep = ""))
    }
    if(!is.null(seed)){
      seed = seed + 10000*tau
    }
    Results = scenario_simulation_study_oneRun(data = data,
                                               methods = methods,
                                               method.names = method.names,
                                               training = training,
                                               scenario = scenarios[[tau]],
                                               scenario.name = names(scenarios)[tau],
                                               repetitions = repetitions,
                                               error_grouping = error_grouping,
                                               error_measure = error_measure,
                                               scaling_robust = scaling_robust,
                                               retrain.always = retrain_always,
                                               verbose = verbose,
                                               seed = seed,
                                               run.nbr = tau,
                                               max.time = max_time,
                                               output_data = output_data)

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
        data.list[[tau+1]] = Results$data.list
      } else{
        Simulation.Results = rbind(Simulation.Results,Results)
      }
    }
  }
  if(!is.null(error_grouping)){
    if(!is.null(names(error_grouping))){
      colnames(evaluation)[7:(ncol(evaluation)-1)] = names(error_grouping)
    }
  }

  if(output_data){
    Simulation.Results = list(evaluation = evaluation,
                              data.list = data.list)
  }
  return(Simulation.Results)
}


