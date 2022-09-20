

#' One Run of the Benchmarking of `simulation_ImputeBench`
#'
#' @description Internal function of ImputeBench computing imputation error for a single scenario/missingness parameters within
#' the benchmarking protocol of `simulation_ImputeBench`.
#'
#' @param methods A list of functions, taking as an input a numeric matrix with missing entries via an argument `data` and a list of
#' parameters via an argument `args`. Default is \code{NULL}, i.e. no additional methods added to the benchmarking.
#' @param method.names A vector of character strings, corresponding to the names of `methods`.
#' @param training A list of functions - which need to be of the same length as the `methods` list -, that take as an input
#' a data matrix with missing entries via an argument `data` and outputs the parameters for the respective method via as a list `args`.
#' @param data_parameters A named list of data parameters, for details and an example see the vignette or the documentation of
#' `simulation_ImputeBench`.
#' @param missingness_parameters A named list of missingness pattern parameters, for details and an example see the vignette or
#' the documentation of `simulation_ImputeBench`.
#' @param repetitions Integer determining how often an additional missingness pattern according to one entry of `scenarios` or  `missingness_parameters`
#' is drawn. Default is `5`.
#' @param error_measure Character string that must be `"l2"` (up to now only error measure supported). Default is `"l2"`.
#' @param scaling_robust Positive numeric, robustness parameter avoiding division by close to zero when the columns are scaled by the standard deviation
#' when computing the imputation error. Default is `0.01`.
#' @param retrain.always Boolean, should parameters for each imputation method be learned for every run or only every scenario/parameter choice?
#' Default is `FALSE`.
#' @param verbose Boolean, determines if an output to the console is made each time a run is finished. Default is \code{FALSE}.
#' @param seed Integer or `NULL`, sets a seed. Default is \code{NULL}.
#' @param run.nbr Integer indicating the number of run in the simulation study. Default is \code{1}.
#' @param max.time The maximal amount of seconds an imputation method is allowed to take. Default is \code{2592000}, i.e. 30 days.
#' @param output_data Boolean, should additional data be collected, i.e. drawn missingness pattern? Default is `TRUE`.
#'
#' @references R. Richter, A. Miloschewski, J.F. Tavares, M.M.B. Breteler, S. Mukherjee (2012), Benchmarking Single
#' Imputation Methods. in preparation.
#'
#' @return Results of one data and missingness parameters of `simulation_ImputeBench`.
#'

varying_simulation_study_oneRun = function(methods,
                                           method.names,
                                           training,
                                           data_parameters,
                                           missingness_parameters,
                                           repetitions = 5,
                                           error_measure = "l2",
                                           scaling_robust = 0.01,
                                           retrain.always = FALSE,
                                           verbose = FALSE,
                                           seed = NULL,
                                           run.nbr = 1,
                                           max.time = 2592000,
                                           output_data = TRUE){
  # We need to know how many imputation performances we need to measure plus median imputation
  nbr.competitors = base::length(methods)
  # We need an evaluation table to fill
  # repetitions*nbr.competitors
  # base::rep(if(is.null(missingness_parameters$MAR)){0} else{missingness_parameters$MCAR$size}, times = repetitions*nbr.competitors )
  evaluation = data.frame(ID = base::rep(NA, times = repetitions*nbr.competitors),
                          method = base::rep(NA, times = repetitions*nbr.competitors),
                          imputation.error.overall = base::rep(NA, times = repetitions*nbr.competitors),
                          imputation.error.gauss = base::rep(NA, times = repetitions*nbr.competitors),
                          imputation.error.pois = base::rep(NA, times = repetitions*nbr.competitors),
                          imputation.error.binary.mse = base::rep(NA, times = repetitions*nbr.competitors),
                          imputation.error.binary.logodds = base::rep(NA, times = repetitions*nbr.competitors),
                          imputation.error.t = base::rep(NA, times = repetitions*nbr.competitors),
                          imputation.error.sine = base::rep(NA, times = repetitions*nbr.competitors),
                          imputation.error.spline = base::rep(NA, times = repetitions*nbr.competitors),
                          imputation.error.continuous = base::rep(NA, times = repetitions*nbr.competitors),
                          nbr.still.missing = base::rep(NA, times = repetitions*nbr.competitors),
                          computation.time = base::rep(NA, times = repetitions*nbr.competitors),
                          missing.entries = base::rep(NA, times = repetitions*nbr.competitors),
                          gaussian.columns = base::rep(NA, times = repetitions*nbr.competitors),
                          poisson.columns = base::rep(NA, times = repetitions*nbr.competitors),
                          binary.columns = base::rep(NA, times = repetitions*nbr.competitors),
                          t.columns = base::rep(NA, times = repetitions*nbr.competitors),
                          sine.columns = base::rep(NA, times = repetitions*nbr.competitors),
                          spline.columns = base::rep(NA, times = repetitions*nbr.competitors),
                          mask.MCAR.columns = base::rep(if(is.null(missingness_parameters$MCAR)){0} else{missingness_parameters$MCAR$size}, times = repetitions*nbr.competitors ),
                          mask.MAR.columns = base::rep(if(is.null(missingness_parameters$MAR)){0}else{ missingness_parameters$MAR$size}, times = repetitions*nbr.competitors ),
                          mask.MNAR.columns = base::rep(if(is.null(missingness_parameters$MNAR)){0}else{ missingness_parameters$MNAR$size}, times = repetitions*nbr.competitors ),
                          mask.MCAR.probability = base::rep(if(is.null(missingness_parameters$MCAR)){0}else{ missingness_parameters$MCAR$probability}, times = repetitions*nbr.competitors ),
                          mask.MAR.max.probability = base::rep(if(is.null(missingness_parameters$MAR)){0}else{ missingness_parameters$MAR$probability.upper}, times = repetitions*nbr.competitors ),
                          mask.MAR.min.probability = base::rep(if(is.null(missingness_parameters$MNAR)){0}else{ missingness_parameters$MNAR$probability.upper}, times = repetitions*nbr.competitors ),
                          mask.MNAR.max.probability = base::rep(if(is.null(missingness_parameters$MAR)){0}else{ missingness_parameters$MAR$probability.lower}, times = repetitions*nbr.competitors ),
                          mask.MNAR.min.probability = base::rep(if(is.null(missingness_parameters$MNAR)){0}else{ missingness_parameters$MNAR$probability.lower}, times = repetitions*nbr.competitors ),
                          nbr.columns = base::rep(sum(data_parameters$data.type), times = repetitions*nbr.competitors),
                          nbr.rows = base::rep(data_parameters$data.size, times = repetitions*nbr.competitors),
                          data.spars = base::rep(data_parameters$data.spars, times = repetitions*nbr.competitors),
                          data.low.dim = base::rep(data_parameters$data.low.dim, times = repetitions*nbr.competitors),
                          data.low.dim.groups = base::rep(data_parameters$data.low.dim.groups, times = repetitions*nbr.competitors),
                          data.non.linearity = base::rep(data_parameters$data.non.linearity, times = repetitions*nbr.competitors),
                          data.groups = base::rep(data_parameters$data.groups, times = repetitions*nbr.competitors),
                          data.groups.distance = base::rep((1/2)*(sum(data_parameters$data.groups.distance)) , times = repetitions*nbr.competitors),
                          data.low.dim.errorStD = base::rep(data_parameters$data.low.dim.parameters$error.std, times = repetitions*nbr.competitors))
  data.list = list()

  for(k in 1:repetitions){
    if(!is.null(seed)){
      seed = seed + (k*1000)
    }
    # k = 1
    # run.nbr = 1
    if(verbose){
      base::print(base::paste("Starting run ", k, " of ", repetitions, " for parameter choice ", run.nbr, sep = ""))
    }
    # First we need to sample data
    data.mtrx = simulate_data(data_parameters = data_parameters,
                              seed = seed)
    counts.and.binaries = determine_count_binary(data = data.mtrx)
    count.columns = counts.and.binaries$count.columns
    binaries = counts.and.binaries$binary.columns
    # We need to draw a missingness pattern on the data
    missingness.scenario = missingness_scenario_from_parameters(nbr_columns = ncol(data.mtrx),
                                                                missingness_parameters = missingness_parameters,
                                                                seed =     if(!is.null(seed)){seed + 100} else{NULL})
    mask.mtrx = simulate_mask(data = data.mtrx,
                              scenario = missingness.scenario,
                              seed =     if(!is.null(seed)){seed + 200} else{NULL})
    masked.data.mtrx = data.mtrx
    for(clmn in 1:base::ncol(data.mtrx)){
      masked.data.mtrx[base::which(mask.mtrx[,clmn] == 0),clmn] = NA
    }
    evaluation$missing.entries[((k-1)*nbr.competitors + 1):(k*nbr.competitors)] = base::rep(base::length(base::which(mask.mtrx == 0)),
                                                                                            times = nbr.competitors)
    # We need to train the imputation parameters
    if(k == 1){
      method_arguments = imputation_training(masked.data = masked.data.mtrx,
                                            methods = methods,
                                            training = training)
    } else{
      if(retrain.always){
        method_arguments = imputation_training(masked.data = masked.data.mtrx,
                                              methods = methods,
                                              training = training)
      }
    }
    if(output_data){
      data.list[[k]] = list(ID = base::paste(base::as.character(run.nbr), base::as.character(k), sep = "."),
                            data = data.mtrx,
                            mask = mask.mtrx,
                            args = method_arguments)
    }
    # Given the method arguments we now need the imputed matrices
    imputation.results = imputation_wrapper(data = masked.data.mtrx,
                                            methods = methods,
                                            method.names = method.names,
                                            method_arguments = method_arguments,
                                            only.count.columns = base::setdiff(count.columns, binaries),
                                            binary.columns = binaries,
                                            max.time = max.time)
    imputations = imputation.results$imputations
    computation.time = imputation.results$computation.time
    # We need to somehow fill all remaining NA's: I suggest we do this via median:
    nbr.still.missing = rep(0, times = nbr.competitors)
    for(ffill in 1:(nbr.competitors-1)){
      if(!base::is.null(imputations[[ffill]])){
        nbr.still.missing[ffill] = base::sum(as.numeric(is.na(imputations[[ffill]])))
        if(nbr.still.missing[ffill] != 0){
          imputations[[ffill]] = baseline_imputation(data = imputations[[ffill]], args = list(method = "median"))
        }
      }
    }
    # Now we need to extract the results and fill in our data frame
    for(l in 1:nbr.competitors){
      row.nbr = (k-1)*nbr.competitors + l
      evaluation$nbr.still.missing[row.nbr] = nbr.still.missing[l]
      data.grouping = data_parameters$data.type
      evaluation$gaussian.columns[row.nbr] = data.grouping[1]
      evaluation$poisson.columns[row.nbr] = data.grouping[2]
      evaluation$binary.columns[row.nbr] = data.grouping[3]
      evaluation$t.columns[row.nbr] = data.grouping[4]
      evaluation$sine.columns[row.nbr] = data.grouping[5]
      evaluation$spline.columns[row.nbr] = data.grouping[6]
      evaluation$ID[row.nbr] = base::paste(base::as.character(run.nbr), base::as.character(k),sep = ".")
      evaluation$method[row.nbr] = method.names[l]
      evaluation$computation.time[row.nbr] = if(base::is.null(computation.time[[l]])){ NA } else{ computation.time[[l]] }
      if(!base::is.null(imputations[[l]])){
        results_of_evaluation = evaluate_imputation_sim(data = data.mtrx,
                                                        mask = mask.mtrx,
                                                        imputed_data = imputations[[l]],
                                                        error_measure = error_measure,
                                                        data.grouping = data.grouping,
                                                        scaling_robust = scaling_robust)
        evaluation$imputation.error.overall[row.nbr] = results_of_evaluation$overall.error
        evaluation$imputation.error.gauss[row.nbr] = results_of_evaluation$error.gauss
        evaluation$imputation.error.pois[row.nbr] = results_of_evaluation$error.pois
        evaluation$imputation.error.binary.mse[row.nbr] = results_of_evaluation$error.binary.mse
        evaluation$imputation.error.binary.logodds[row.nbr] = results_of_evaluation$error.binary.logodds
        evaluation$imputation.error.t[row.nbr] = results_of_evaluation$error.t
        evaluation$imputation.error.sine[row.nbr] = results_of_evaluation$error.sine
        evaluation$imputation.error.spline[row.nbr] = results_of_evaluation$error.spline
        evaluation$imputation.error.continuous[row.nbr] = results_of_evaluation$error.continuous
      }
    }
  }
  if(output_data){
    Results = list(evaluation = evaluation,
                   data.list = data.list)
  } else{
    Results = evaluation
  }
  return(Results)
}


