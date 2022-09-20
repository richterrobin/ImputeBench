
#' One Run of the Benchmarking of `data_ImputeBench`
#'
#' @description Internal function of ImputeBench computing imputation error for a single scenario/missingness parameters within
#' the benchmarking protocol of `data_ImputeBench`.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries.
#' @param methods A list of functions, taking as an input a numeric matrix with missing entries via an argument `data` and a list of
#' parameters via an argument `args`. Default is \code{NULL}, i.e. no additional methods added to the benchmarking.
#' @param method.names A vector of character strings, corresponding to the names of `methods`.
#' @param training A list of functions - which need to be of the same length as the `methods` list -, that take as an input
#' a data matrix with missing entries via an argument `data` and outputs the parameters for the respective method via as a list `args`.
#' @param scenario A list constituting a missingness scenario (parameters thereof), for details and an example see the vignette or
#' the documentation of `data_ImputeBench`.
#' @param scenario.name A character string giving the name of the `scenario`.
#' @param repetitions Integer determining how often an additional missingness pattern according to one entry of `scenarios` or  `missingness_parameters`
#' is drawn. Default is `5`.
#' @param error_grouping A list of indice vectors defining column groups. Errors for each groups are collected and the overall error
#' is computed by the mean error over all groups (weighting all groups the same irrespective of the number of columns they contain).
#' @param error_measure Character string that must be `"l2"` (up to now only error measure supported). Default is `"l2"`.
#' @param scaling_robust Positive numeric, robustness parameter avoiding division by close to zero when the columns are scaled by the standard deviation
#' when computing the imputation error. Default is `0.01`.
#' @param retrain.always Boolean, should parameters for each imputation method be learned for every run or only every scenario/parameter choice?
#' Default is \code{FALSE}.
#' @param verbose Boolean, determines if an output to the console is made each time a run is finished. Default is \code{FALSE}.
#' @param seed Integer or `NULL`, sets a seed. Default is \code{NULL}.
#' @param run.nbr Integer indicating the number of run in the simulation study. Default is \code{1}.
#' @param max.time The maximal amount of seconds an imputation method is allowed to take. Default is \code{2592000}, i.e. 30 days.
#' @param output_data Boolean, should additional data be collected, i.e. drawn missingness pattern? Default is `TRUE`.
#'
#' @references R. Richter, A. Miloschewski, J.F. Tavares, M.M.B. Breteler, S. Mukherjee (2012), Benchmarking Single
#' Imputation Methods. in preparation.
#'
#' @return Results of one scenario/missingness parameters of `data_ImputeBench`.
#'


scenario_simulation_study_oneRun = function(data,
                                            methods,
                                            method.names,
                                            training,
                                            scenario,
                                            scenario.name,
                                            repetitions = 5,
                                            error_grouping,
                                            error_measure = "l2",
                                            scaling_robust = 0.01,
                                            retrain.always = FALSE,
                                            verbose = FALSE,
                                            seed = NULL,
                                            run.nbr = 1,
                                            max.time = 2592000,
                                            output_data = TRUE){
  set.seed(seed)
  # We need to know how many imputation performances we need to measure plus median imputation
  nbr.competitors = base::length(methods)
  # We need an evaluation table to fill
  evaluation = data.frame(ID = base::rep(NA, times = repetitions*nbr.competitors),
                          scenario = base::rep(scenario.name, times = repetitions*nbr.competitors),
                          method = base::rep(NA, times = repetitions*nbr.competitors),
                          nbr.still.missing = base::rep(NA, times = repetitions*nbr.competitors),
                          computation.time = base::rep(NA, times = repetitions*nbr.competitors),
                          missing.entries = base::rep(NA, times = repetitions*nbr.competitors),
                          imputation.error.overall = base::rep(NA, times = repetitions*nbr.competitors))
  for(l in 1:length(error_grouping)){
    evaluation[,(6+l)] = base::rep(NA, times = repetitions*nbr.competitors)
    names(evaluation)[6+l] = base::paste("imputation.error.group.",base::as.character(l),sep="")
  }
  data.list = list()

  for(k in 1:repetitions){
    # k = 1
    if(!is.null(seed)){
    seed = seed + (1000*k)
    }
    if(verbose){
      base::print(base::paste("Starting run ", k, " of ", repetitions, " for Scenario ", run.nbr, sep = ""))
    }
    # First we need to ensure the data is numeric and a matrix
    data.mtrx = base::as.matrix(data)
    counts.and.binaries = determine_count_binary(data = data.mtrx)
    counts = counts.and.binaries$count.columns
    binaries = counts.and.binaries$binary.columns
    # We need to draw a missingness pattern on the data
    mask.mtrx = simulate_mask(data = data.mtrx,
                              scenario = scenario,
                              seed = seed)

    masked.data.mtrx = data.mtrx
    for(clmn in 1:base::ncol(data.mtrx)){
      masked.data.mtrx[base::which(mask.mtrx[,clmn] == 0),clmn] = NA
    }
    nbr.missing = base::length(base::which(mask.mtrx == 0| !is.na(data.mtrx) ))
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
                          mask = mask.mtrx,
                          args = method_arguments)
    }
    # Given the method arguments we now need the imputed matrices
    imputation.results = imputation_wrapper(data = masked.data.mtrx,
                                            methods = methods,
                                            method.names = method.names,
                                            method_arguments = method_arguments,
                                            only.count.columns = base::setdiff(counts, binaries),
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
      # l = 1
      row.nbr = (k-1)*nbr.competitors + l
      evaluation$nbr.still.missing[row.nbr] = nbr.still.missing[l]
      evaluation$missing.entries[row.nbr] = nbr.missing
      evaluation$ID[row.nbr] = base::paste(base::as.character(run.nbr), base::as.character(k),sep = ".")
      evaluation$method[row.nbr] = method.names[l]
      evaluation$computation.time[row.nbr] = if(base::is.null(computation.time[[l]])){ NA } else{ computation.time[[l]] }
      if(!base::is.null(imputations[[l]])){
        results_of_evaluation = switch(error_measure,
                                       l2 = group_RMSEscaled_onmask(data = data.mtrx,
                                                                    imputed_data = imputations[[l]],
                                                                    scaling_type = "StandardDev",
                                                                    mask = mask.mtrx,
                                                                    scaling_robust = scaling_robust,
                                                                    error_grouping = error_grouping))
        evaluation$imputation.error.overall[row.nbr] = results_of_evaluation$overall
        if(base::length(error_grouping) == 1){
          evaluation[row.nbr,7] = results_of_evaluation$group_wise
        } else{
          for(s in 1:base::length(error_grouping)){
            evaluation[row.nbr,(6 + s)] = results_of_evaluation$group_wise[s]
          }
        }
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

