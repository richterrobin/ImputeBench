

#' Drawing a Missingness Scenario from Missingness Parameters
#'
#' @description Given missingness parameters this function draws a missingness scenario.
#'
#' @param nbr_columns The amount of columns of the to be masked numeric matrix (needed for the defaul values).
#' @param missingness_parameters A lists of parameters defining the way the missingness scenario is drawn, for details see
#' below and the vignette. Default is \code{NULL}, for the default choice see the vignette.
#' @param seed An integer, setting a seed.
#'
#' @details For the details on the definition of missingness scenarios and the protocol by which they are drawn from missingness
#' parameter we refer to the accompanying paper RMTBM22. Find below an example of a `missingness_parameters` list of parameters that
#' can be passed to `missingness_scenario_from_parameters`:
#'
#' ```
#' missingness_parameters = list("MCAR" = list(columns = 1:10,
#'                                             size = 10,
#'                                             probability = 0.05),
#'                               "MAR" = list(columns = 2:4,
#'                                            size = 3,
#'                                            size.regressors = c(1,2),
#'                                            probability.upper = 0.9,
#'                                            probability.lower = 0.1,
#'                                            probability.midpoint = c(0.5,0.2)),
#'                               "MNAR" = list(columns = c(5,10),
#'                                             size = 2,
#'                                             probability.upper = 0.05,
#'                                             probability.lower = 0.95,
#'                                             probability.midpoint = c(0.5,0.3))
#' ```
#'
#' @seealso `data_ImputeBench`
#'
#' @references Robin Richter, Anne Miloschewski, Juliana F. Tavares, Monique M.B. Breteler and Sach Mukherjee, "Benchmarking Single
#' Imputation Methods", in preparation.
#'
#' @return A missingness scenario.
#'
#' @export
#'


missingness_scenario_from_parameters = function(nbr_columns,
                                                missingness_parameters = NULL,
                                                seed = NULL){


  if(is.null(missingness_parameters)){
    missingness_parameters = list(MCAR = list(columns = 1:nbr_columns,
                                              size = nbr_columns/2,
                                              probability = 0.25))
  }

  # Check for not used entries
  chk = length(setdiff(names(missingness_parameters),c("MCAR","MAR","MNAR")))
  if(chk != 0){
    stop(paste0("missingness_parameters seems to have an unknown entry."))
  }
  if(!is.null(missingness_parameters$MCAR)){
    chk = length(setdiff(names(missingness_parameters$MCAR), c("columns", "size", "probability")))
    if(chk != 0){
      stop(paste0("missingness_parameters seems to have an unknown entry in its MCAR component."))
    }
  }
  if(!is.null(missingness_parameters$MAR)){
    chk = length(setdiff(names(missingness_parameters$MAR), c("columns", "size", "size.regressors" ,"probability.upper", "probability.lower", "probability.midpoint")))
    if(chk != 0){
      stop(paste0("missingness_parameters seems to have an unknown entry in its MAR component."))
    }
  }
  if(!is.null(missingness_parameters$MNAR)){
    chk = length(setdiff(names(missingness_parameters$MNAR), c("columns", "size" ,"probability.upper", "probability.lower", "probability.midpoint")))
    if(chk != 0){
      stop(paste0("missingness_parameters seems to have an unknown entry in its MNAR component."))

    }
  }

  p = nbr_columns
  base::set.seed(seed)
  scenario = list()

  if("MCAR" %in% names(missingness_parameters)){

    if(is.null(missingness_parameters$MCAR)){
      scenarios$MCAR = NULL
    } else{

      missingness_parameters$MCAR = list( columns = if(is.null(missingness_parameters$MCAR$columns)){ 1:nbr_columns
      } else{missingness_parameters$MCAR$columns},
      size = if(is.null(missingness_parameters$MCAR$size)){ max(floor(nbr_columns/2), length(missingness_parameters$MCAR$columns))
      } else{missingness_parameters$MCAR$size},
      probability = if(is.null(missingness_parameters$MCAR$probability)){0.25
      } else{missingness_parameters$MCAR$probability})

      scenario$MCAR = list(columns = sample(missingness_parameters$MCAR$columns,
                                            size = missingness_parameters$MCAR$size , replace = FALSE),
                           probability = rep(missingness_parameters$MCAR$probability,
                                             times =  missingness_parameters$MCAR$size ))
    }
  }
  if("MAR" %in% names(missingness_parameters)){

    if(is.null(missingness_parameters$MAR)){
      scenarios$MCAR = NULL
    } else{

      missingness_parameters$MAR = list( columns = if(is.null(missingness_parameters$MAR$columns)){ 1:nbr_columns
      } else{missingness_parameters$MAR$columns},
      size = if(is.null(missingness_parameters$MAR$size)){ max(floor(nbr_columns/2), length(missingness_parameters$MAR$columns))
      } else{missingness_parameters$MAR$size},
      size.regressors = if(is.null(missingness_parameters$MAR$size.regressors)){ c(1,2,3)
      } else{missingness_parameters$MAR$size.regressors},
      probability.upper = if(is.null(missingness_parameters$MAR$probability.upper)){ 0.8
      } else{missingness_parameters$MAR$probability.upper},
      probability.lower = if(is.null(missingness_parameters$MAR$probability.lower)){ 0.2
      } else{missingness_parameters$MAR$probability.lower},
      probability.midpoint = if(is.null(missingness_parameters$MAR$probability.midpoint)){ c(0.5,0.5)
      } else{missingness_parameters$MAR$probability.midpoint})

      scenario$MAR = list(columns = sample(missingness_parameters$MAR$columns,
                                           size = missingness_parameters$MAR$size , replace = FALSE),
                          reg.columns = list(),
                          probability.upper = rep(missingness_parameters$MAR$probability.upper,
                                                  times =  missingness_parameters$MAR$size ),
                          probability.lower = rep(missingness_parameters$MAR$probability.lower,
                                                  times =  missingness_parameters$MAR$size ),
                          probability.midpoint = list())
      poss.reg.size = missingness_parameters$MAR$size.regressors

      for(t in 1:length(scenario$MAR$columns)){
        # t = 1
        how.many = sample(missingness_parameters$MAR$size.regressors, size = 1)
        setdiff(1:p, scenario$MAR$columns[t])
        scenario$MAR$reg.columns[[t]] = sample(setdiff(1:p, scenario$MAR$columns[t]), size = how.many, replace = FALSE)
        scenario$MAR$probability.midpoint[[t]] = missingness_parameters$MAR$probability.midpoint
      }
    }
  }
  if("MNAR" %in% names(missingness_parameters)){

    if(is.null(missingness_parameters$MNAR)){
      scenarios$MCAR = NULL
    } else{

      missingness_parameters$MNAR = list( columns = if(is.null(missingness_parameters$MNAR$columns)){ 1:nbr_columns
      } else{missingness_parameters$MNAR$columns},
      size = if(is.null(missingness_parameters$MNAR$size)){ max(floor(nbr_columns/2), length(missingness_parameters$MNAR$columns))
      } else{missingness_parameters$MNAR$size},
      probability.upper = if(is.null(missingness_parameters$MNAR$probability.upper)){ 0.8
      } else{missingness_parameters$MNAR$probability.upper},
      probability.lower = if(is.null(missingness_parameters$MNAR$probability.lower)){ 0.2
      } else{missingness_parameters$MNAR$probability.lower},
      probability.midpoint = if(is.null(missingness_parameters$MNAR$probability.midpoint)){ c(0.5,0.5)
      } else{missingness_parameters$MNAR$probability.midpoint})

      scenario$MNAR = list(columns = sample(missingness_parameters$MNAR$columns,
                                            size = missingness_parameters$MNAR$size , replace = FALSE),
                           probability.upper = rep(missingness_parameters$MNAR$probability.upper,
                                                   times =  missingness_parameters$MNAR$size ),
                           probability.lower = rep(missingness_parameters$MNAR$probability.lower,
                                                   times =  missingness_parameters$MNAR$size ),
                           probability.midpoint = list())
      for(t in 1:length(scenario$MNAR$columns)){
        scenario$MNAR$probability.midpoint[[t]] = missingness_parameters$MNAR$probability.midpoint
      }
    }
  }

  return(scenario)

}
