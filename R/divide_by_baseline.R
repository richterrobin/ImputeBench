
#' Dividing all Imputation Errors by the Imputation Error of the Baseline
#'
#' @description ImputeBench internal function using the $evaluation list entry of the output of `data_ImputeBench` or
#' `simulation_ImputeBench` and scaling the imputation errors with the imputation error of the baseline imputation method.
#'
#' @param Evaluation The output table of `data_ImputeBench` or `simulation_ImputeBench`.
#'
#' @return The same table with scaled RMSE results
#'
#' @import stringr
#'


divide_by_baseline = function(Evaluation){

  if(!("Setting" %in% names(Evaluation))){
  Evaluation[,c(base::ncol(Evaluation)+1,base::ncol(Evaluation)+2)] =
    stringr::str_split_fixed(string = Evaluation$ID, pattern = "\\.", n = 2)
  names(Evaluation)[c(base::ncol(Evaluation)-1,base::ncol(Evaluation))] = c("Setting","Run")
  }
  n = base::nrow(Evaluation)
  nbr.competing = base::as.numeric(base::length(unique(Evaluation$method)))
  #nbr.settings = base::as.numeric(base::max(Evaluation$Setting))
  error.clms = base::setdiff(stringr::str_which(names(Evaluation),pattern = "imputation.error"),
                 stringr::str_which(names(Evaluation), pattern = "logodds"))
  for(rw in 1:n){
    # rw = 1
    competitor = (rw %% nbr.competing)
    if(competitor != 0){
      Evaluation[rw,error.clms] = Evaluation[rw,error.clms] / Evaluation[(rw + nbr.competing - competitor), error.clms]
    }
  }
  return(Evaluation)
}


