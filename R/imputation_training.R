
#' Training the Arguments of Imputation Methods
#'
#' @description Internal function of ImputeBench calling the training function for all imputation methods and outputting the list
#' of trained parameters.
#'
#' @param masked.data A numeric matrix with missing entries given by `NA`.
#' @param methods A list of functions, each entry taking as input a numeric matrix with missing entries via `data` and a list of
#' parameters via `args` and outputting an imputed matrix.
#' @param training A list of functions, each entry taking as input a numeric matrix with missing entries via `data` and outputting
#' a list of parameters `args`, which is suited to be passed to the corresponding (via indexing) entry in `methods`.
#'
#' @return A list of parameters (giving as a list) for each entry in `method`.
#'


imputation_training = function(masked.data,
                               methods,
                               training){
  # We need an empty list to fill with the parameters for each method
  methods_parameters = list()
  # Now we can use the given training methods
  nbr.methods = base::length(methods)
  # We need to make sure that the training list is as long as the methods list
  if(base::length(training) != nbr.methods){
    training[[nbr.methods + 1]] = "End"
  }
  for(t in 1:nbr.methods){
    if(base::is.null(training[[t]])){
      methods_parameters[[t]] = NULL
    } else{
      methods_parameters[[t]] = base::do.call(training[[t]],args = list(masked.data))
    }
  }
  return(methods_parameters)
}








