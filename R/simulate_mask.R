

#' Missingness Pattern Simulation Protocol of ImputeBench
#'
#' @description Simulating a missingness pattern given a missingness scenario according to the missingness pattern simulation
#' protocol of RMTBM22.
#'
#' @param data A matrix with numeric entries or `NA` entries marking missing entries.
#' @param scenario A named list defining the missingness scenario, for details see below and the accompanying vignette.
#' Default is given further below.
#' @param seed An integer, sets a seed for the simulation. Default is `NULL`.
#'
#' @details The parameter `scenario` needs to be a list specifying the missingness scenario (as defined in RMTBM22) guiding
#' the missingness pattern simulation protocol defined in RMTBM22. Not all parameters have to be specified, missing ones are
#' replaced by default ones. For a list and discussion of the parameters we refer to the accompanying vignette and RMTBM22.
#' Here we just give an example of `scenario` for a data matrix with at least 9 columns:
#'
#' ```
#' scenario = list(MCAR = list(columns = c(1,2,4,6),
#'                             probability = c(0.4,0.4,0.5,0.6)),
#'                 MAR = list(columns = c(5,7),
#'                            reg.columns = list(c(1,2,3),
#'                                               c(3,6)),
#'                            probability.upper = c(0.9,0.8),
#'                            probability.midpoint = list(c(0.8,0.25),
#'                                                        c(0.75,0.3)),
#'                            probability.lower = c(0.1,0.2)),
#'                 MNAR = list(columns = c(8,9),
#'                             probability.upper = c(0.9,0.95),
#'                             probability.midpoint = list(c(0.8,0.25),
#'                                                         c(0.75,0.3)),
#'                             probability.lower = c(0.1,0.1))),
#'
#' ```
#'
#' The default of `scenario` is given by the missingness pattern present in `data`: For each column the percentage of missing
#' entries is calculated. In the case there are missing entries the columns index is written into `MCAR$columns` and the percentage
#' of missing entries is denoted in the same-index entry of `MCAR$probability`.
#'
#' @return Binary missingness pattern matrix for the given data.
#'
#' @references R. Richter, A. Miloschewski, J.F. Tavares, M.M.B. Breteler, S. Mukherjee (2012), Benchmarking Single
#' Imputation Methods. in preparation.
#'
#' @import matrixStats
#'
#' @export
#'


simulate_mask = function(data,
                         scenario = NULL,
                         seed = NULL){

  extra.output = FALSE # This is hard set since not needed anymore

  if(is.null(scenario)){
    exists.miss = matrixStats::colSums2(matrix(as.numeric(is.na(data)), ncol = ncol(data)))
    clms.with.miss = which(exists.miss != 0)
    how.much.miss = exists.miss[clms.with.miss]/nrow(data)
    scenario = list(MCAR = list(columns =  clms.with.miss,
                                probability = how.much.miss))
  }


  if("MCAR" %in% names(scenario) & !is.null(scenario$MCAR)){
    MCAR = TRUE
  } else{
    MCAR = FALSE
  }
  if("MAR" %in% names(scenario) & !is.null(scenario$MAR)){
    MAR = TRUE
  } else{
    MAR = FALSE
  }
  if("MNAR" %in% names(scenario)& !is.null(scenario$MNAR)){
    MNAR = TRUE
  } else{
    MNAR = FALSE
  }
  set.seed(seed)

  data.mtrx = data

  # We need to initialize some variables
  columns.mcar = scenario$MCAR$columns
  columns.mar = scenario$MAR$columns
  columns.mnar = scenario$MNAR$columns

  # We need an empty mask that we can fill. 1 will stand for NOT-missing
  one.mask = matrix(1, ncol = ncol(data.mtrx), nrow = nrow(data.mtrx))
  # In extra we collect extra data that can also be output later
  extra = list()
  # We need an empty MCAR mask
  binary.mask.mcar = one.mask
  if(MCAR){

    smpl = columns.mcar

    MCAR.extra = list()
    MCAR.extra$sample        = smpl
    MCAR.extra$missingness   = 1: length(smpl)

    mcar.counter             = 1

    for(tau in smpl){
      mcar.missing.fct = scenario$MCAR$probability[mcar.counter]

      miss.smpl = base::sample(c(0,1),
                               size = base::nrow(data.mtrx),
                               replace = TRUE,
                               prob = c( mcar.missing.fct, (1 - mcar.missing.fct) ))

      binary.mask.mcar[ , tau] = base::pmin( binary.mask.mcar[ , tau] , miss.smpl)
      MCAR.extra$missingness[mcar.counter] = mcar.missing.fct
      mcar.counter             = mcar.counter + 1
    }
    extra$MCAR = MCAR.extra
  } else{
    extra$MCAR$sample = list()
  }

  ###########################################################################
  # We need an empty MAR mask
  binary.mask.mar = one.mask

  if(MAR){

    smpl = columns.mar

    MAR.extra = list()
    MAR.extra$sample          = smpl
    MAR.extra$missingness     = 1: length(smpl)
    MAR.extra$influences      = list()
    MAR.extra$regr.coef      = list()
    MAR.extra$probabilities   = list()

    mar.counter               = 1

    for(nu in smpl){

      # mar.missing.fct = scenario$MAR$prob.1[mar.counter] # Not needed?

      inf.smpl = scenario$MAR$reg.columns[[mar.counter]]
      nbr.regressors = length(inf.smpl)

      MAR.extra$influences[[mar.counter]] = inf.smpl
      cent.norm.data = base::scale(data.mtrx[, inf.smpl])

      p_2 = scenario$MAR$probability.upper[mar.counter]
      p_mid = scenario$MAR$probability.midpoint[[mar.counter]][2]
      p_1 = scenario$MAR$probability.lower[mar.counter]

      # p_2 = 0.00001

      x_1 = min(matrixStats::rowSums2(cent.norm.data), na.rm = TRUE)
      x_2 = max(matrixStats::rowSums2(cent.norm.data), na.rm = TRUE)
      x_mid = scenario$MAR$probability.midpoint[[mar.counter]][1]*(x_2 - x_1) + x_1

      r_1 = (1/(x_1 - x_mid))*(log((1-p_mid) / p_mid) - log( (1 - p_1)/p_1))
      alpha_1 = log((1-p_1)/p_1) + (r_1*x_1)
      r_2 = (1/(x_mid - x_2))*(log((1-p_2) / p_2) - log( (1 - p_mid)/p_mid))
      alpha_2 = log((1-p_mid)/p_mid) + (r_2*x_mid)
      # 1/(1+exp((-r*x_2)+alpha))
      func = function(x){ ifelse(x < x_mid, (1/(1+exp((-r_1*x)+alpha_1))) , (1/(1+exp((-r_2*x)+alpha_2))))}
      # curve(func, from = x_1, to = x_2)
      rx = cent.norm.data %*% rep(1, times = length(inf.smpl))
      prob_mask = func(rx)
      prob_mask[base::is.na(prob_mask)] = 0

      MAR.extra$probabilities[[mar.counter]] = prob_mask

      binary.vec = 1:base::nrow(data.mtrx)
      for(k in 1:base::nrow(data.mtrx)){
        binary.vec[k] = base::sample(c(0,1),
                                     size = 1,
                                     prob =  c( prob_mask[k], (1 - prob_mask[k])) )
      }
      binary.mask.mar[, nu] = base::pmin(binary.mask.mar[, nu] ,  binary.vec)
      mar.counter           = mar.counter + 1
    }

    extra$MAR = MAR.extra

  } else{
    extra$MAR$sample = list()
  }


  ###########################################################################
  # We need an empty MNAR mask
  binary.mask.mnar = one.mask
  if(MNAR){

    MNAR.extra = list()

    smpl = columns.mnar

    MNAR.extra$sample         = smpl
    MNAR.extra$missingness    = 1: length(smpl)
    MNAR.extra$influences     = list()
    MNAR.extra$reg.coef       = list()
    MNAR.extra$probabilities  = list()

    mnar.counter              = 1

    for(xi in smpl){
      # xi = smpl[1]
      inf.smpl = c(scenario$MNAR$columns[mnar.counter])
      #nrow(data.mtrx)
      cent.norm.data = base::scale(data.mtrx[, inf.smpl])

      p_2 = scenario$MNAR$probability.upper[mnar.counter]
      p_mid = scenario$MNAR$probability.midpoint[[mnar.counter]][2]
      p_1 = scenario$MNAR$probability.lower[mnar.counter]

      x_1 = min(matrixStats::rowSums2(cent.norm.data), na.rm = TRUE)
      x_2 = max(matrixStats::rowSums2(cent.norm.data), na.rm = TRUE)
      x_mid = scenario$MNAR$probability.midpoint[[mnar.counter]][1]*(x_2 - x_1) + x_1

      r_1 = (1/(x_1 - x_mid))*(log((1-p_mid) / p_mid) - log( (1 - p_1)/p_1))
      alpha_1 = log((1-p_1)/p_1) + (r_1*x_1)
      r_2 = (1/(x_mid - x_2))*(log((1-p_2) / p_2) - log( (1 - p_mid)/p_mid))
      alpha_2 = log((1-p_mid)/p_mid) + (r_2*x_mid)
      # 1/(1+exp((-r*x_2)+alpha))
      func = function(x){ ifelse(x < x_mid, (1/(1+exp((-r_1*x)+alpha_1))) , (1/(1+exp((-r_2*x)+alpha_2))))}
      # curve(func, from = x_1, to = x_2)
      rx = cent.norm.data %*% rep(1, times = length(inf.smpl))
      prob_mask = func(rx)
      prob_mask[base::is.na(prob_mask)] = 0

      MNAR.extra$probabilities[[mnar.counter]] = prob_mask

      binary.vec = 1:base::nrow(data.mtrx)
      for(k in 1:base::nrow(data.mtrx)){
        binary.vec[k] = base::sample(c(0,1),
                                     size = 1,
                                     prob =  c( prob_mask[k], (1 - prob_mask[k])) )
      }

      binary.mask.mnar[, xi] = base::pmin(binary.mask.mnar[, xi] ,  binary.vec)
      mnar.counter           = mnar.counter + 1
    }
    extra$MNAR = MNAR.extra
  }

  binary.mask = base::pmin(binary.mask.mcar , binary.mask.mar , binary.mask.mnar)
  for(t in 1:base::ncol(binary.mask)){
    if(base::sum(binary.mask[,t]) < 2){
      binary.mask[1,t] = 1
      binary.mask[2,t] = 1
    }
  }
  masked.data = data.mtrx
  for(clmn in 1:base::ncol(data.mtrx)){
    masked.data[base::which(binary.mask[,clmn] == 0),clmn] = NA
  }
  if(extra.output){
    output = list(binary.mask = binary.mask,
                  masked.data = masked.data,
                  extra = extra)
  } else{
    output = binary.mask
  }
  return(output)
}


