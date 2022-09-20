
#' Data Simulation Protocol of ImputeBench
#'
#' @description Simulating data according to the data simulation protocol of RMTBM22.
#'
#' @param data_parameters A named list of data parameters, for details see below and the accompanying vignette. Default is given
#' further below.
#' @param seed An integer, sets a seed for the simulation. Default is `NULL`.
#'
#' @details The parameter `data_parameters` needs to be a list specifying the data parameters of the data simulation protocol
#' defined in RMTBM22. Not all parameters have to be specified, missing ones are replaced by default ones. For a list and discussion
#' of the parameters we refer to the accompanying vignette and RMTBM22. Here we just give an example that also constitutes the
#' default of `data_parameters`:
#'
#' ```
#' data_parameters = list(data.size = 100,
#'                        data.type = c(5,5,5,5,5,5),
#'                        data.shape = list(gauss.mean = c(0,1),
#'                                          gauss.sd = c(1,2),
#'                                          pois.lambda = c(1),
#'                                          binary.limits = c(0.2,0.8),
#'                                          t.mean = c(0,1),
#'                                          t.limits = c(1,20),
#'                                          nl.mean = c(0,1),
#'                                          nl.limits = c(10,30),
#'                                          nl.freq = c(10,20,15,1),
#'                                          sp.points = 5,
#'                                          incl.group = 0),
#'                        data.groups = 1,
#'                        data.groups.distance = c(0.5,1.5),
#'                        data.non.linearity = 0,
#'                        data.spars = 0.4,
#'                        data.low.dim = 5,
#'                        data.low.dim.groups = 1,
#'                        data.low.dim.parameters = list(trunc.parameter = c(0.5,10,2,0.5),
#'                                                       mean = 1,
#'                                                       error.std = 0.05))
#' ```
#'
#' @references R. Richter, A. Miloschewski, J.F. Tavares, M.M.B. Breteler, S. Mukherjee (2012), Benchmarking Single
#' Imputation Methods. in preparation.
#'
#' @seealso `data_ImputeBench`
#'
#' @import stats
#' @import genscore
#' @import mvtnorm
#' @import truncnorm
#' @import matrixStats
#' @import extraDistr
#'
#' @return A numeric matrix.
#'
#' @export
#'



simulate_data = function(data_parameters = list(data.size = 100,
                                                data.type = c(5,5,5,5,5,5),
                                                data.shape = list(gauss.mean = c(0,1),
                                                                  gauss.sd = c(1,2),
                                                                  pois.lambda = c(1),
                                                                  binary.limits = c(0.2,0.8),
                                                                  t.mean = c(0,1),
                                                                  t.limits = c(1,20),
                                                                  nl.mean = c(0,1),
                                                                  nl.limits = c(10,30),
                                                                  nl.freq = c(10,20,15,1),
                                                                  sp.points = 5,
                                                                  incl.group = 0),
                                                data.groups = 1,
                                                data.groups.distance = c(0.5,1.5),
                                                data.non.linearity = 0,
                                                data.spars = 0.4,
                                                data.low.dim = 5,
                                                data.low.dim.groups = 1,
                                                data.low.dim.parameters = list(trunc.parameter = c(0.5,10,2,0.5),
                                                                               mean = 1,
                                                                               error.std = 0.05)),
                         seed = NULL){

  # Check for not used entries
  if(!is.null(data_parameters)){
    chk = length(setdiff(names(data_parameters),c("data.size", "data.type", "data.shape", "data.groups", "data.non.linearity", "data.spars",
                                                  "data.groups.distance", "data.low.dim", "data.low.dim.groups", "data.low.dim.parameters")))
    if(chk != 0){
      stop(paste0("data_parameters seems to have an unknown entry."))
    }
    if(!is.null(data_parameters$data.shape)){
      chk = length(setdiff(names(data_parameters$data.shape),c("gauss.mean","gauss.sd","pois.lambda","binary.limits","t.mean","t.limits", "nl.mean",
                                                               "nl.limits","nl.freq","sp.points","incl.group")))
      if(chk != 0){
        stop(paste0("data_parameters$data.shape seems to have an unknown entry."))
      }
    }
    if(!is.null(data_parameters$data.low.dim.parameters)){
      chk = length(setdiff(names(data_parameters$data.low.dim.parameters),c("trunc.parameter","mean","error.std")))
      if(chk != 0){
        stop(paste0("data_parameters$data.low.dim.parameters seems to have an unknown entry."))
      }
    }
  }

  # Check for problems
  data.p = data_parameters
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

  ## Add default parameters

  if(!is.null(data_parameters$data.shape)){
    data_parameters$data.shape = list(gauss.mean = if(is.null(data_parameters$data.shape$gauss.mean)){c(0,1)
    } else{data_parameters$data.shape$gauss.mean},
    gauss.sd = if(is.null(data_parameters$data.shape$gauss.sd)){c(1,2)
    } else{data_parameters$data.shape$gauss.sd},
    pois.lambda = if(is.null(data_parameters$data.shape$pois.lambda)){1
    } else{data_parameters$data.shape$pois.lambda},
    binary.limits = if(is.null(data_parameters$data.shape$binary.limits)){c(0.2,0.8)
    } else{data_parameters$data.shape$binary.limits},
    t.mean = if(is.null(data_parameters$data.shape$t.mean)){c(0,1)
    } else{data_parameters$data.shape$t.mean},
    t.limits = if(is.null(data_parameters$data.shape$t.limits)){c(1,20)
    } else{data_parameters$data.shape$t.limits},
    nl.mean = if(is.null(data_parameters$data.shape$nl.mean)){c(0,1)
    } else{data_parameters$data.shape$nl.mean},
    nl.limits = if(is.null(data_parameters$data.shape$nl.limits)){c(10,30)
    } else{data_parameters$data.shape$nl.limits},
    nl.freq = if(is.null(data_parameters$data.shape$nl.freq)){c(10,20,15,1)
    } else{data_parameters$data.shape$nl.freq},
    sp.points = if(is.null(data_parameters$data.shape$sp.points)){5
    } else{data_parameters$data.shape$sp.points},
    incl.group = if(is.null(data_parameters$data.shape$incl.group)){0
    } else{data_parameters$data.shape$incl.group})
  }

  if(!is.null(data_parameters$data.low.dim.parameters)){
    data_parameters$data.low.dim.parameters = list(trunc.parameter = if(is.null(data_parameters$data.low.dim.parameters$trunc.parameter)){ c(0.5,10,2,0.5)
    } else{data_parameters$data.low.dim.parameters$trunc.parameter},
    mean = if(is.null(data_parameters$data.low.dim.parameters$mean)){ 1
    } else{data_parameters$data.low.dim.parameters$mean},
    error.std = if(is.null(data_parameters$data.low.dim.parameters$error.std)){ 0.05
    } else{data_parameters$data.low.dim.parameters$error.std})
  }

  data_parameters = list(data.size = if(is.null(data_parameters$data.size)){100} else{data_parameters$data.size},
                         data.type = if(is.null(data_parameters$data.type)){c(5,5,5,5,5,5)} else{data_parameters$data.type},
                         data.shape = if(is.null(data_parameters$data.shape)){list(gauss.mean = c(0,1) ,
                                                                                   gauss.sd = c(1,2) ,
                                                                                   pois.lambda = c(1) ,
                                                                                   binary.limits = c(0.2,0.8) ,
                                                                                   t.mean = c(0,1),
                                                                                   t.limits = c(1,20) ,
                                                                                   nl.mean = c(0,1) ,
                                                                                   nl.limits = c(10,30),
                                                                                   nl.freq = c(10,20,15,1),
                                                                                   sp.points = 5,
                                                                                   incl.group = 0)} else{data_parameters$data.shape},
                         data.non.linearity = if(is.null(data_parameters$data.non.linearity)){0} else{data_parameters$data.non.linearity},
                         data.spars = if(is.null(data_parameters$data.spars)){0.4} else{data_parameters$data.spars},
                         data.low.dim = if(is.null(data_parameters$data.low.dim)){5} else{data_parameters$data.low.dim},
                         data.groups = if(is.null(data_parameters$data.groups)){1} else{data_parameters$data.groups},
                         data.groups.distance = if(is.null(data_parameters$data.groups.distance)){c(0.5,1.5)} else{data_parameters$data.groups.distance},
                         data.low.dim.groups = if(is.null(data_parameters$data.low.dim.groups)){1} else{data_parameters$data.low.dim.groups},
                         data.low.dim.parameters = if(is.null(data_parameters$data.low.dim.parameters)){list(trunc.parameter = c(0.5,10,2,0.5),
                                                                                                             mean = 1,
                                                                                                             error.std = 0.05)} else{data_parameters$data.low.dim.parameters})

  N =  data_parameters$data.size
  p = data_parameters$data.type
  low.dim.groups = data_parameters$data.low.dim.groups
  low.dim = data_parameters$data.low.dim
  non.linearity = data_parameters$data.non.linearity
  low.dim.parameters_trunc = data_parameters$data.low.dim.parameters$trunc.parameter
  low.dim.parameters_error = data_parameters$data.low.dim.parameters$error.std
  param_gauss_mean = data_parameters$data.shape$gauss.mean
  param_gauss_sd = data_parameters$data.shape$gauss.sd
  param_pois_lambda = data_parameters$data.shape$pois.lambda
  param_binary_limits = data_parameters$data.shape$binary.limits
  param_t_mean = data_parameters$data.shape$t.mean
  param_t_limits = data_parameters$data.shape$t.limits
  param_nl_mean = data_parameters$data.shape$nl.mean
  param_nl_limits = data_parameters$data.shape$nl.limits
  param_nl_frequency = data_parameters$data.shape$nl.freq
  param_sp_points = data_parameters$data.shape$sp.points
  param_modes = data_parameters$data.groups
  param_modes_include = data_parameters$data.shape$incl.group
  sparsity = data_parameters$data.spars
  p.mean  = data_parameters$data.low.dim.parameters$mean
  group_min_max = data_parameters$data.groups.distance

  truncparam = 5000
  epsilon = 10^-10

  base::set.seed(seed)
  P = base::sum(p)
  if(param_modes != 1){
    P = P + 1
  }
  if(is.null(low.dim)){
    q = P
  } else{
    q = low.dim
  }
  if(low.dim.groups < 1){
    stop("The Number of low.dim.trees must be greater or equal 1")
  }
  if(q > P){
    stop("Low dimensionality parameter must be smaller than P")
  }
  if(base::min(q) < 0){
    stop("Low dimensionality parameter must be greter or equal than 0")
  }
  # We need to first iterate over the independent trees
  group_lw = floor(N/low.dim.groups)
  for(t in 1:low.dim.groups){
    # We need to draw latent Gaussians with given precision matrix sparsity
    mu = stats::rnorm(n = q, mean = 0, sd = 1)
    # Draw a precision matrix
    if(q == 1){
      Precision.matrix = 1
    } else{
      Precision.matrix = genscore::cov_cons(mode  = "er",
                                            p     = q,
                                            spars = sparsity,
                                            eig   = 0.1)  ### What is eig?!
    }
    # Now we can and should draw the latent Gaussians
    if(q == 1){
      Latent.Gaussians = base::as.matrix(stats::rnorm(n = group_lw, mean = stats::rnorm(n = 1, mean = 0, sd = 1), sd = Precision.matrix),
                                         nrow = group_lw,
                                         ncol = 1)
    } else{
      Latent.Gaussians = mvtnorm::rmvnorm(group_lw,
                                          mean = mu,
                                          sigma = base::solve(Precision.matrix) ) # Is base here right?
    }
    # Now we need to draw a projection matrix and send our data through it (Note, it also goes through it if q = p)
    proj = base::sample(c(-1,1),
                        size = P * q,
                        replace = TRUE) *
      truncnorm::rtruncnorm(n    = P * q,
                            a    = low.dim.parameters_trunc[1],
                            b    = low.dim.parameters_trunc[2],
                            mean = low.dim.parameters_trunc[3],
                            sd   = low.dim.parameters_trunc[4])
    Proj = base::matrix(proj, ncol = P, nrow = q)
    error.mtx = base::matrix(stats::rnorm(n = P * group_lw, mean = 0, sd = low.dim.parameters_error),
                             nrow = group_lw, ncol = P)
    proj.mean = base::matrix(rep(stats::rnorm(n = P, mean = 0, sd = p.mean), each = group_lw), nrow = group_lw)
    Raw.Gaussians = (Latent.Gaussians %*% Proj) + error.mtx + proj.mean
    # We want to add some non-linearity via the non.linearity parameter
    if(non.linearity != 0){
      core = matrixStats::colMedians(Raw.Gaussians)
      dists = base::apply(X = Raw.Gaussians, 1, base::norm, type = "2")
      max.allongation = base::max(dists)
      radius = non.linearity * max.allongation
      for(nu in 1:base::nrow(Raw.Gaussians)){
        difference = (Raw.Gaussians[nu,] - core)
        distance = base::norm(difference, type = "2")
        if(distance < radius){
          if(distance == 0){
            Raw.Gaussians[nu,] = core + radius*(core/(base::norm(core, type = "2") + epsilon))
          } else{
            Raw.Gaussians[nu,] = core + (radius/distance)*difference
          }
        }
      }
    }
    # We need to bind the different independent latent data draws together
    if(t == 1){
      Full.Raw.Gaussians = Raw.Gaussians
    } else{
      Full.Raw.Gaussians = rbind(Full.Raw.Gaussians, Raw.Gaussians)
    }
  }
  shuffle.rows = base::sample(1:(group_lw*low.dim.groups), size = (group_lw*low.dim.groups), replace = FALSE)
  Full.Raw.Gaussians = Full.Raw.Gaussians[shuffle.rows,]
  # Let us scale the raw data as to easily dictate the parameters of the distributions
  Full.Raw.Gaussians = base::scale(Full.Raw.Gaussians)
  # We want to shuffle as to randomly assing a data type, i.e. Gauss, Poisson, etc., to each column
  shuffle = base::sample(1:P, size = P, replace = FALSE)
  Full.Raw.Gaussians = Full.Raw.Gaussians[,shuffle]
  # We need to Create the different kind of data columns.
  if(p[1] != 0){
    gauss = 1:p[1]
    Final.Gaussians = Full.Raw.Gaussians[,gauss]
    gauss.sigma = truncnorm::rtruncnorm( p[1],
                                         a    = 0.5,
                                         b    = Inf,
                                         mean = param_gauss_sd[1],
                                         sd   = param_gauss_sd[2])
    gauss.mu = stats::rnorm( p[1],
                             mean = param_gauss_mean[1],
                             sd = param_gauss_mean[2])
    Final.Gaussians = base::sqrt(base::matrix(base::rep(gauss.sigma, each = N), nrow = N, ncol = p[1])) *
      (Final.Gaussians + base::matrix(base::rep(gauss.mu, each = N), nrow = N, ncol = p[1]))
  } else{
    Final.Gaussians = NULL
  }
  if(p[2] != 0){
    pois = (1 + p[1]) : (p[1] + p[2])
    Final.Poisson = base::as.matrix(Full.Raw.Gaussians[,pois])
    lambda = stats::rpois(n = p[2], lambda = param_pois_lambda) +
      base::rep(1, times = p[2])
    for(i in 1:p[2]){
      Final.Poisson[,i] = stats::qpois( stats::pnorm( base::scale(Final.Poisson[,i]),
                                                      mean = 0,
                                                      sd = 1),
                                        lambda = lambda[i])
    }
  } else{
    Final.Poisson = NULL
  }
  if(p[3] != 0){
    bin = (1 + p[1] + p[2]) : (p[1] + p[2] + p[3])
    Final.Binary = as.matrix(Full.Raw.Gaussians[,bin])
    binary.p = stats::runif(n = p[3],
                            min = param_binary_limits[1],
                            max = param_binary_limits[2])
    for(i in 1:p[3]){
      temporary = Final.Binary[,i]

      thres = stats::qnorm(1 - binary.p[i])

      temporary[ temporary >  thres] = 1
      temporary[ temporary <= thres] = 0
      Final.Binary[,i] = temporary
    }
  } else{
    Final.Binary = NULL
  }
  if(p[4] != 0){
    t.dist = (1 + p[1] + p[2] + p[3]) : (p[1] + p[2] + p[3] + p[4])
    Final.t = base::as.matrix(Full.Raw.Gaussians[,t.dist])
    means_t = stats::rnorm(n = p[4],
                           mean = param_t_mean[1],
                           sd = param_t_mean[2])
    degrees.freedom.t = base::sample( param_t_limits[1]:param_t_limits[2],
                                      size = p[4],
                                      replace = TRUE)
    for(i in 1:p[4]){
      Final.t[,i] = stats::qt( stats::pnorm( base::scale( Final.t[,i]),
                                             mean = 0,
                                             sd = 1),
                               df = degrees.freedom.t[i])
      + base::rep(means_t[i], times = N)
    }
  } else{
    Final.t = NULL
  }
  if(p[5] != 0){
    non.lin = (1 + p[1] + p[2] + p[3] + p[4]) : (p[1] + p[2] + p[3] + p[4] + p[5])
    Final.non.lin = base::as.matrix(Full.Raw.Gaussians[,non.lin])
    non.lin_means = stats::rnorm(n = p[5],
                                 mean = param_nl_mean[1],
                                 sd = param_nl_mean[2] )
    non.lin_amplitude = stats::runif( n = p[5],
                                      min = param_nl_limits[1],
                                      max = param_nl_limits[2])
    non.lin_frequency = truncnorm::rtruncnorm( n = p[5],
                                               a = param_nl_frequency[1],
                                               b = param_nl_frequency[2],
                                               mean = param_nl_frequency[3],
                                               sd = param_nl_frequency[4])
    for(i in 1:p[5]){
      Final.non.lin[,i] = base::rep(non.lin_means[i], times = N) +
        (sin(non.lin_frequency[i] * Final.non.lin[,i]) * non.lin_amplitude[i])
    }
  } else{
    Final.non.lin = NULL
  }
  if(p[6] != 0){
    spl = (1 + p[1] + p[2] + p[3] + p[4] + p[5]) : (p[1] + p[2] + p[3] + p[4] + p[5] + p[6])
    Final.spline = base::as.matrix(Full.Raw.Gaussians[,spl])
    for(i in 1:p[6]){
      top = base::max(Final.spline[,i])
      bottom = base::min(Final.spline[,i])
      ### Points on the x-Axis
      intervals = base::seq(from = bottom, to = top, by = (top-bottom)/(param_sp_points + 1))
      ### Y-Axis Deflections from the identity:
      deflections = ((top-bottom)/(param_sp_points + 1)) * stats::runif(n = (param_sp_points + 2), min = 0.5, max = 1 )
      ### Collect the support points:
      spl.points = list()
      spl.points[[1]] = c(intervals[1], intervals[1] - deflections[1])
      for(t in 1:(param_sp_points+2)){
        spl.points[[t+1]] = c(intervals[t+1], intervals[t+1] + ((-1)^(t+1)*deflections[t+1]))
      }
      spl.points[[param_sp_points+2]] = c(intervals[(param_sp_points + 2)], intervals[param_sp_points + 2] + deflections[param_sp_points +2])
      ### Collect the slope and y-Axis intersection for the linear functions between all support points:
      linearfcts = list()
      for(t in 1:(param_sp_points+1)){
        grad = (spl.points[[t+1]][2]-spl.points[[t]][2])/(spl.points[[t+1]][1]-spl.points[[t]][1])
        linearfcts[[t]] = c( grad, spl.points[[t+1]][2] - grad*(spl.points[[t+1]][1]) )
      }
      for(n0 in 1:nrow(Final.spline)){
        old = Final.spline[n0,i]
        for(t in 1:(param_sp_points + 1)){
          if((intervals[t] <= old) & (old <= intervals[t+1])){
            Final.spline[n0,i] = linearfcts[[t]][1]*old + linearfcts[[t]][2]
          }
        }
      }
    }
  } else{
    Final.spline = NULL
  }
  # Collect all columns in one matrix
  Final.Data = cbind(Final.Gaussians,
                     Final.Poisson,
                     Final.Binary,
                     Final.t,
                     Final.non.lin,
                     Final.spline)
  # Check the Consistency of the data (If we find NA's replace them by a uniform draw from -truncparam to +truncparam)
  Replace = stats::runif(base::length(base::which(!base::is.finite(Final.Data))), -1, 1)*truncparam
  Final.Data[!base::is.finite(Final.Data)] = Replace

  # Preparing the mode shift
  if(param_modes != 1){
    mode_indicator = extraDistr::qdunif( p = stats::pnorm( base::scale(
      base::as.matrix(Full.Raw.Gaussians[,P])),
      mean = 0,
      sd = 1),
      min = 1,
      max = param_modes)
    column.iqrs = matrixStats::colIQRs(Final.Data)
    for(cc in   c((1:p[1]),((p[1]+p[2]+p[3]+p[4]):(P-1)))){
      col.iqr = column.iqrs[cc]
      ran.fct = sample(x = c(-1,1),size = param_modes, replace = TRUE) * runif(n = param_modes, min = group_min_max[1], max = group_min_max[2])
      for(mode in 1:param_modes){
        if(length(which(mode_indicator == mode)) != 0){
          Final.Data[which(mode_indicator == mode),cc] =
            Final.Data[which(mode_indicator == mode),cc] + ran.fct[mode] * col.iqr
        }
      }
    }
  }
  if(param_modes_include == 1){
    Final.Data = cbind(Final.Data, mode_indicator)
  }

  # Now we need to add modes



  return(Final.Data)
}



##########################################################
###############   create count variables #################
# based on approach suggested in 'Nonparametric Graphical Model for Counts', Roy and Dunson, 2019,
# Journal of Machine Learning Research
# 1.X (pois*N)xp  MVN(0p,Omega^(-1)pxp) with Omega being sparse precision matrix
# 2.calculate Pij= c(xij), where c is the cumulative density function of the standard normal
# 3.Yij=QP(Pij,lambda) for a given mean parameter lambda with QP the quantile function of Poisson(lambda) is then Poisson distributed




