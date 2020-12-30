
##' Prediction and simulation method for 'mobility.model' class
##'
##' This function uses a fitted \code{mobility.model} object to simulate a connectivity matrix based on estimated parameters.
##'
##' @param object a \code{mobility.model} object containing a fitted mobility model and its associated data
##' @param newdata a list containing new data to used in model prediction. If \code{NULL} (the default) the function will simulate the model with the data used for fitting.
##' \describe{
##'   \item{\code{D}}{a named matrix of distances between all \eqn{ij} location pairs}
##'   \item{\code{N}}{a named vector of population sizes for all locations (either N or both N_orig and N_dest must be supplied)}
##'   \item{\code{N_orig}}{a named vector of population sizes for each origin}
##'   \item{\code{N_dest}}{a named vector of population sizes for each destination}
##' }
##' @param nsim number of simulations (default = 1).
##' @param seed optional integer specifying the call to \code{set.seed} prior to model simulation (default = \code{NULL})
##' @param ... further arguments passed to or from other methods
##'
##' @details When \code{nsim = 1}, the prediction matrix is calculated
##' using the mean point estimate of parameter values. If \code{nsim > 1} then returns and array that contains
##' \code{nsim} number of simulated replications based on the posterior distributions of each parameter.
##'
##' @return a vector, matrix, or array containing predited or simulated mobility values.
##'
##' @author John Giles
##'
##' @example R/examples/predict.R
##'
##' @family model
##'
##' @export
##'

predict <- function(object, newdata, nsim, seed, ...) UseMethod('predict')

##' @export

predict.mobility.model <- function(object,
                                   newdata=NULL,
                                   nsim=1,
                                   seed=NULL,
                                   ...) {

  if (!exists(".Random.seed", envir=.GlobalEnv, inherits = FALSE)) {

    runif(1)

  } else if (is.null(seed)) {

    RNGstate <- get(".Random.seed", envir=.GlobalEnv)

  } else {

    global_seed <- get(".Random.seed", envir=.GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind=as.list(RNGkind()))
    on.exit(assign(".Random.seed", global_seed, envir=.GlobalEnv))

  }

  if (!is.null(newdata)) {

    if (!is.list(newdata)) stop("Argument 'newdata' must be a list object")

    if( all(c('D', 'N') %in% names(newdata)) ) {

      D <- newdata$D
      N_orig <- N_dest <- newdata$N

    } else if ( all(c('D', 'N_orig', 'N_dest') %in% names(newdata)) )  {

      for(i in seq_along(newdata)) assign(names(newdata)[i], newdata[[i]])

    } else {

      stop("Argument 'newdata' must be a list containg EITHER c(D, N) OR c(D, N_orig, N_dest)")

    }

  } else {

    if( all(c('D', 'N') %in% names(object$data)) ) {

      D <- object$data$D
      N_orig <- N_dest <- object$data$N

    } else if ( all(c('D', 'N_orig', 'N_dest') %in% names(object$data)) )  {

      for(i in seq_along(object$data)) assign(names(object$data)[i], object$data[[i]])

    } else {

      stop("Check slot 'data' of mobility.model object")

    }
  }

  msg <- 'D must be a named numeric matrix'
  if (!(is.matrix(D) & is.numeric(D))) stop(msg)
  if (any(unlist(lapply(dimnames(D), is.null)))) stop(msg)

  msg <- 'N_orig and N_dest must be named numeric vectors'
  if (!(is.vector(N_orig) & is.numeric(N_orig))) stop(msg)
  if (is.null(names(N_orig))) stop(msg)
  if (!(is.vector(N_dest) & is.numeric(N_dest))) stop(msg)
  if (is.null(names(N_dest))) stop(msg)

  check_dims <- c(identical(dim(D)[1], length(N_orig)),
                  identical(dim(D)[2], length(N_dest)))
  if (any(!check_dims)) stop('Dimensions of input data do not match')

  check_names <- c(identical(dimnames(D)[[1]], names(N_orig)),
                   identical(dimnames(D)[[2]], names(N_dest)))
  if (any(!check_names)) stop('Dimension names of input data do not match')

  vals <- c(D, N_orig, N_dest)
  if (any(is.na(vals)) | any(is.nan(vals))) stop('D and N are covariates and cannot contain missing values')

  if (all(is.null(object$summary), is.null(object$params))) stop('Model object must contain estimated model parameters or a model summary')

  if (object$model == 'radiation') {

    if (!is.null(newdata)) warning('Radiation models cannot be predicted using only covariates.')
    if (nsim > 1) warning('Radiation models cannot be used to simulate multiple realizations because they are parameter free.')
    out <- object$params

  } else {

    if (is.null(object$summary)) object$summary <- summary.mobility.model(object$params)

    params <- object$summary
    params <- params[!(row.names(params) %in% c('DIC', 'deviance', 'pD')),]

    params <- apply(params, 1, function(x){
      if (nsim == 1) {
        x['mean']
      } else {
        truncnorm::rtruncnorm(nsim, a=0, mean=x['mean'], sd=x['sd'])
      }
    })

    if (!is.matrix(params)) params <- t(as.matrix(params))
    environment(eqn_mobility) <- environment()

    out <- foreach::foreach(k=1:nrow(params), .combine=function(a, b) abind::abind(a, b, along=3)) %do% {

      eqn_mobility(object, D, N_orig, N_dest, params)

    }
  }

  attr(out, "model") <- object$model
  attr(out, "type") <- object$type
  if (!is.null(object$hierarchical)) attr(out, "hierarchical") <- object$hierarchical
  if (!is.null(seed)) attr(out, "seed") <- RNGstate
  return(out)
}


eqn_mobility <- function(object, D, N_orig, N_dest, params) {


  M_hat <- matrix(NA, length(N_orig), length(N_dest))

  if (object$model == 'gravity') {

    if (object$type == 'basic') {

      for (i in 1:length(N_orig)) {
        for (j in 1:length(N_dest)) {

          M_hat[i,j] <- params[k,'theta'] * N_orig[i] * N_dest[j] * (D[i,j]+0.001)

        }
      }

    } else if (object$type == 'transport') {

      for (i in 1:length(N_orig)) {
        for (j in 1:length(N_dest)) {

          M_hat[i,j] <- params[k,'theta'] * N_orig[i] * N_dest[j] * (D[i,j]+0.001)^(-params[k, 'gamma'])

        }
      }

    } else if (object$type == 'power') {

      for (i in 1:length(N_orig)) {
        for (j in 1:length(N_dest)) {

          M_hat[i,j] <- params[k,'theta'] * (N_orig[i]^params[k,'omega_1']) * (N_dest[j]^params[k,'omega_2']) * (D[i,j]+0.001)^(-params[k,'gamma'])

        }
      }

    } else if (object$type == 'exp') {

      for (i in 1:length(N_orig)) {
        for (j in 1:length(N_dest)) {

          M_hat[i,j] <- params[k,'theta'] * (N_orig[i]^params[k,'omega_1']) * (N_dest[j]^params[k,'omega_2']) * exp((-D[i,j]+0.001)/params[k,'delta'])

        }
      }

    } else if (object$type == 'power_norm') {

      x <- D; x[,] <- NA
      for (i in 1:length(N_orig)) {
        for (j in 1:length(N_dest)) {

          x[i,j] <- (N_dest[j]^params[k,'omega']) * (D[i,j]+0.001)^(-params[k,'gamma'])

        }
      }

      for (i in 1:length(N_orig)) {
        for (j in 1:length(N_dest)) {

          M_hat[i,j] <- params[k,'theta'] * N_orig[i] * (x[i,j]/sum(x[i,]))

        }
      }

    } else if (object$type == 'exp_norm') {

      x <- D; x[,] <- NA
      for (i in 1:length(N_orig)) {
        for (j in 1:length(N_dest)) {

          x[i,j] <- (N_dest[j]^params[k,'omega']) * exp((-D[i,j]+0.001)/params[k,'delta'])

        }
      }

      for (i in 1:length(N_orig)) {
        for (j in 1:length(N_dest)) {

          M_hat[i,j] <- params[k,'theta'] * N_orig[i] * (x[i,j]/sum(x[i,]))

        }
      }

    } else if (object$type == 'scaled_power') {

      for (i in 1:length(N_orig)) {
        for (j in 1:length(N_dest)) {

          M_hat[i,j] <- (N_dest[j]^params[k,'tau']) * ((1+(D[i,j]/params[k,'rho']))^-params[k,'alpha'])

        }
      }

    } else {

      stop('Unknown model type in object$type slot')

    }

  } else if (object$model == 'departure-diffusion') {

    if (object$type == 'power') {

      x <- D; x[,] <- NA
      for (i in 1:length(N_orig)) {
        for (j in 1:length(N_dest)) {

          x[i,j] <- ifelse(
            i == j,
            0,
            (N_dest[j]^params[k,'omega']) * (D[i,j]+0.001)^(-params[k,'gamma'])
          )

        }
      }

    } else if (object$type == 'exp') {

      x <- D; x[,] <- NA
      for (i in 1:length(N_orig)) {
        for (j in 1:length(N_dest)) {

          x[i,j] <- ifelse(
            i == j,
            0,
            (N_dest[j]^params[k,'omega']) * exp((-D[i,j]+0.001)/params[k,'delta'])
          )

        }
      }

    } else if (object$type == 'radiation') {

      if (!is.null(object$data$S)) {

        S <- object$data$S

      } else {

        S <- D; S[,] <- NA
        for (i in 1:nrow(D)) {
          for (j in 1:ncol(D)) {

            tot <- sum(N_dest[which(D[i,] <= D[i,j])])

            S[i,j] <- ifelse(
              i == j,
              tot - N_orig[i],
              tot - N_orig[i] - N_dest[j]
            )

          }
        }

      }

      x <- D; x[,] <- NA
      for (i in 1:length(N_orig)) {
        for (j in 1:length(N_dest)) {

          x[i,j] <- ifelse(
            i == j,
            0,
            N_dest[j] / ( (N_orig[i]+S[i,j]) * (N_orig[i]+N_dest[j]+S[i,j]) )
          )

        }
      }


    } else {

      stop('Unknown model type in object$type slot')

    }

    if (object$hierarchical == FALSE) {

      for (i in 1:length(N_orig)) {
        for (j in 1:length(N_dest)) {

          M_hat[i,j] <- ifelse(
            i == j,
            params[k,'theta'] * N_orig[i] * (1 - params[k,'tau']),
            params[k,'theta'] * N_orig[i] * params[k,'tau'] * (x[i,j]/sum(x[i,]))
          )

        }
      }

    } else if (object$hierarchical == TRUE) {

      for (i in 1:length(N_orig)) {
        for (j in 1:length(N_dest)) {

          M_hat[i,j] <- ifelse(
            i == j,
            params[k,'theta'] * N_orig[i] * (1 - params[k, paste0('tau_', i)]),
            params[k,'theta'] * N_orig[i] * params[k, paste0('tau_', i)] * (x[i,j]/sum(x[i,]))
          )

        }
      }

    } else {

      stop('Must indicate whether departure-diffusion model is hierarchical or not')

    }

  } else {

    stop('Unknown mobility model in object$model slot')

  }

  dimnames(M_hat) <- dimnames(D)
  return(M_hat)
}



##' @export

predict.prob_travel <- function(object,
                                newdata=NULL,
                                nsim=1,
                                seed=NULL,
                                ...

){

  if (!is.null(newdata)) stop('The probability of travel model cannot be predicted onto new data')

  if (!exists(".Random.seed", envir=.GlobalEnv, inherits = FALSE)) {

    runif(1)

  } else if (is.null(seed)) {

    RNGstate <- get(".Random.seed", envir=.GlobalEnv)

  } else {

    global_seed <- get(".Random.seed", envir=.GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind=as.list(RNGkind()))
    on.exit(assign(".Random.seed", global_seed, envir=.GlobalEnv))

  }

  if (nsim == 1) {

    out <- object$summary$mean
    names(out) <- names(object$data$travel)

  } else {

    mu <- object$summary$mean
    sigma <- object$summary$sd

    if (any(c(mu > 1, mu < 0, sigma > 1, sigma < 0))) stop('mu and sigma must be between 0 and 1')
    if (!(length(mu) == length(sigma))) stop('mu and sigma must have same length')

    bp <- do.call(rbind, get_beta_params(mu, sigma^2))
    colnames(bp) <- names(object$data$travel)

    sims <- apply(bp, 2, function(x) {
      suppressWarnings(rbeta(nsim, x['shape1'], x['shape2']))
    })

    out <- t(sims)
  }

  attr(out, "model") <- object$model
  attr(out, "type") <- object$type
  if (!is.null(object$hierarchical)) attr(out, "hierarchical") <- object$hierarchical
  if (!is.null(seed)) attr(out, "seed") <- RNGstate
  return(out)
}

