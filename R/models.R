##' Fit Bayesian model using MCMC
##'
##' This is a general wrapper function for fitting a model to data using the JAGS MCMC algorithm.
##' The function uses code adapted from the \code{\link[rjags]{rjags}} package. Options include
##' calculating the Deviance Information Criterion (DIC) and running sampling chains in parallel.
##'
##' @param jags_data a list of data objects
##' @param jags_model character string giving the model specification in JAGS/BUGS syntax
##' @param params vector of parameters to monitor
##' @param n_chain number of MCMC sampling chains
##' @param n_burn number of iterations to discard before sampling of chains begins (burn in)
##' @param n_samp number of iterations to sample each chain
##' @param n_thin interval to thin samples
##' @param DIC logical indicating whether or not to calculate the Deviance Information Criterion (DIC)
##' @param parallel logical indicating whether or not to run MCMC chains in parallel or sequentially (default = FALSE). An even number of chains is recommended when running in parallel.
##'
##' @details If JAGS library not already installed, install here: \url{https://sourceforge.net/projects/mcmc-jags/files/JAGS/3.x/}
##'
##' @return an \code{\link[coda:mcmc.list]{mcmc.list}} object
##'
##' @author John Giles
##'
##' @example R/examples/fit_jags.R
##'
##' @family model
##'
##' @export
##'

fit_jags <- function(
  jags_data,
  jags_model,
  params,
  n_chain=2,
  n_burn=1000,
  n_samp=1000,
  n_thin=1,
  DIC=FALSE,
  parallel=FALSE
) {

  if (parallel == FALSE) {

    suppressMessages(rjags::load.module('lecuyer'))

    init_list <- replicate(n_chain,
                           list(.RNG.name='lecuyer::RngStream',
                                .RNG.seed= sample(1:1e6, 1)),
                           simplify=FALSE)

    if (DIC == FALSE) {

      mod <- rjags::jags.model(file=textConnection(jags_model),
                               data=jags_data,
                               inits=init_list,
                               n.chains=n_chain,
                               n.adapt=0)

      Matrix::update(mod, n_burn)

      mod <- rjags::jags.samples(mod,
                                 variable.names=params,
                                 n.iter=n_samp,
                                 thin=n_thin)

      mod <- rjags_to_mcmc(mod)

    } else if (DIC == TRUE) {

      if (n_chain < 2) stop('Calculation of DIC requires at least 2 sampling chains')

      suppressMessages(rjags::load.module('dic'))

      mod <- rjags::jags.model(file=textConnection(jags_model),
                               data=jags_data,
                               inits=init_list,
                               n.chains=n_chain,
                               n.adapt=0)

      Matrix::update(mod, n_burn)

      mod <- rjags::jags.samples(mod,
                                 variable.names=c(params, 'deviance', 'pD'),
                                 n.iter=n_samp,
                                 thin=n_thin)

      # clean up penalty output
      mod$pD <- array(mean(mod$pD), dim=dim(mod$deviance))
      attributes(mod$pD) <- attributes(mod$deviance)
      attributes(mod$pD)$varname <- 'pD'

      # convert mcmc and add DIC
      mod <- coda::as.mcmc.list(lapply(rjags_to_mcmc(mod), function(x){
        coda::as.mcmc(cbind(x, DIC=apply(x, 1, function(y) y['deviance'] + 2*y['pD'])))
      }))
    }

    return(mod)

  } else if (parallel == TRUE) {

    if(n_chain > parallel::detectCores()) warning('Number of sampling chains greater than available cores')

    if (DIC == FALSE) {

      cl <- parallel::makeCluster(n_chain,
                                  setup_strategy='sequential',
                                  setup_timeout=0.5)

      doParallel::registerDoParallel(cl)

      if (foreach::getDoParRegistered()) message(paste("Initiated cluster", foreach::getDoParName(), "with", foreach::getDoParWorkers(), "workers", sep=' '))

      out <- foreach::foreach(i=1:n_chain, .combine='combine_rjags', .packages=c('rjags', 'abind')) %dopar% {

        rjags::load.module('lecuyer')

        mod <- rjags::jags.model(file=textConnection(jags_model),
                                 data=jags_data,
                                 inits=list(.RNG.name='lecuyer::RngStream',
                                            .RNG.seed=sample(1:1e6, 1)),
                                 n.chains=1,
                                 n.adapt=0)

        Matrix::update(mod, n_burn)

        rjags::jags.samples(mod,
                            variable.names=params,
                            n.iter=n_samp,
                            thin=n_thin)
      }

      out <- rjags_to_mcmc(out)

    } else if (DIC == TRUE) {

      if (n_chain < 2) stop('Calculation of DIC requires at least 2 sampling chains')

      cl <- parallel::makeCluster(n_chain/2,
                                  setup_strategy='sequential',
                                  setup_timeout=0.5)

      doParallel::registerDoParallel(cl)

      if (foreach::getDoParRegistered()) message(paste("Initiated cluster", foreach::getDoParName(), "with", foreach::getDoParWorkers(), "workers", sep=' '))

      out <- foreach::foreach(i=1:(n_chain/2), .combine='combine_rjags', .packages=c('rjags', 'abind')) %dopar% {

        rjags::load.module('lecuyer')
        rjags::load.module('dic')

        init_list <- replicate(2,
                               list(.RNG.name='lecuyer::RngStream',
                                    .RNG.seed= sample(1:1e6, 1)),
                               simplify=FALSE)

        mod <- rjags::jags.model(file=textConnection(jags_model),
                                 data=jags_data,
                                 inits=init_list,
                                 n.chains=2,
                                 n.adapt=0)

        Matrix::update(mod, n_burn)

        mod <- rjags::jags.samples(mod,
                                   variable.names=c(params, 'deviance', 'pD'),
                                   n.iter=n_samp,
                                   thin=n_thin)

        # clean up penalty output
        mod$pD <- array(mean(mod$pD), dim=dim(mod$deviance))
        attributes(mod$pD) <- attributes(mod$deviance)
        attributes(mod$pD)$varname <- 'pD'

        mod
      }

      # convert mcmc and add DIC
      out <- coda::as.mcmc.list(lapply(rjags_to_mcmc(out), function(x){
        coda::as.mcmc(cbind(x, DIC=apply(x, 1, function(y) y['deviance'] + 2*y['pD'])))
      }))
    }

    parallel::stopCluster(cl)
    return(out)
  }
}




# Convert rjags object to mcmc.list
#
# This function converts an rjags model object to an \code{\link[coda:mcmc.list]{mcmc.list}} object.
#
# param x an rjags list
#
# return an \code{\link[coda:mcmc.list]{mcmc.list}} object
#
# author John Giles
#
# export
#

rjags_to_mcmc <- function(x) {

  n_chain <- max(unlist(lapply(x, function(x) dim(x)[length(dim(x))])))

  out <- foreach::foreach(i=1:n_chain) %do% {

    coda::as.mcmc(
      do.call(
        cbind,
        lapply(
          x,
          function(x) {

            n_samp <- dim(x)[2]
            samps <- x[,,i]

            if (!is.null(dim(samps))) {

              param_names <- stringr::str_c(attributes(x)$varname, 1:nrow(samps), sep='_')
              return(matrix(t(samps), nrow=n_samp, dimnames=list(NULL, param_names)))

            } else {

              return(matrix(samps, nrow=n_samp, byrow=TRUE, dimnames=list(NULL, attributes(x)$varname)))
            }
          }
        )
      )
    )
  }

  coda::as.mcmc.list(out)
}


# Combine two rjags objects
#
# This function combines two rjags model objects.
#
# param a an rjags list
# param b an rjags list
#
# return an rjags list containing model ouput from \code{a} and \code{b}
#
# author John Giles
#
# export
#

combine_rjags <- function(a, b) {

  out <- a

  for (i in seq_along(out)) {

    out[[i]] <- abind::abind(a[[i]], b[[i]], along=length(dim(a[[i]])))
    atts <- attributes(a[[i]])
    atts$dim <- dim(out[[i]])
    attributes(out[[i]]) <- atts
  }

  out
}




##' Estimate probability of travelling outside origin
##'
##' This function fits a hierarchical beta-binomial model that estimates the probability an individual travels outside their home location
##' within the time period of the survey (tau). The model estimates both the overall population-level probability of travel (tau_pop) and
##' the origin-level probability of travel (tau_i). Further this method is designed for sparse observations that typically result from
##' data with high temporal resolution or travel survey data. When data are missing, unobserved routes of travel regress to the population mean.
##'
##' @param travel named vector of number of people that reported travelling outside their home location
##' @param total named vector of the total number of individuals in travel survey for each location
##' @param n_chain number of MCMC sampling chains
##' @param n_burn number of iterations to discard before sampling of chains begins (burn in)
##' @param n_samp number of iterations to sample each chain
##' @param n_thin interval to thin samples
##' @param DIC logical indicating whether or not to calculate the Deviance Information Criterion (DIC)
##' @param parallel logical indicating whether or not to run MCMC chains in parallel or sequentially (default = \code{FALSE})
##'
##' @return An object of class \code{mobility.model} containing model information, data, and fitted model parameters
##'
##' @author John Giles
##'
##' @example R/examples/fit_prob_travel.R
##'
##' @family model
##' @family travel probability
##'
##' @export
##'

fit_prob_travel <- function(
  travel,
  total,
  n_chain=2,
  n_burn=1000,
  n_samp=1000,
  n_thin=1,
  DIC=FALSE,
  parallel=FALSE
) {

  # Check data
  if (!(identical(length(travel), length(total)))) stop('Dimensions of input data must match')
  if (!(identical(names(travel), names(total)))) stop('Dimension names of input data do not match.')
  if (is.null(names(travel))) stop('Vectors are not named.')

  # Work around for NAs
  na.fix <- any(is.na(travel)) | any(is.na(total))
  if (na.fix) {

    complete_obs <- complete.cases(cbind(travel, total))
    missing_obs <- !complete_obs

    message('These missing locations will inherit population mean:')
    message(paste(names(travel)[missing_obs], collapse=' '))

  } else {
    complete_obs <- seq_along(travel)
  }

  jags_data <- list(
    travel=travel[complete_obs],
    total=total[complete_obs]
  )

  jags_model <- "
  model {

    # Origin-level probability of travel
    for (i in 1:length(travel)) {

      travel[i] ~ dbin(tau[i], total[i])
    }

    # Population-level priors
    tau_pop ~ dbeta(1+alpha, 1+beta)
    alpha ~ dgamma(0.01, 0.01)
    beta ~ dgamma(0.1, 0.01)

    # Origin-level priors
    for (i in 1:length(travel)) {
      tau[i] ~ dbeta( 1+alpha, 1+beta)
    }
  }"

  out <- fit_jags(jags_data=jags_data,
                  jags_model=jags_model,
                  params=c('tau_pop', 'tau'),
                  n_chain=n_chain,
                  n_burn=n_burn,
                  n_samp=n_samp,
                  n_thin=n_thin,
                  DIC=DIC,
                  parallel=parallel)

  sel <- which(coda::varnames(out) == 'tau_pop')
  pop_mean <- out[,sel]
  out <- out[,-sel]

  if (na.fix) {

    if (DIC) {
      sel <- which(coda::varnames(out) %in% c('deviance', 'pD', 'DIC'))
      DIC_samps <- out[,sel]
      out <- out[,-sel]
    }

    coda::varnames(out) <- stringr::str_c('tau', which(complete_obs), sep='_')
    missing_names <- stringr::str_c('tau', which(missing_obs), sep='_')

    for (i in 1:n_chain) {

      tmp <- pop_mean[[i]]

      out[[i]] <- cbind(out[[i]],
                        matrix(rep(tmp, times=sum(missing_obs)),
                               nrow=length(tmp),
                               dimnames=list(NULL, missing_names)))

      index <- as.numeric(stringr::str_split(colnames(out[[i]]),
                                             pattern = '_',
                                             simplify=TRUE)[,2])

      out[[i]] <- coda::as.mcmc(out[[i]][,order(index)])
      if (DIC) out[[i]] <- coda::as.mcmc(cbind(out[[i]], DIC_samps[[i]]))
    }

  }

  out <- structure(
    list(model='prob_travel',
         type=NULL,
         n_chain=n_chain,
         n_burn=n_burn,
         n_samp=n_samp,
         n_thin=n_thin,
         DIC=DIC,
         data=list(travel=travel,
                   total=total),
         params=out),
    class=c('prob_travel', 'mobility.model')
  )

  out$summary <- summary.mobility.model(out)

  return(out)
}



##' Fit mobility model to data
##'
##' This function fits a variety of mobility models to a supplied movement matrix (\code{M}) and covariates (\code{D} and \code{N}) using Bayesian MCMC inference.
##' The function specifies the type of mobility model and serves as a wrapper for the \code{\link{fit_jags}} function.
##'
##' @param data a list containing mobility data and covariates. The list object must include EITHER \code{M}, \code{D}, and \code{N}
##' OR \code{M}, \code{D}, \code{N_orig}, and \code{N_dest}. Details about each item are as follows:
##' \describe{
##'   \item{M}{named matrix of trip counts between all \eqn{ij} location pairs}
##'   \item{D}{named matrix of distances between all \eqn{ij} location pairs}
##'   \item{N}{named vector of population sizes for all locations (either N or both N_orig and N_dest must be supplied)}
##'   \item{N_orig}{named vector of population sizes for each origin}
##'   \item{N_dest}{named vector of population sizes for each destination}
##' }
##' @param model character vector indicating which mobility model to fit to the data. Mobility models include: \code{'gravity'},
##' \code{'radiation'}, and \code{'departure-diffusion'}.
##' @param type character vector indicating the particular sub-type of mobility model to fit.
##' \describe{
##'   \item{Gravity model types include:}{\code{basic}, \code{transport}, \code{power}, \code{exp}, \code{power_norm}, \code{exp_norm}, and \code{Marshall}}
##'   \item{Radiation model types include:}{\code{basic} and \code{finite}}
##'   \item{Departure-diffusion model types include:}{\code{power}, \code{exp}, and \code{radiation}}
##'   }
##' See model list vignette for more detailed description of each model type.
##' @param hierarchical Applies only to the \code{'departure-diffusion'} model. A logical argument indicating whether or not to fit \eqn{\tau}
##' as a single parameter or hierarchically (as \eqn{\tau_{pop}} and \eqn{\tau_i}).
##' @param n_chain number of MCMC sampling chains
##' @param n_burn number of iterations to discard before sampling of chains begins (burn in)
##' @param n_samp number of iterations to sample each chain
##' @param n_thin interval to thin samples
##' @param DIC logical indicating whether or not to calculate the Deviance Information Criterion (DIC) (default = \code{FALSE})
##' @param parallel logical indicating whether or not to run MCMC chains in parallel or sequentially (default = \code{FALSE})
##'
##' @return An object of class \code{mobility.model} containing model information, data, and fitted model parameters
##'
##' @author John Giles
##'
##' @example R/examples/mobility.R
##'
##' @family model
##'
##' @export
##'

mobility <- function(
  data,
  model,
  type,
  hierarchical=FALSE,
  n_chain=2,
  n_burn=1000,
  n_samp=1000,
  n_thin=1,
  DIC=FALSE,
  parallel=FALSE
) {

  if (!is.list(data)) stop("Argument 'data' must be a list object")

  if( all(c('M', 'D', 'N') %in% names(data)) ) {

    M <- round(data$M)
    D <- data$D
    N_orig <- N_dest <- data$N

  } else if ( all(c('M', 'D', 'N_orig', 'N_dest') %in% names(data)) )  {

    for(i in seq_along(data)) assign(names(data)[i], data[[i]])

  } else {

    stop("Argument 'data' must be a list containing EITHER c(M, D, N) OR c(M, D, N_orig, N_dest)")

  }

  # Check data formats
  msg <- 'M must be a named numeric matrix'
  if (!(is.matrix(M) & is.numeric(M))) stop(msg)
  if (any(unlist(lapply(dimnames(M), is.null)))) stop(msg)

  msg <- 'D must be a named numeric matrix'
  if (!(is.matrix(D) & is.numeric(D))) stop(msg)
  if (any(unlist(lapply(dimnames(D), is.null)))) stop(msg)

  msg <- 'N_orig must be a named numeric vector'
  if (!(is.vector(N_orig) & is.numeric(N_orig))) stop(msg)
  if (is.null(names(N_orig))) stop(msg)

  msg <- 'N_dest must be a named numeric vector'
  if (!(is.vector(N_dest) & is.numeric(N_dest))) stop(msg)
  if (is.null(names(N_dest))) stop(msg)

  # check data dimensions
  check_dims <- c(
    identical(dim(M)[1], dim(D)[1]),
    identical(dim(M)[1], dim(D)[2]),
    identical(dim(M)[1], length(N_orig)),
    identical(dim(M)[2], length(N_dest))
  )

  if (any(!check_dims)) stop('Dimensions of input data do not match')

  # check data names
  check_names <- c(
    identical(dimnames(M)[[1]], dimnames(D)[[1]]),
    identical(dimnames(M)[[2]], dimnames(D)[[2]]),
    identical(dimnames(M)[[1]], names(N_orig)),
    identical(dimnames(M)[[2]], names(N_dest))
  )

  if (any(!check_names)) stop('Dimension names of input data do not match')

  vals <- c(D, N_orig, N_dest)
  if (any(is.na(vals)) | any(is.nan(vals))) stop('D and N are covariates and cannot contain missing values')

  if (any(is.nan(M))) M[is.nan(M)] <- NA

  message(
    paste('::Fitting', type, model, 'model for', dim(M)[1], 'origins and', dim(M)[2], 'destinations::', sep=' ')
  )

  if (model == 'gravity') {

    return(
      fit_gravity(M, D, N_orig, N_dest, type,
                  n_chain, n_burn, n_samp, n_thin,
                  DIC, parallel)
    )


  } else if (model == 'radiation') {

    return(fit_radiation(M, D, N_orig, N_dest, type))

  } else if (model == 'departure-diffusion') {

    return(
      fit_departure_diffusion(M, D, N_orig, N_dest,
                              type, hierarchical,
                              n_chain, n_burn, n_samp, n_thin,
                              DIC, parallel)
    )

  } else {

    stop ('Unknown mobility model')

  }
}


# Fit gravity model to movement matrix

fit_gravity <- function(
  M,
  D,
  N_orig=NULL,
  N_dest=NULL,
  type,
  n_chain=2,
  n_burn=1000,
  n_samp=1000,
  n_thin=1,
  DIC=FALSE,
  parallel=FALSE
) {

  jags_data <- list(
    M=M,
    D=D,
    N_orig=N_orig,
    N_dest=N_dest
  )

  if (type == 'basic') {

    jags_model <- "
      model {

        for (i in 1:length(N_orig)) {
          for (j in 1:length(N_dest)) {

            M[i,j] ~ dpois( theta * ((N_orig[i] * N_dest[j]) / (D[i,j]+0.001)) )

          }
        }

        # Priors
        theta ~ dgamma(0.001,0.001)

      }"

    params <- 'theta'


  } else if (type == 'transport') {

    jags_model <- "
      model {

        for (i in 1:length(N_orig)) {
          for (j in 1:length(N_dest)) {

            M[i,j] ~ dpois( theta * N_orig[i] * N_dest[j] * (D[i,j]+0.001)^(-gamma) )

          }
        }

        # Priors
        theta ~ dgamma(0.001,0.001)
        gamma ~ dgamma(1,1)

      }"

    params <- c('theta', 'gamma')

  } else if (type == 'power') {

    jags_model <- "
      model {

        for (i in 1:length(N_orig)) {
          for (j in 1:length(N_dest)) {

            M[i,j] ~ dpois( theta * (N_orig[i]^omega_1) * (N_dest[j]^omega_2) * (D[i,j]+0.001)^(-gamma) )

          }
        }

        # Priors
        theta ~ dgamma(0.001,0.001)
        omega_1 ~ dgamma(1,1)
        omega_2 ~ dgamma(1,1)
        gamma ~ dgamma(1,1)

      }"

    params <- c('omega_1', 'omega_2', 'theta', 'gamma')

  } else if (type == 'exp') {

    jags_model <- "
      model {

        for (i in 1:length(N_orig)) {
          for (j in 1:length(N_dest)) {

            M[i,j] ~ dpois(theta * (N_orig[i]^omega_1) * (N_dest[j]^omega_2) * exp((-D[i,j]+0.001)/delta))

          }
        }

        # Priors
        theta ~ dgamma(0.001,0.001)
        omega_1 ~ dgamma(1,1)
        omega_2 ~ dgamma(1,1)
        delta ~ dnorm(mean(D), 1/sd(D)) T(0,)

      }"

    params <- c('omega_1', 'omega_2', 'theta', 'delta')

  } else if (type == 'power_norm') {

    jags_model <- "
      model {

        for (i in 1:length(N_orig)) {
          for (j in 1:length(N_dest)) {

            M[i,j] ~ dpois( theta * N_orig[i] * (c[i,j]/sum(c[i,])) )

          }
        }

        for (i in 1:length(N_orig)) {
          for (j in 1:length(N_dest)) {

            c[i,j] <- (N_dest[j]^omega) * (D[i,j]+0.001)^(-gamma)

          }
        }

        # Priors
        theta ~ dgamma(0.001,0.001)
        omega ~ dgamma(1,1)
        gamma ~ dgamma(1,1)

      }"

    params <- c('theta', 'omega', 'gamma')

  } else if (type == 'exp_norm') {

    jags_model <- "
      model {

        for (i in 1:length(N_orig)) {
          for (j in 1:length(N_dest)) {

            M[i,j] ~ dpois( theta * N_orig[i] * (c[i,j]/sum(c[i,])) )

          }
        }

        for (i in 1:length(N_orig)) {
          for (j in 1:length(N_dest)) {

            c[i,j] <- (N_dest[j]^omega) * exp((-D[i,j]+0.001)/delta)

          }
        }

        # Priors
        theta ~ dgamma(0.001,0.001)
        omega ~ dgamma(1,1)
        delta ~ dnorm(mean(D), 1/sd(D)) T(0,)

      }"

    params <- c('theta', 'omega', 'delta')

  } else if (type == 'scaled_power') {

    jags_model <- "
      model {

        for (i in 1:length(N_orig)) {
          for (j in 1:length(N_dest)) {

            M[i,j] ~ dpois( (N_dest[j]^tau) * ((1+(D[i,j]/rho))^-alpha)  )

          }
        }

        # Priors
        tau ~ dgamma(1, 1)
        rho ~  dgamma(0.001, 0.001)
        alpha ~ dgamma(1, 1)

      }"

    params <- c('tau', 'rho', 'alpha')

  } else {

    stop('Unknown model type')

  }

  mod <- fit_jags(jags_data=jags_data,
                  jags_model=jags_model,
                  params=params,
                  n_chain=n_chain,
                  n_burn=n_burn,
                  n_samp=n_samp,
                  n_thin=n_thin,
                  DIC=DIC,
                  parallel=parallel)


  out <- structure(
    list(model='gravity',
         type=type,
         n_chain=n_chain,
         n_burn=n_burn,
         n_samp=n_samp,
         n_thin=n_thin,
         DIC=DIC,
         data=jags_data,
         params=mod),
    class='mobility.model'
  )

  out$summary <- summary.mobility.model(out)

  return(out)

}



# Fit radiation model to movement matrix

fit_radiation <- function(
  M,
  D,
  N_orig=NULL,
  N_dest=NULL,
  type
) {

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

  if (type == 'basic') {

    R <- D; R[,] <- NA

    for (i in 1:nrow(D)) {
      for (j in 1:ncol(D)) {

        num <- as.numeric(N_orig[i])*as.numeric(N_dest[j])
        den <- (N_orig[i]+S[i,j])*(N_orig[i]+N_dest[j]+S[i,j])

        R[i,j] <- sum(M[i,], na.rm=TRUE) * (num/den)

      }
    }

  } else if (type == 'finite') {

    R <- D; R[,] <- NA

    for (i in 1:nrow(D)) {
      for (j in 1:ncol(D)) {

        num <- as.numeric(N_orig[i])*as.numeric(N_dest[j])
        den <- (N_orig[i]+S[i,j])*(N_orig[i]+N_dest[j]+S[i,j])
        tot_pop <- sum(N_orig, na.rm=TRUE) + sum(N_dest[which(!(N_dest %in% N_orig))], na.rm=TRUE)

        R[i,j] <- ( sum(M[i,], na.rm=TRUE)/(1-(N_orig[i]/tot_pop)) ) * (num/den)

      }
    }

  } else {

    stop('Unknown model type')

  }

  return(
    structure(
      list(
        model='radiation',
        type=type,
        data=list(M=M,
                  D=D,
                  N_orig=N_orig,
                  N_dest=N_dest,
                  S=S),
        params=R),
      class='mobility.model'
    )
  )
}


# Fit departure-diffusion model to movement matrix

fit_departure_diffusion <- function(
  M,
  D,
  N_orig=NULL,
  N_dest=NULL,
  type,
  hierarchical=FALSE,
  n_chain=2,
  n_burn=1000,
  n_samp=1000,
  n_thin=1,
  DIC=FALSE,
  parallel=FALSE
) {

  jags_data <- list(
    M=M,
    D=D,
    N_orig=N_orig,
    N_dest=N_dest
  )

  params <- c('tau', 'omega', 'theta')

  if (type == 'power') {

    kernel <- "(N_dest[j]^omega) * (D[i,j]+0.001)^(-gamma)"
    kernel_param <- 'gamma'
    kernel_prior <- 'gamma ~ dgamma(1, 1)'

  } else if (type == 'exp') {

    kernel <- "(N_dest[j]^omega) * exp((-D[i,j]+0.001)/delta)"
    kernel_param <- 'delta'
    kernel_prior <- 'delta ~ dnorm(mean(D), 1/sd(D)) T(0,)'

  } else if (type == 'radiation') {

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

    jags_data <- list(
      M=M,
      D=D,
      N_orig=N_orig,
      N_dest=N_dest,
      S=S
    )

    kernel <- "N_dest[j]/((N_orig[i]+S[i,j])*(N_orig[i]+N_dest[j]+S[i,j]))"
    kernel_param <- kernel_prior <- NULL

  } else {

    stop('Unknown model type')

  }

  if (hierarchical == FALSE) {

    jags_model <- paste0("
      model {

        for (i in 1:length(N_orig)) {
          for (j in 1:length(N_dest)) {

            M[i,j] ~ dpois(lambda[i,j])

            lambda[i,j] <- ifelse(
              i == j,
              theta * N_orig[i] * (1 - tau),
              theta * N_orig[i] * tau * (x[i,j]/sum(x[i,]))
            )

          }
        }


        for (i in 1:length(N_orig)) {
          for (j in 1:length(N_dest)) {

            x[i,j] <- ifelse(
              i == j,
              0,
              ", kernel, "
            )
          }
        }

        # Priors
        tau ~ dbeta(1, 1)
        theta ~ dgamma(0.001,0.001)
        omega ~ dgamma(1,1)
        ", kernel_prior, "

      }")

    params <- c(params, kernel_param)

  } else if (hierarchical == TRUE) {

    jags_model <- paste0("
      model {

        for (i in 1:length(N_orig)) {
          for (j in 1:length(N_dest)) {

            M[i,j] ~ dpois(lambda[i,j])

            lambda[i,j] <- ifelse(
              i == j,
              theta * N_orig[i] * (1 - tau[i]),
              theta * N_orig[i] * tau[i] * (x[i,j]/sum(x[i,]))
            )

          }

          tau[i] ~ dbeta(1+alpha, 1+beta) # Departure process

        }


        for (i in 1:length(N_orig)) {
          for (j in 1:length(N_dest)) {

            x[i,j] <- ifelse(
              i == j,
              0,
              ", kernel, "
            )
          }
        }

        # Priors
        tau_pop ~ dbeta(1+alpha, 1+beta)
        alpha ~ dgamma(0.1, 0.1) # priors 1+alpha and 1+beta allow tau[i] parameters to start with flat dbeta(1,1) distribution
        beta ~ dgamma(0.1, 0.1)
        theta ~ dgamma(0.001,0.001)
        omega ~ dgamma(1,1)
        ", kernel_prior, "

      }")

    params <- c('tau_pop', params, kernel_param)

  } else {

    stop("Unknown model type")

  }

  suppressWarnings(
    mod <- fit_jags(jags_data=jags_data,
                    jags_model=jags_model,
                    params=params,
                    n_chain=n_chain,
                    n_burn=n_burn,
                    n_samp=n_samp,
                    n_thin=n_thin,
                    DIC=DIC,
                    parallel=parallel)

  )

  out <- structure(
    list(model='departure-diffusion',
         type=type,
         hierarchical=hierarchical,
         n_chain=n_chain,
         n_burn=n_burn,
         n_samp=n_samp,
         n_thin=n_thin,
         DIC=DIC,
         data=jags_data,
         params=mod),
    class='mobility.model'
  )

  out$summary <- summary.mobility.model(out)

  return(out)
}


