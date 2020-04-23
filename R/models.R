##' Fit model using JAGS
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

      cl <- parallel::makeCluster(n_chain)
      doParallel::registerDoParallel(cl)

      out <- foreach(i=1:n_chain, .combine='combine_rjags', .packages=c('rjags', 'abind')) %dopar% {

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

      cl <- parallel::makeCluster(n_chain/2)
      doParallel::registerDoParallel(cl)

      out <- foreach(i=1:(n_chain/2), .combine='combine_rjags', .packages=c('rjags', 'abind')) %dopar% {

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




##' Convert rjags object to mcmc.list
##'
##' This function converts an rjags model object to an \code{\link[coda:mcmc.list]{mcmc.list}} object.
##'
##' @param x an rjags list
##'
##' @return an \code{\link[coda:mcmc.list]{mcmc.list}} object
##'
##' @author John Giles
##'
##' @export
##'

rjags_to_mcmc <- function(x) {

  n_chain <- max(unlist(lapply(x, function(x) dim(x)[length(dim(x))])))

  out <- foreach(i=1:n_chain) %do% {

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


##' Combine two rjags objects
##'
##' This function combines two rjags model objects.
##'
##' @param a an rjags list
##' @param b an rjags list
##'
##' @return an rjags list containing model ouput from \code{a} and \code{b}
##'
##' @author John Giles
##'
##' @export
##'

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


##' Fit gravity model to movement matrix
##'
##' This function fits gravity model parameters to a supplied movement matrix using Bayesian MCMC inference. The function defines the model and serves as a wrapper for the \code{\link{run_jags}}
##' function in the \code{\link{runjags}} package. Gravity model formula:
##' \deqn{
##'     \theta * ( N_i^\omega_1 N_j^\omega_2 / f(d_ij) )
##' }
##'
##' @param M named matrix of trip counts among all \eqn{ij} location pairs
##' @param D named matrix of distances among all \eqn{ij} location pairs
##' @param N named vector of population sizes for all locations (either N or both n_orig and n_dest must be supplied)
##' @param N_orig named vector of population sizes for each origin
##' @param N_dest named vector of population sizes for each destination_
##' @param n_chain number of MCMC sampling chains
##' @param n_burn number of iterations to discard before sampling of chains begins (burn in)
##' @param n_samp number of iterations to sample each chain
##' @param n_thin interval to thin samples
##' @param DIC logical indicating whether or not to calculate the Deviance Information Criterion (DIC) (default = FALSE)
##' @param parallel logical indicating whether or not to run MCMC chains in parallel or sequentially (default = FALSE)
##'
##' @return a runjags model object containing fitted gravity model paramters
##'
##' @author John Giles
##'
##' @example R/examples/fit_gravity.R
##'
##' @family model
##' @family gravity
##'
##' @export
##'

fit_gravity <- function(
  M,
  D,
  N=NULL,
  N_orig=NULL,
  N_dest=NULL,
  n_chain=2,
  n_burn=1000,
  n_samp=1000,
  n_thin=1,
  prior=NULL,
  DIC=FALSE,
  parallel=FALSE
) {

  # Check data
  if (all(!is.null(N), is.null(N_orig), is.null(N_dest))) {
    N_dest <- N_orig <- N
  } else if (all(is.null(N), !is.null(N_orig), is.null(N_dest))) {
    N_dest <- N_orig
  }

  if (!(identical(dim(M)[1], dim(D)[1], length(N_orig)))) stop('Dimensions of input data must match')
  if (!(identical(dim(M)[2], dim(D)[2], length(N_dest)))) stop('Dimensions of input data must match')

  if ( !(identical(dimnames(M)[[1]], dimnames(D)[[1]])) | !(identical(dimnames(M)[[1]], names(N_orig))) ) {
    stop('Dimension names of input data do not match.')
  }

  if ( !(identical(dimnames(M)[[2]], dimnames(D)[[2]])) | !(identical(dimnames(M)[[2]], names(N_dest))) ) {
    stop('Dimension names of input data do not match.')
  }

  message(
    paste('::Fitting gravity model for',
          dim(M)[1],
          'origins and',
          dim(M)[2],
          'destinations::',
          sep=' ')
  )

  if (!all(unlist(lapply(list(M, N_orig, N_dest), is.integer)))) {
    M[,] <- as.integer(M)
    N_orig[] <- as.integer(N_orig)
    N_dest[] <- as.integer(N_dest)
  }

  if (is.null(prior)) {

    message('Using uniformative priors')
    prior <- c(1, 0.05)
    prior <- list(theta=prior,
                  omega_1=prior,
                  omega_2=prior,
                  gamma=prior)

  } else {

    message('Using supplied informative priors')
  }

  jags_data <- list(
    M=M,
    D=D,
    N_orig=N_orig,
    N_dest=N_dest,
    prior_theta=prior$theta,
    prior_omega_1=prior$omega_1,
    prior_omega_2=prior$omega_2,
    prior_gamma=prior$gamma
  )

  jags_model <- "
  model {

    # Poisson likelihood
    for (i in 1:length(N_orig)) {
      for (j in 1:length(N_dest)) {

        M[i,j] ~ dpois(pi_hat[i,j]*N_orig[i])
      }

      pi_hat[i,1:length(N_dest)] <- x[i,]/sum(x[i,])
    }

    # Gravity model
    for (i in 1:length(N_orig)) {
      for (j in 1:length(N_dest)) {

        x[i,j] <- ifelse(
          i == j,
          0,
          exp(log(theta) + (omega_1*log(N_dest[i]) + omega_2*log(N_orig[j]) - log( f_d[i,j] )))
        )

        f_d[i,j] <- D[i,j]^gamma
      }
    }

    # Priors
    theta ~ dgamma(prior_theta[1], prior_theta[2])
    omega_1 ~ dgamma(prior_omega_1[1], prior_omega_1[2])
    omega_2 ~ dgamma(prior_omega_2[1], prior_omega_2[2])
    gamma ~ dgamma(prior_gamma[1], prior_gamma[2])

  }"

  params <- c('omega_1', 'omega_2', 'theta', 'gamma')

  return(
    fit_jags(jags_data=jags_data,
             jags_model=jags_model,
             params=params,
             n_chain=n_chain,
             n_burn=n_burn,
             n_samp=n_samp,
             n_thin=n_thin,
             DIC=DIC,
             parallel=parallel)
  )
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
##' @param format_locations logical indicating which format to return results:
##' \describe{
##'   \item{\code{FALSE}}{returns only summary of estimated parameters (default)}
##'   \item{\code{TRUE}}{returns probability of travel with input data, including missing locations}
##' }
##'
##' @return dataframe giving input data along with estimates of travel probability for each location
##'
##' @author John Giles
##'
##' @example R/examples/fit_prob_travel.R
##'
##' @family model
##'
##' @export
##'

fit_prob_travel <- function(
  travel,
  total,
  n_chain=4,
  n_burn=1000,
  n_samp=1000,
  n_thin=1,
  DIC=FALSE,
  parallel=FALSE,
  format_locations=FALSE
) {

  # Check data
  if (!(identical(length(travel), length(total)))) stop('Dimensions of input data must match')
  if (!(identical(names(travel), names(total)))) stop('Dimension names of input data do not match.')
  if (is.null(names(travel))) stop('Vectors are not named.')

  # Work around for NAs
  na.fix <- any(is.na(travel)) | any(is.na(total))
  if (na.fix) {
    sel <- complete.cases(cbind(travel, total))
  } else {
    sel <- seq_along(travel)
  }

  jags_data <- list(
    travel=travel[sel],
    total=total[sel],
    travel_pop=sum(travel[sel]),
    total_pop=sum(total[sel])
  )

  jags_model <- "
  model {

    # Population-level probability of travel
    travel_pop ~ dbin(tau_pop, total_pop)

    # Origin-level probability of travel
    for (j in 1:length(travel)) {

      travel[j] ~ dbin(tau[j], total[j])
    }

    # Population-level hyper-prior
    tau_pop ~ dbeta(alpha, beta)
    alpha ~ dgamma(1, 0.1)
    beta ~ dgamma(1, 0.1)

    # Origin-level priors
    for (k in 1:length(travel)) {

      tau[k] ~ dbeta(alpha, beta)
    }
  }"

  params <- c('tau_pop', 'tau')

  mod <- summarize_mobility(
    fit_jags(jags_data=jags_data,
             jags_model=jags_model,
             params=params,
             n_chain=n_chain,
             n_burn=n_burn,
             n_samp=n_samp,
             n_thin=n_thin,
             DIC=DIC,
             parallel=parallel)
  )

  if (!format_locations) {

    return(mod)

  } else if (format_locations) {

    options(row.names=NULL, stringsAsFactors=FALSE)
    ignore <- 'tau_pop'
    if (DIC) ignore <- c(ignore, "DIC", "deviance", "pD")

    if (na.fix) {

      # Merge with missing obs
      out <- merge(
        data.frame(orig_id=names(travel),
                   travel=travel,
                   total=total),
        data.frame(orig_id=names(travel[sel]),
                   travel=travel[sel],
                   total=total[sel],
                   mod[-which(rownames(mod) %in% ignore),]),
        all=T
      )

      # Set missing obs to population mean
      for(i in which(is.na(out$Mean))) out[i, -(1:3)] <- mod['tau_pop',]
      return(out)

    } else {

      return(
        data.frame(orig_id=names(travel),
                   travel=travel,
                   total=total,
                   mod[-which(rownames(mod) %in% ignore),])
      )
    }
  }
}


