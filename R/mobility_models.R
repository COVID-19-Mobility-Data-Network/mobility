##' Fit model using JAGS
##'
##' This is a general wrapper function for fitting a model to data using the JAGS MCMC algorithm.
##' The function allows for running sampling chains in parallel and calculating the Deviance Information Criterion (DIC).
##'
##' @param jags.data a list of data objects
##' @param jags.model character string giving the model specification in JAGS/BUGS syntax
##' @param params vector of parameters to monitor
##' @param n.chain number of MCMC sampling chains
##' @param n.burn number of iterations to discard before sampling of chains begins (burn in)
##' @param n.samp number of iterations to sample each chain
##' @param n.thin interval to thin samples
##' @param DIC logical indicating whether or not to calculate the Deviance Information Criterion (DIC)
##' @param parallel logical indicating whether or not to run MCMC chains in parallel or sequentially (default = FALSE)
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

fit.jags <- function(
  jags.data,
  jags.model,
  params,
  n.chain=2,
  n.burn=1000,
  n.samp=1000,
  n.thin=1,
  DIC=FALSE,
  parallel=FALSE
) {


  if (parallel == FALSE) {

    rjags::load.module('lecuyer')

    init.list <- replicate(n.chain,
                           list(.RNG.name='lecuyer::RngStream',
                                .RNG.seed= sample(1:1e6, 1)),
                           simplify=FALSE)

    if (DIC == FALSE) {

      mod <- rjags::jags.model(file=textConnection(jags.model),
                               data=jags.data,
                               inits=init.list,
                               n.chains=n.chain,
                               n.adapt=0)

      Matrix::update(mod, n.burn)

      mod <- rjags::jags.samples(mod,
                                 variable.names=params,
                                 n.iter=n.samp,
                                 thin=n.thin)

    } else if (DIC == TRUE) {

      if (n.chains < 2) stop('Calculation of DIC requires at least 2 sampling chains')

      rjags::load.module('dic')

      mod <- jags.model(file=textConnection(jags.model),
                        data=jags.data,
                        inits=init.list,
                        n.chains=n.chain,
                        n.adapt=0)

      Matrix::update(mod, n.burn)

      mod <- jags.samples(mod,
                          variable.names=c(params, 'deviance', 'pD'),
                          n.iter=n.samp,
                          thin=n.thin) # thin in jags samples reduces total samps rather than multiplying it

      # clean up penalty output
      tmp <- mod[[which(!(names(mod) %in% c('deviance', 'pD')))[1]]]
      mod$pD <- array(mean(mod$pD), dim=dim(tmp))
      attributes(mod$pD) <- attributes(tmp)
      attributes(mod$pD)$varname <- 'pD'
    }

    # return mcmc list object
    return(rjags.mcmc.list(mod))

  } else if (parallel == TRUE) {

    if(n.chain > parallel::detectCores()) warning('Number of sampling chains greater than available cores')

    if (DIC == FALSE) {

      cl <- parallel::makeCluster(n.chain)
      doParallel::registerDoParallel(cl)

      out <- foreach(i=1:n.chain, .combine='rjags.combine', .packages=c('rjags', 'abind')) %dopar% {

        rjags::load.module('lecuyer')

        mod <- rjags::jags.model(file=textConnection(jags.model),
                                 data=jags.data,
                                 inits=list(.RNG.name='lecuyer::RngStream',
                                            .RNG.seed=sample(1:1e6, 1)),
                                 n.chains=1,
                                 n.adapt=0)

        Matrix::update(mod, n.burn)

        rjags::jags.samples(mod,
                            variable.names=params,
                            n.iter=n.samp,
                            thin=n.thin)
      }

      return(rjags.mcmc.list(out))

    } else if (DIC == TRUE) {

      if (n.chain < 2) stop('Calculation of DIC requires at least 2 sampling chains')

      cl <- parallel::makeCluster(n.chain/2)
      doParallel::registerDoParallel(cl)

      out <- foreach(i=1:(n.chain/2), .combine='rjags.combine', .packages=c('rjags', 'abind')) %dopar% {

        rjags::load.module('lecuyer')
        rjags::load.module('dic')

        init.list <- replicate(2,
                               list(.RNG.name='lecuyer::RngStream',
                                    .RNG.seed= sample(1:1e6, 1)),
                               simplify=FALSE)

        mod <- rjags::jags.model(file=textConnection(jags.model),
                                 data=jags.data,
                                 inits=init.list,
                                 n.chains=2,
                                 n.adapt=0)

        Matrix::update(mod, n.burn)

        mod <- rjags::jags.samples(mod,
                                   variable.names=c(params, 'deviance', 'pD'),
                                   n.iter=n.samp,
                                   thin=n.thin)

        # clean up penalty output
        tmp <- mod[[which(!(names(mod) %in% c('deviance', 'pD')))[1]]]
        mod$pD <- array(mean(mod$pD), dim=dim(tmp))
        attributes(mod$pD) <- attributes(tmp)
        attributes(mod$pD)$varname <- 'pD'

        mod
      }

      return(rjags.mcmc.list(out))
    }

    parallel::stopCluster(cl)
  }
}



##' Convert rjags object to mcmc.list
##'
##' This function converts an rjags object to an mcmc.list.
##'
##' @param x an rjags list
##'
##' @return an \code{\link[coda:mcmc.list]{mcmc.list}} object
##'
##' @author John Giles
##'
##' @export
##'

rjags.mcmc.list <- function(x) {

  n.chain <- max(unlist(lapply(x, function(x) dim(x)[length(dim(x))])))

  out <- foreach(i=1:n.chain) %do% {

    coda::as.mcmc(
      do.call(
        cbind,
        lapply(
          x,
          function(x) {

            n.samp <- dim(x)[2]
            samps <- x[,,i]

            if (!is.null(dim(samps))) {

              param.names <- stringr::str_c(attributes(x)$varname, 1:nrow(samps), sep='_')
              return(matrix(t(samps), nrow=n.samp, dimnames=list(NULL, param.names)))

            } else {

              return(matrix(samps, nrow=n.samp, byrow=TRUE, dimnames=list(NULL, attributes(x)$varname)))
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
##' This function combines two rjags objects.
##'
##' @param x an rjags list
##'
##' @return an rjags list
##'
##' @author John Giles
##'
##' @export
##'

rjags.combine <- function(a, b) {

  out <- a

  for (i in seq_along(out)) {

    out[[i]] <- abind::abind(a[[i]], b[[i]], along=length(dim(a[[i]])))
    atts <- attributes(a[[i]])
    atts$dim <- dim(out[[i]])
    attributes(out[[i]]) <- atts
  }

  out
}



##' Calculate summary statistics for a model
##'
##' This is a wrapper function for \code{\link[MCMCvis:MCMCsummary]{MCMCsummary}} that calculates summary statistics
##' for each parameter in an mcmc.list object. If the model contains deviance and penalty calculations, then
##' Deviance Information Criterion (DIC) is calculated and appended to the summary.
##'
##' @param mod an mcmc.list object
##' @param ac.lags vector of lags over which to calculate autocorrelation of samples within chains (default = c(2,5,10))
##'
##' @return a dataframe with summary statistics
##'
##' @author John Giles
##'
#@example R/examples/summary_hmob.R
##'
##' @family model
##'
##' @export
##'

summarize.hmob <- function(mod, ac.lags=c(2,5,10)) {

  if (!(class(mod) == 'mcmc.list')) stop('Model object must be mcmc.list')

  param.names <- dimnames(mod[[1]])[[2]]

  s <- MCMCvis::MCMCsummary(mod,
                            func=function(x, lags=ac.lags) {
                              acf(x, lag.max=lags[length(lags)], plot=FALSE)$acf[lags]
                            },
                            func_name=stringr::str_c('AC', ac.lags))

  names(s)[c(1:5,7)] <- c('Mean', 'SD', 'CI2.5', 'CI50', 'CI97.5', 'SSeff')

  if (all(c('deviance', 'pD') %in% param.names)) {

    # include DIC in summary object
    sel <- rownames(s) %in% c('deviance', 'pD')
    s <- rbind(
      s[!sel,],
      s[sel,],
      matrix(NA, nrow=1, ncol=ncol(s), dimnames=list('DIC', colnames(s)))
    )

    s['DIC', 'Mean'] <- s['deviance','Mean'] + 2*s['pD','Mean']
    s['pD', !(colnames(s) == 'Mean')] <- NA

    return(s)

  } else {

    return(s)
  }
}



##' Fit gravity model to movement matrix
##'
##' This function fits gravity model parameters to a supplied movement matrix using Bayesian MCMC inference. The function defines the model and serves as a wrapper for the \code{\link{run.jags}}
##' function in the \code{\link{runjags}} package. Gravity model formula:
##' \deqn{
##'     \theta * ( N_i^\omega_1 N_j^\omega_2 / f(d_ij) )
##' }
##'
##' @param M named matrix of trip counts among all \eqn{ij} location pairs
##' @param D named matrix of distances among all \eqn{ij} location pairs
##' @param N named vector of population sizes for all locations (either N or both N.orig and N.dest must be supplied)
##' @param N.orig named vector of population sizes for each origin
##' @param N.dest named vector of population sizes for each destination.
##' @param n.chain number of MCMC sampling chains
##' @param n.burn number of iterations to discard before sampling of chains begins (burn in)
##' @param n.samp number of iterations to sample each chain
##' @param n.thin interval to thin samples
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

fit.gravity <- function(
  M,
  D,
  N=NULL,
  N.orig=NULL,
  N.dest=NULL,
  n.chain=2,
  n.burn=1000,
  n.samp=1000,
  n.thin=1,
  prior=NULL,
  DIC=FALSE,
  parallel=FALSE
) {

  # Check data
  if (all(!is.null(N), is.null(N.orig), is.null(N.dest))) {
    N.dest <- N.orig <- N
  } else if (all(is.null(N), !is.null(N.orig), is.null(N.dest))) {
    N.dest <- N.orig
  }

  if (!(identical(dim(M)[1], dim(D)[1], length(N.orig)))) stop('Dimensions of input data must match')
  if (!(identical(dim(M)[2], dim(D)[2], length(N.dest)))) stop('Dimensions of input data must match')

  if ( !(identical(dimnames(M)[[1]], dimnames(D)[[1]])) | !(identical(dimnames(M)[[1]], names(N.orig))) ) {
    stop('Dimension names of input data do not match.')
  }

  if ( !(identical(dimnames(M)[[2]], dimnames(D)[[2]])) | !(identical(dimnames(M)[[2]], names(N.dest))) ) {
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

  if (!all(unlist(lapply(list(M, N.orig, N.dest), is.integer)))) {
    M[,] <- as.integer(M)
    N.orig[] <- as.integer(N.orig)
    N.dest[] <- as.integer(N.dest)
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

  jags.data <- list(
    M=M,
    D=D,
    N.orig=N.orig,
    N.dest=N.dest,
    prior_theta=prior$theta,
    prior_omega_1=prior$omega_1,
    prior_omega_2=prior$omega_2,
    prior_gamma=prior$gamma
  )

  jags.model <- "
  model {

  # Poisson likelihood
  for (i in 1:length(N.orig)) {
  for (j in 1:length(N.dest)) {

  M[i,j] ~ dpois(pi[i,j]*N.orig[i])
  }

  pi[i,1:length(N.dest)] <- c[i,]/sum(c[i,])
  }

  # Gravity model
  for (i in 1:length(N.orig)) {
  for (j in 1:length(N.dest)) {

  c[i,j] <- ifelse(
  i == j,
  0,
  exp(log(theta) + (omega_1*log(N.dest[i]) + omega_2*log(N.orig[j]) - log( f.d[i,j] )))
  )

  f.d[i,j] <- D[i,j]^gamma
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
    fit.jags(jags.data=jags.data,
             jags.model=jags.model,
             params=params,
             n.chain=n.chain,
             n.burn=n.burn,
             n.samp=n.samp,
             n.thin=n.thin,
             DIC=DIC,
             parallel=parallel)
  )
}



##' Estimate probability of travelling outside origin
##'
##' This function fits a hierarchical model that estimates the probability an individual travels outside their home location
##' within the time period of the survey (tau). The model estimates both the overall population-level probability of travel (tau_pop) and
##' the origin-level probability of travel (tau_i). Further this method is designed for sparse observations that typically result from
##' travel survey data, where unobserved routes of travel regress to the population mean.
##'
##' @param V.travel named vector of total number of people that reported travelling outside their home location
##' @param V.tot named vector of the total number of individuals in travel survey for each location
##' @param n.chain number of MCMC sampling chains
##' @param n.adapt number of adaptive iterations
##' @param n.burn number of iterations to discard before sampling of chains begins (burn in)
##' @param n.samp number of iterations to sample each chain
##' @param n.thin interval to thin samples
##' @param DIC logical indicating whether or not to calculate the Deviance Information Criterion (DIC)
##' @param parallel logical indicating whether or not to run MCMC chains in parallel or sequentially (default = FALSE)
##'
##' @return dataframe giving input data along with estimates of travel probability for each location
##'
##' @author John Giles
##'
##' @example R/examples/fit_prob_travel.R
##'
##' @family model
##' @family gravity
##'
##' @export
##'

fit.prob.travel <- function(
  V.travel,
  V.tot,
  n.chain=4,
  n.burn=1000,
  n.samp=1000,
  n.thin=1,
  DIC=FALSE,
  parallel=FALSE
) {

  # Check data
  if (!(identical(length(V.travel), length(V.tot)))) stop('Dimensions of input data must match')
  if (!(identical(names(V.travel), names(V.tot)))) stop('Dimension names of input data do not match.')
  if (is.null(names(V.travel))) stop('Vectors are not named.')

  # Work around for NAs
  na.fix <- any(is.na(V.travel)) | any(is.na(V.tot))
  if (na.fix) {
    sel <- complete.cases(cbind(V.travel, V.tot))
  } else {
    sel <- seq_along(V.travel)
  }

  jags.data <- list(
    V.travel=V.travel[sel],
    V.tot=V.tot[sel],
    V.travel_pop=sum(V.travel[sel]),
    V.tot_pop=sum(V.tot[sel])
  )

  jags.model <- "
  model {

  # Population-level probability of travel
  V.travel_pop ~ dbin(tau_pop, V.tot_pop)

  # Origin-level probability of travel
  for (j in 1:length(V.travel)) {

  V.travel[j] ~ dbin(tau[j], V.tot[j])
  }

  # Population-level hyper-prior
  tau_pop ~ dbeta(alpha, beta)
  alpha ~ dgamma(1, 0.1)
  beta ~ dgamma(1, 0.1)

  # Origin-level priors
  for (k in 1:length(V.travel)) {

  tau[k] ~ dbeta(alpha, beta)
  }
  }"

  params <- c('tau_pop', 'tau')

  mod <- summarize.hmob(
    fit.jags(jags.data=jags.data,
             jags.model=jags.model,
             params=params,
             n.chain=n.chain,
             n.burn=n.burn,
             n.samp=n.samp,
             n.thin=n.thin,
             DIC=DIC,
             parallel=parallel)
  )

  options(row.names=NULL, stringsAsFactors = FALSE)

  if (na.fix) {

    # Merge with missing obs
    out <- merge(
      data.frame(orig_id=names(V.travel),
                 travel=V.travel,
                 total=V.tot),
      data.frame(orig_id=names(V.travel[sel]),
                 travel=V.travel[sel],
                 total=V.tot[sel],
                 mod[-which(rownames(mod) =='tau_pop'),]),
      all=T
    )

    # Set missing obs to population mean
    for(i in which(is.na(out$Mean))) out[i, -(1:3)] <- mod['tau_pop',]
    return(out)

  } else {

    return(
      data.frame(orig_id=names(V.travel),
                 travel=V.travel,
                 total=V.tot,
                 mod[-which(rownames(mod) =='tau_pop'),])
    )
  }
}
