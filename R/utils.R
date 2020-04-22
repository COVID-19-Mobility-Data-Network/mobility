##' Build distance matrix from XY coordinates
##'
##' This function builds the pairwise distance matrix from vectors of XY coordinates and associated names.
##'
##' @param x vector giving X coordinates
##' @param y vector giving Y coordinates
##' @param id vector of names for each location
##'
##' @return a named matrix of pairwise distances among locations
##'
##' @author John Giles
##'
##' @example R/examples/get_distance_matrix.R
##'
##' @family data synthesis
##'
##' @export
##'

get_distance_matrix <- function(x,   # x coord
                                y,   # y coord
                                id   # name associated with each element
) {
  xy <- cbind(x, y)
  window <- spatstat::bounding.box.xy(xy)
  out <- spatstat::pairdist(spatstat::as.ppp(xy, window, check=FALSE))
  dimnames(out) <- list(origin=id, destination=id)
  out[order(dimnames(out)$origin), order(dimnames(out)$destination)]
}

##' Simulate connectivity values using gravity model
##'
##' This function uses the gravity model formula to simulate a connectivity matrix based on the supplied model parameters. The
##' gravity model formula uses a Gamma distribution as the dispersal kernel in the denominator. A null model (where all model parameters = 1) can be
##' simulated by supplying only population sizes (\code{N}) and pairwise distances (\code{D}).
##' \deqn{\theta * ( N_i^\omega_1 N_j^\omega_2 / f(d_ij) )}
##'
##' @param N vector of population sizes
##' @param D matrix of distances among all \eqn{ij} pairs
##' @param theta scalar giving the proportionality constant of gravity formula (default = 1)
##' @param omega_1 scalar giving exponential scaling of origin population size (default = 1)
##' @param omega_2 scalar giving exponential scaling of destination population size (default = 1)
##' @param gamma scalar giving the dispersal kernel paramater (default = 1)
##' @param counts logical indicating whether or not to return a count variable by scaling the connectivity matrix by origin population size (\eqn{N_i}) (default = FALSE)
##'
##' @return a matrix with values between 0 and 1 (if \code{counts = FALSE}) or positive integers (if \code{counts = TRUE})
##'
##' @author John Giles
##'
##' @example R/examples/sim_gravity.R
##'
##' @family simulation
##' @family gravity
##'
##' @export
##'

sim_gravity <- function(
  N,
  D,
  theta=1,
  omega_1=1,
  omega_2=1,
  gamma=1,
  counts=FALSE
) {

  if (!(identical(length(N), dim(D)[1], dim(D)[1]))) stop('Check dimensions of input data N and D')
  if (!(length(c(theta, omega_1, omega_2, gamma)) == 4)) stop('theta and omega parameters must be scalars')

  n_districts <- length(N)
  x <- f_d <- matrix(NA, n_districts, n_districts)

  for (i in 1:n_districts) {
    for (j in 1:n_districts) {

      # Gravity model
      if (i == j) {

        x[i,j] <- 0

      } else {

        f_d[i,j] <- (D[i,j]^gamma)

        x[i,j] <- exp(log(theta) + (omega_1*log(N[i]) + omega_2*log(N[j]) - log(f_d[i,j])))
      }
    }

    x[i,] <- (x[i,]/sum(x[i,]))
    if (counts == TRUE) x[i,] <- round(x[i,]*N[i])
  }

  dimnames(x) <- list(origin=dimnames(D)[[1]], destination=dimnames(D)[[2]])
  return(x)
}


##' Calculate summary statistics for a model
##'
##' This is a wrapper function for \code{\link[MCMCvis:MCMCsummary]{MCMCsummary}} that calculates summary statistics
##' for each parameter in an mcmc.list object. If the model contains deviance and penalty calculations, then
##' Deviance Information Criterion (DIC) is calculated and appended to the summary.
##'
##' @param mod an mcmc.list object
##' @param ac_lags vector of lags over which to calculate autocorrelation of samples within chains (default = c(2,5,10))
##'
##' @return a dataframe with summary statistics
##'
##' @author John Giles
##'
##' @example R/examples/summarize_mobility.R
##'
##' @export
##'

summarize_mobility <- function(mod, ac_lags=c(2,5,10)) {

  if (!(class(mod) == 'mcmc.list')) stop('Model object must be mcmc.list')

  param_names <- dimnames(mod[[1]])[[2]]
  param_DIC <- c('deviance', 'pD', 'DIC')

  out <- tryCatch({

    tmp <- MCMCvis::MCMCsummary(mod,
                            func=function(x, lags=ac_lags) {
                              acf(x, lag.max=lags[length(lags)], plot=FALSE)$acf[lags]
                            },
                            func_name=stringr::str_c('AC', ac_lags))

    names(tmp)[c(1:5,7)] <- c('Mean', 'SD', 'CI2.5', 'CI50', 'CI97.5', 'SSeff')

    if (all(param_DIC %in% param_names)) {

      tmp['pD', !(colnames(tmp) == 'Mean')] <- NA
      sel <- rownames(tmp) %in% param_DIC
      tmp <- rbind(tmp[!sel,], tmp[sel,])

    }

    tmp

  }, error = function(e) {

    message('ERROR: cannot calculate DIC for this model')

    mod <- coda::as.mcmc.list(
      lapply(mod, function(x) coda::as.mcmc(x[,!(colnames(x) %in% param_DIC)]))
    )

    tmp <- MCMCvis::MCMCsummary(mod,
                                func=function(x, lags=ac_lags) {
                                  acf(x, lag.max=lags[length(lags)], plot=FALSE)$acf[lags]
                                },
                                func_name=stringr::str_c('AC', ac_lags))

    names(tmp)[c(1:5,7)] <- c('Mean', 'SD', 'CI2.5', 'CI50', 'CI97.5', 'SSeff')
    tmp

  })

    return(out)

}