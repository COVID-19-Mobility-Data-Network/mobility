##' Get mobility matrix from longform data
##'
##' Takes X and Y coordinates of two locations and returns cross distance for all entries.
##'
##' @param orig vector of origin names
##' @param dest vector of destination names
##' @param value vector of observed values for each origin/destination pair
##' @param missing_obs filler value for missing observations (default = \code{NA})
##'
##' @return named numeric matrix
##'
##' @author John Giles
##'
##' @example R/examples/get_mob_matrix.R
##'
##' @family data synthesis
##'
##' @export
##'

get_mob_matrix <- function(orig,
                           dest,
                           value,
                           missing_obs='NA'
){

  if (is.factor(orig)) orig <- as.character(orig)
  if (is.factor(dest)) dest <- as.character(dest)

  fac <- factor(sort(unique(c(orig, dest))))

  m <- Matrix::formatSpMatrix(
    Matrix::sparseMatrix(i=as.integer(factor(orig, levels=levels(fac))),
                         j=as.integer(factor(dest, levels=levels(fac))),
                         x=value,
                         dims=rep(length(fac), 2)),
    zero.print=missing_obs
  )

  suppressWarnings(class(m) <- "numeric")
  dimnames(m) <- list(origin=levels(fac), destination=levels(fac))
  m[order(dimnames(m)$origin), order(dimnames(m)$destination)]
}



##' Get unique origin and destination names
##'
##' This function builds unique character string identifiers for all origins and destinations.
##'
##' @param data generalized data frame described in \code{\link{travel_data_template}}
##' @param orig logical indicating whether to include unique coordinates from origin locations (default=TRUE)
##' @param dest logical indicating whether to include unique coordinates from destination locations (default=TRUE)
##' @param adm_start highest administrative level to include in unique names (default = NULL, which uses highest observed in the data)
##' @param adm_stop lowest administrative level to include in unique names (default = NULL, which uses lowest observed in the data)
##' @param name_class character indicating whether unique names should be either a unique character string (\code{name_class = "character"}) or a unique integer code (\code{name_class = "numeric"})
##'
##' @return two column dataframe containing unique names
##'
##' @author John Giles
##'
##' @example R/examples/get_unique_ids.R
##'
##' @family utility
##'
##' @export
##'

get_unique_ids <- function(data,
                           orig=TRUE,
                           dest=TRUE,
                           adm_start=NULL,
                           adm_stop=NULL,
                           name_class="character"
) {

  if (is.null(orig) & is.null(dest)) stop('Both orig and dest cannot be FALSE')

  pads <- data.frame(
    adm_level=0:6,
    n=c(3, # admin0
        3, # admin1
        3, # admin2
        4, # admin3
        5, # admin4
        5, # admin5
        5) # admin6
  )

  if (all(orig, dest)) {

    sel <- grep('orig_adm', colnames(data))
    if (is.null(adm_start)) adm_start <- 0
    if (is.null(adm_stop)) adm_stop <- length(sel)-1
    sel <- sel[order(colnames(data)[sel])]
    sel_orig <- sel[(adm_start+1):(adm_stop+1)]

    sel <- grep('dest_adm', colnames(data))
    if (is.null(adm_start)) adm_start <- 0
    if (is.null(adm_stop)) adm_stop <- length(sel)-1
    sel <- sel[order(colnames(data)[sel])]
    sel_dest <- sel[(adm_start+1):(adm_stop+1)]


    if (name_class == "numeric") {

      for (i in seq_along(sel_orig)) {

        n_pad <- pads$n[(adm_start+1) + (i-1)]
        fac <- factor(sort(unique(c(data[,sel_orig[i]], data[,sel_dest[i]]))))

        data[,sel_orig[i]] <- stringr::str_pad(as.numeric(factor(data[,sel_orig[i]], levels=levels(fac))),
                                               width=n_pad,
                                               side='left',
                                               pad="0")

        data[,sel_dest[i]] <- stringr::str_pad(as.numeric(factor(data[,sel_dest[i]], levels=levels(fac))),
                                               width=n_pad,
                                               side='left',
                                               pad="0")
      }
    }

    orig_id <- apply(data[,sel_orig], 1, function(x) paste(x[!is.na(x)], collapse='_'))
    dest_id <- apply(data[,sel_dest], 1, function(x) paste(x[!is.na(x)], collapse='_'))
    return(data.frame(orig_id, dest_id, row.names=NULL, stringsAsFactors=FALSE))

  } else if (all(orig, !dest)) {

    sel <- grep('orig_adm', colnames(data))
    if (is.null(adm_start)) adm_start <- 0
    if (is.null(adm_stop)) adm_stop <- length(sel)-1
    sel <- sel[order(colnames(data)[sel])]
    sel_orig <- sel[(adm_start+1):(adm_stop+1)]

    if (name_class == "numeric") {

      for (i in seq_along(sel_orig)) {

        n_pad <- pads$n[(adm_start+1) + (i-1)]
        data[,sel_orig[i]] <- stringr::str_pad(as.numeric(factor(data[,sel_orig[i]])),
                                               width=n_pad,
                                               side='left',
                                               pad="0")
      }
    }

    orig_id <- apply(data[,sel_orig], 1, function(x) paste(x[!is.na(x)], collapse='_'))
    return(data.frame(orig_id, row.names=NULL, stringsAsFactors=FALSE))

  } else if (all(!orig, dest)) {

    sel <- grep('dest_adm', colnames(data))
    if (is.null(adm_start)) adm_start <- 0
    if (is.null(adm_stop)) adm_stop <- length(sel)-1
    sel <- sel[order(colnames(data)[sel])]
    sel_dest <- sel[(adm_start+1):(adm_stop+1)]

    if (name_class == "numeric") {

      for (i in seq_along(sel_dest)) {

        n_pad <- pads$n[(adm_start+1) + (i-1)]
        data[,sel_dest[i]] <- stringr::str_pad(as.numeric(factor(data[,sel_dest[i]])),
                                               width=n_pad,
                                               side='left',
                                               pad="0")
      }
    }

    dest_id <- apply(data[,sel_dest], 1, function(x) paste(x[!is.na(x)], collapse='_'))
    return(data.frame(dest_id, row.names=NULL, stringsAsFactors=FALSE))
  }
}


##' Get unique coordinates
##'
##' This function returns the coordinates for all unique origins and/or destinations in supplied data.
##'
##' @param data generalized data frame described in \code{\link{travel_data_template}} or derivative thereof
##' @param orig logical indicating whether to include unique coordinates from origin locations (default=TRUE)
##' @param dest logical indicating whether to include unique coordinates from destination locations (default=TRUE)
##'
##' @return three column dataframe containing unique coordinates and location names
##'
##' @author John Giles
##'
##' @example R/examples/get_unique_coords.R
##'
##' @family utility
##'
##' @export
##'

get_unique_coords <- function(data,
                              orig=TRUE,
                              dest=TRUE
) {

  require(dplyr)

  if (!all(c('orig_id', 'dest_id') %in% colnames(data))) {

    data <- cbind(data, get_unique_ids(data))
    warning('Added missing unique location names.')
  }

  if (is.factor(data$orig_id)) data$orig_id <- as.character(data$orig_id)
  if (is.factor(data$dest_id)) data$dest_id <- as.character(data$dest_id)

  if (all(orig, dest)) {

    out <- rbind(
      data %>%
        dplyr::rename(x=orig_x, y=orig_y, id=orig_id) %>%
        dplyr::select(x, y, id) %>%
        data.frame(),
      data %>%
        dplyr::rename(x=dest_x, y=dest_y, id=dest_id) %>%
        dplyr::select(x, y, id) %>%
        data.frame()
    )

  } else if (all(orig, !dest)) {

    out <- data %>%
      dplyr::rename(x=orig_x, y=orig_y, id=orig_id) %>%
      dplyr::select(x, y, id) %>%
      data.frame()

  } else if (all(!orig, dest)) {

    out <- data %>%
      dplyr::rename(x=dest_x, y=dest_y, id=dest_id) %>%
      dplyr::select(x, y, id) %>%
      data.frame()

  } else {

    stop('At least one of the "orig" or "dest" arguments must be TRUE')
  }

  out %>%
    dplyr::group_by(id) %>%
    dplyr::distinct(id, .keep_all = T) %>%
    dplyr::arrange(id) %>%
    data.frame()
}


##' Get unique origin and destination names
##'
##' This function returns coordinates for all unique locations in supplied data.
##'
##' @param data generalized data frame described in \code{\link{travel_data_template}} or derivative thereof
##' @param orig logical indicating whether to include origin location in unique names (default= TRUE)
##' @param dest logical indicating whether to include destination location in unique names (default= TRUE)
##'
##' @return two column dataframe containing unique names
##'
##' @author John Giles
##'
##' @example R/examples/get_pop_vec.R
##'
##' @family utility
##'
##' @export
##'

get_pop_vec <- function(data,
                        orig=TRUE,
                        dest=TRUE
) {

  require(dplyr)

  if (!all(c('orig_id', 'dest_id') %in% colnames(data))) {

    data <- cbind(data, get_unique_ids(data))
    warning('Added missing unique location names.')
  }

  if (is.factor(data$orig_id)) data$orig_id <- as.character(data$orig_id)
  if (is.factor(data$dest_id)) data$dest_id <- as.character(data$dest_id)

  if (all(orig, dest)) {

    out <- rbind(
      data %>%
        dplyr::rename(pop=orig_pop, id=orig_id) %>%
        dplyr::select(pop, id) %>%
        data.frame(),
      data %>%
        dplyr::rename(pop=dest_pop, id=dest_id) %>%
        dplyr::select(pop, id) %>%
        data.frame()
    )

  } else if (all(orig, !dest)) {

    out <- data %>%
      dplyr::rename(pop=orig_pop, id=orig_id) %>%
      dplyr::select(pop, id) %>%
      data.frame()

  } else if (all(!orig, dest)) {

    out <- data %>%
      dplyr::rename(pop=dest_pop, id=dest_id) %>%
      dplyr::select(pop, id) %>%
      data.frame()

  } else {

    stop('At least one of the "orig" or "dest" arguments must be TRUE')
  }

  out <- out %>%
    dplyr::group_by(id) %>%
    dplyr::distinct(id, .keep_all=T) %>%
    dplyr::arrange(id) %>%
    data.frame()

  setNames(out$pop, out$id)
}


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




##' Get distance matrix for two different set of coordinates
##'
##' Takes XY coordinates of two sets of locations and returns cross distance for all entries.
##'
##' @param xy1 two column matrix of XY coordinates for first group
##' @param xy2 two column matrix of XY coordinates for second group
##' @param id1 optional names for first group
##' @param id2 optional names for second group
##'
##' @return numeric scalar or vector
##'
##' @author John Giles
##'
##' @example R/examples/get_crossdist.R
##'
##' @family data synthesis
##'
##' @export
##'

get_crossdist <- function(xy1,
                          xy2,
                          id1=NULL,
                          id2=NULL
) {

  if (!is.null(xy1) | !is.null(xy2)) colnames(xy1) <- colnames(xy2) <- rep(NA, ncol(xy1))

  suppressWarnings(
    window <- spatstat::bounding.box.xy(rbind(xy1, xy2))
  )

  out <-  spatstat::crossdist(spatstat::as.ppp(xy1, window, check=FALSE),
                              spatstat::as.ppp(xy2, window, check=FALSE))

  dimnames(out) <- list(origin=id1, destination=id2)

  if (!is.null(id1)) out <- out[order(dimnames(out)$origin),]
  if (!is.null(id2)) out <- out[,order(dimnames(out)$destination)]

  out
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
  param_DIC <- c('DIC', 'deviance', 'pD')

  out <- tryCatch({

    tmp <- MCMCvis::MCMCsummary(mod,
                            func=function(x, lags=ac_lags) {
                              acf(x, lag.max=lags[length(lags)], plot=FALSE)$acf[lags]
                            },
                            func_name=stringr::str_c('AC', ac_lags))

    names(tmp)[c(1:5,7)] <- c('Mean', 'SD', 'CI2.5', 'CI50', 'CI97.5', 'SSeff')

    if (all(param_DIC %in% param_names)) {

      tmp['pD', !(colnames(tmp) == 'Mean')] <- NA
      tmp <- rbind(tmp[!(rownames(tmp) %in% param_DIC),], tmp[param_DIC,])
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



##' Check goodness of fit of a gravity model
##'
##' This function takes a fitted gravity model and calculates goodness of fit metrics for observed routes. If the
##' Deviance Information Criterin (DIC) was calculated in the supplied model object, it is included in output.
##' When \code{plot_check = TRUE}, two plots are shown containing the posterior distribution of trip counts compared to observed data
##' and a Normal Q-Q plot showing the quantiles of model residuals against those expected from a Normal distribution.
##' Goodness of fit metrics include:
##' \describe{
##'   \item{DIC}{\href{https://en.wikipedia.org/wiki/Deviance_information_criterion}{Deviance Information Criterion}}
##'   \item{RMSE}{\href{https://en.wikipedia.org/wiki/Root-mean-square_deviation}{Root Mean Squared Error}}
##'   \item{MAPE}{\href{https://en.wikipedia.org/wiki/Mean_absolute_percentage_error}{Mean Absolute Percent Error}}
##'   \item{R2}{\href{https://en.wikipedia.org/wiki/Coefficient_of_determination}{R-squared}}
##' }
##'
##' @param M named matrix of trip counts among all \eqn{ij} location pairs
##' @param D named matrix of distances among all \eqn{ij} location pairs
##' @param N named vector of population sizes for all locations (either N or both n_orig and n_dest must be supplied)
##' @param mod model output from either the \code{\link{fit_gravity}} or \code{\link{summarize_mobility}} functions
##' @param plot_check logical indicating whether to plot the Posterior Predictive Check and Normal Q-Q Plot (default = \code{TRUE})
##'
##' @return a list of goodness of fit measures
##'
##' @author John Giles
##'
##' @example R/examples/check_gravity.R
##'
##' @family model
##' @family gravity
##'
##' @export
##'

check_gravity <- function(M,
                          D,
                          N,
                          mod,
                          plot_check=TRUE
) {

  if (coda::is.mcmc.list(mod)) mod <- summarize_mobility(mod)

  M_hat <- sim_gravity(N=N,
                       D=D,
                       theta=mod['theta', 'Mean'],
                       omega_1=mod['omega_1', 'Mean'],
                       omega_2=mod['omega_2', 'Mean'],
                       gamma=mod['gamma', 'Mean'],
                       count=T)

  err_rsq <- err_perc <- err_rmse <- rep(NA, nrow(M))
  for(i in 1:nrow(M)) {

    sel <- which(!is.na(M[i,]))
    err_perc[i] <- Metrics::mape(M[i, sel], M_hat[i,sel])
    err_rmse[i] <- Metrics::rmse(M[i, sel], M_hat[i,sel])
    err_rsq[i] <- cor(M[i, sel], M_hat[i,sel])^2
  }

  if (plot_check) {

    sel <- which(!is.na(M))
    M <- M[sel]
    M_hat <- M_hat[sel]

    par(mfrow=c(1,2))
    dens_M <- density(M)
    dens_M_hat <- density(M_hat)
    plot(dens_M, lwd=2, col='red',
         xlab='Trip count',
         main='Posterior predictive check',
         ylim=c(0, max(c(dens_M$y, dens_M_hat$y))))
    lines(dens_M_hat, lwd=2)

    err <- M - M_hat
    qqnorm(err, cex=1.25)
    qqline(err, lwd=2, col=2)
  }

  if ('DIC' %in% rownames(mod)) {

    return(
      list(DIC=mod['DIC', 'Mean'],
           RMSE=mean(err_rmse, na.rm=T),
           MAPE=mean(err_perc, na.rm=T),
           R2=mean(err_rsq, na.rm=T))
    )

  } else {

    list(RMSE=mean(err_rmse, na.rm=T),
         MAPE=mean(err_perc, na.rm=T),
         R2=mean(err_rsq, na.rm=T))
  }
}




##' Simulate connectivity values using gravity model
##'
##' This function uses the gravity model formula to simulate a connectivity matrix based on the supplied model parameters. The
##' gravity model formula uses a Gamma distribution as the dispersal kernel in the denominator. A null model (where all model parameters = 1) can be
##' simulated by supplying only population sizes (\code{N}) and pairwise distances (\code{D}).
##' \deqn{\theta * ( N_i^\omega_1 N_j^\omega_2 / f(d_ij) )}
##'
##' @param N vector of population sizes
##' @param D matrix giving distances among the origins and destinations
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



##' Get parameters for Beta distribution
##'
##' This function finds the two shape parameters for the Beta distribution of a random variable between 0 and 1.
##'
##' @param mu scalar or vector giving the mean of the proportion
##' @param sigma scalar or vector giving the standard deviation of the proportion
##'
##' @return A list containing the two shape parameters of the Beta distribution
##'
##' @author John Giles
##'
##' @example R/examples/get_beta_params.R
##'
##' @family simulation
##'
##' @export
##'


get_beta_params <- function(
  mu,
  sigma
) {

  shape1 <- ((1-mu) / sigma - 1/mu) * mu^2
  list(shape1=shape1, shape2=shape1 * (1 / mu-1))
}




##' Simulate probability of leaving origin
##'
##' This function simulates one stochastic realization of the proportion of individuals that leave origin \eqn{i}.
##' The function takes posterior estimates of the mean (\code{mu}) and standard deviation (\code{sigma})
##' for each origin \eqn{i}, and then derives the shape and rate parameters for the Beta distribution. Simulated values are random
##' draws from this Beta distribution.
##'
##' @param mu scalar or vector giving mean probability of leaving origin
##' @param sigma scalar or vector giving standard deviation of the probability of leaving origin
##' @param id optional scalar or vector giving name(s) of origin
##'
##' @return a numeric scalar or vector with values between 0 and 1
##'
##' @author John Giles
##'
##' @example R/examples/sim_prob_travel.R
##'
##' @family simulation
##'
##' @export
##'

sim_prob_travel <- function(mu,
                            sigma,
                            id=NULL
){

  if (any(c(mu > 1, mu < 0, sigma > 1, sigma < 0))) stop('mu and sigma must be between 0 and 1')
  if (!(length(mu) == length(sigma))) stop('mu and sigma must have same length')

  bp <- get_beta_params(mu, sigma)

  out <- rep(NA, length(mu))

  for (i in seq_along(mu)) {

    out[i] <- rbeta(1, bp$shape1[i], bp$shape2[i])
  }

  if (!is.null(id)) names(out) <- id

  return(out)
}
