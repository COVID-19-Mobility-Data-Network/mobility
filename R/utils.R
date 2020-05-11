utils::globalVariables(
  c("date_start" ,"date_stop"  ,"date_span"  ,"indiv_id"   ,"indiv_age" ,
    "indiv_sex"  ,"indiv_type" ,"orig_adm0"  ,"orig_adm1"  ,"orig_adm2" ,
    "orig_adm3"  ,"orig_adm4"  ,"orig_adm5"  ,"orig_type"  ,"orig_x"    ,
    "orig_y"     ,"orig_pop"   ,"dest_adm0"  ,"dest_adm1"  ,"dest_adm2" ,
    "dest_adm3"  ,"dest_adm4"  ,"dest_adm5"  ,"dest_type"  ,"dest_x"    ,
    "dest_y"     ,"dest_pop"   ,"trips", "orig_id", "dest_id")
)

utils::globalVariables(c("pop", "x", "y", "id", "i", "j", "k", "travel", "n_distinct"))

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
##' @importFrom magrittr %>%
##'
##' @export
##'

get_unique_coords <- function(data,
                              orig=TRUE,
                              dest=TRUE
) {

  if (!all(c('orig_id', 'dest_id') %in% colnames(data))) {

    data <- cbind(data, get_unique_ids(data))
    message('Added missing unique location names.')
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
##' @importFrom magrittr %>%
##'
##' @export
##'

get_pop_vec <- function(data,
                        orig=TRUE,
                        dest=TRUE
) {

  if (!all(c('orig_id', 'dest_id') %in% colnames(data))) {

    data <- cbind(data, get_unique_ids(data))
    message('Added missing unique location names.')
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

  suppressMessages(
    out <- spatstat::pairdist(
      spatstat::as.ppp(xy,
                       spatstat::bounding.box.xy(xy),
                       check=FALSE)
    )
  )

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



##' Calculate summary statistics for a mobility model
##'
##' This is a wrapper function of \code{\link[MCMCvis:MCMCsummary]{MCMCsummary}} that calculates summary statistics for each
##' parameter in a \code{\link[coda:mcmc.list]{mcmc.list}} object. Summary statistics are calculated for all parameters across
##' each chain along with convergance diagnosics like the Gelman-Rubin convergence diagnostic and (Rhat) and samples
##' auto-correlation foreach parameter. If the model object contains deviance and penalty parameters, then Deviance Information
##' Criterion (DIC) is calculated and appended to the summary.
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
##' @family model
##'
##' @export
##'

summarize_mobility <- function(mod, ac_lags=c(2,5,10)) {

  if (!(class(mod) == 'mcmc.list')) stop('Model object must be mcmc.list')

  param_names <- dimnames(mod[[1]])[[2]]
  param_DIC <- c('DIC', 'deviance', 'pD')

  out <- tryCatch({

    tmp <- MCMCvis::MCMCsummary(mod,
                                HPD=TRUE,
                                func=function(x, lags=ac_lags) {
                                  round(acf(x, lag.max=lags[length(lags)], plot=FALSE)$acf[lags], 2)
                                },
                                func_name=stringr::str_c('AC', ac_lags))

    names(tmp)[c(1:4,6)] <- c('Mean', 'SD', 'HPD2.5', 'HPD97.5', 'SSeff')

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
                                HPD=TRUE,
                                func=function(x, lags=ac_lags) {
                                  round(acf(x, lag.max=lags[length(lags)], plot=FALSE)$acf[lags], 2)
                                },
                                func_name=stringr::str_c('AC', ac_lags))

    names(tmp)[c(1:4,6)] <- c('Mean', 'SD', 'HPD2.5', 'HPD97.5', 'SSeff')
    tmp

  })

  out
}



##' Check goodness of fit of a mobility model
##'
##' This function takes a fitted mobility model and calculates goodness of fit metrics. Model objects produced by the
##' \code{\link{fit_prob_travel}}, \code{\link{fit_gravity}}, \code{\link{fit_mobility}} or \code{\link{summarize_mobility}}
##' functions are accepted. If the Deviance Information Criterin (DIC) was calculated in the supplied model object, it is included in output.
##' When \code{plot_check = TRUE}, two plots are shown containing the posterior distribution of trip counts compared to observed data
##' and a Normal Q-Q plot showing the quantiles of model residuals against those
##' expected from a Normal distribution. Goodness of fit metrics include:
##' \describe{
##'   \item{DIC}{\href{https://en.wikipedia.org/wiki/Deviance_information_criterion}{Deviance Information Criterion}}
##'   \item{RMSE}{\href{https://en.wikipedia.org/wiki/Root-mean-square_deviation}{Root Mean Squared Error}}
##'   \item{MAPE}{\href{https://en.wikipedia.org/wiki/Mean_absolute_percentage_error}{Mean Absolute Percent Error}}
##'   \item{R2}{\href{https://en.wikipedia.org/wiki/Coefficient_of_determination}{R-squared}}
##' }
##'
##' @param M named matrix of trip counts among all \eqn{ij} location pairs
##' @param D named matrix of distances among all \eqn{ij} location pairs
##' @param N named vector of population sizes for all locations
##' @param mod model output from either the \code{fit_} functions or \code{\link{summarize_mobility}} function
##' @param plot_check logical indicating whether to plot the Posterior Predictive Check and Normal Q-Q Plot (default = \code{TRUE})
##'
##' @return a list of goodness of fit measures
##'
##' @author John Giles
##'
##' @example R/examples/check_mobility.R
##'
##' @family model
##'
##' @export
##'

check_mobility <- function(M,
                           D=NULL,
                           N=NULL,
                           mod,
                           plot_check=TRUE
) {

  if (coda::is.mcmc.list(mod)) mod <- summarize_mobility(mod)

  params_travel <- any(grep('tau', rownames(mod)))
  params_gravity <- all(c("gamma", "omega_1", "omega_2", "theta") %in% rownames(mod))

  if (params_travel & !params_gravity) {

    return(check_prob_travel(M=M, mod=mod, plot_check=plot_check))

  } else if ( !params_travel & params_gravity ) {

    return(check_gravity(M=M, D=D, N=N, mod=mod, plot_check=plot_check))

  } else if ( params_travel & params_gravity ) {

    if (!(identical(dim(M)[1], dim(D)[1], length(N)))) stop('Dimensions of input data must match')

    M_hat <- sim_mobility(N=N,
                          D=D,
                          theta=mod['theta', 'Mean'],
                          omega_1=mod['omega_1', 'Mean'],
                          omega_2=mod['omega_2', 'Mean'],
                          gamma=mod['gamma', 'Mean'],
                          tau=mod[grep('tau', rownames(mod)), 'Mean'],
                          counts=TRUE)

    err_rsq <- err_perc <- err_rmse <- rep(NA, nrow(M))
    for(i in 1:nrow(M)) {
      sel <- which(!is.na(M[i,]))
      err_perc[i] <- Metrics::mape(M[i, sel] + 1e-03, M_hat[i,sel])
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

      legend("topright",
             legend=c("Model", "Data"),
             col=c("black", "red"),
             lty=1,
             lwd=1.25,
             cex=0.8,
             seg.len=0.8,
             bty='n')

      err <- M - M_hat
      qqnorm(err, cex=1.25)
      qqline(err, lwd=2, col=2)
    }

    if ('DIC' %in% rownames(mod)) {

      return(
        list(DIC=mod['DIC', 'Mean'],
             RMSE=mean(err_rmse, na.rm=TRUE),
             MAPE=mean(err_perc, na.rm=TRUE),
             R2=mean(err_rsq, na.rm=TRUE))
      )

    } else {

      return(
        list(RMSE=mean(err_rmse, na.rm=TRUE),
             MAPE=mean(err_perc, na.rm=TRUE),
             R2=mean(err_rsq, na.rm=TRUE))
      )
    }
  }
}

# Model checking function specific to gravity model output
check_gravity <- function(M,
                          D,
                          N,
                          mod,
                          plot_check=TRUE
) {

  if (!(identical(dim(M)[1], dim(D)[1], length(N)))) stop('Dimensions of input data must match')
  if (coda::is.mcmc.list(mod)) mod <- summarize_mobility(mod)

  M_hat <- sim_gravity(N=N,
                       D=D,
                       theta=mod['theta', 'Mean'],
                       omega_1=mod['omega_1', 'Mean'],
                       omega_2=mod['omega_2', 'Mean'],
                       gamma=mod['gamma', 'Mean'],
                       counts=TRUE)

  diag(M_hat) <- NA
  diag(M) <- NA

  err_rsq <- err_perc <- err_rmse <- rep(NA, nrow(M))
  for(i in 1:nrow(M)) {
    sel <- which(!is.na(M[i,]))
    err_perc[i] <- Metrics::mape(M[i, sel] + 1e-03, M_hat[i,sel])
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

    legend("topright",
           legend=c("Model", "Data"),
           col=c("black", "red"),
           lty=1,
           lwd=1.25,
           cex=0.8,
           seg.len=0.8,
           bty='n')

    err <- M - M_hat
    qqnorm(err, cex=1.25)
    qqline(err, lwd=2, col=2)
  }

  if ('DIC' %in% rownames(mod)) {

    return(
      list(DIC=mod['DIC', 'Mean'],
           RMSE=mean(err_rmse, na.rm=TRUE),
           MAPE=mean(err_perc, na.rm=TRUE),
           R2=mean(err_rsq, na.rm=TRUE))
    )

  } else {

    list(RMSE=mean(err_rmse, na.rm=TRUE),
         MAPE=mean(err_perc, na.rm=TRUE),
         R2=mean(err_rsq, na.rm=TRUE))
  }
}

# Model checking function specific to travel probability model output
check_prob_travel <- function(M,
                              mod,
                              plot_check=TRUE
) {

  taus <- grep('tau', rownames(mod))

  if ( !(identical(dim(M)[1], length(taus))) ) stop('Dimensions of input data must match')
  if (coda::is.mcmc.list(mod)) mod <- summarize_mobility(mod)

  tau_hat <- mod$Mean[taus]
  tau <- 1 - (diag(M) / rowSums(M, na.rm=TRUE))

  if (plot_check) {

    sel <- which(!is.na(tau))
    tau <- tau[sel]
    tau_hat <- tau_hat[sel]

    par(mfrow=c(1,2))
    dens_tau <- density(tau)
    dens_tau_hat <- density(tau_hat)
    plot(dens_tau, lwd=2, col='red',
         xlab='Probability of travel outside origin',
         main='Posterior predictive check',
         xlim=c(0,1),
         ylim=c(0, max(c(dens_tau$y, dens_tau_hat$y))))
    lines(dens_tau_hat, lwd=2)

    legend("topright",
           legend=c("Model", "Data"),
           col=c("black", "red"),
           lty=1,
           lwd=1.25,
           cex=0.8,
           seg.len=0.8,
           bty='n')

    err <- tau - tau_hat
    qqnorm(err, cex=1.25)
    qqline(err, lwd=2, col=2)
  }

  if ('DIC' %in% rownames(mod)) {

    return(
      list(DIC=mod['DIC', 'Mean'],
           MAPE=Metrics::mape(tau + 1e-05, tau_hat),
           R2=cor(tau, tau_hat)^2)
    )

  } else {

    list(MAPE=Metrics::mape(tau, tau_hat),
         R2=cor(tau, tau_hat)^2)
  }
}



##' Simulate connectivity values using gravity model
##'
##' This function uses the gravity model formula to simulate a connectivity matrix based on the supplied model parameters. The
##' gravity model formula uses a Gamma distribution as the dispersal kernel in the denominator. A null model (where all model parameters = 1) can be
##' simulated by supplying only population sizes (\code{N}) and pairwise distances (\code{D}).
##'
##' @param D matrix giving distances among the origins and destinations
##' @param N vector of population sizes
##' @param theta scalar giving the proportionality constant of gravity formula (default = 1)
##' @param omega_1 scalar giving exponential scaling of origin population size (default = 1)
##' @param omega_2 scalar giving exponential scaling of destination population size (default = 1)
##' @param gamma scalar giving the dispersal kernel paramater (default = 1)
##' @param n number of simulations (requires argument \code{mod} to be supplied with a gravity model object)
##' @param mod a gravity model object produced by the \code{\link{fit_gravity}} or \code{\link{summarize_mobility}} functions
##' @param counts logical indicating whether or not to return a count variable by scaling the connectivity matrix by origin population size (\eqn{N_i}) (default = FALSE)
##'
##' @return a matrix with values between 0 and 1 (if \code{counts = FALSE}) or positive integers (if \code{counts = TRUE}). If \code{n > 1} then returns and array with 3 dimensions
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
  D,
  N,
  theta=1,
  omega_1=1,
  omega_2=1,
  gamma=1,
  n=1,
  mod=NULL,
  counts=FALSE
) {

  if (!is.null(mod)) {

    if(coda::is.mcmc.list(mod)) mod <- summarize_mobility(mod)

    params <- apply(mod, 1, function(x){

      sim_param(n=n,
                mean=x['Mean'],
                sd=x['SD'],
                CI_low=x['HPD2.5'],
                CI_high=x['HPD97.5'])
    })

    if (!is.matrix(params)) params <- t(as.matrix(params))

    out <- foreach::foreach(i=1:nrow(params),
                            .combine=function(a, b) abind::abind(a, b, along=3)) %do% {

                              sim_gravity_pt_est(D=D,
                                                 N=N,
                                                 theta=params[i,'theta'],
                                                 omega_1=params[i,'omega_1'],
                                                 omega_2=params[i, 'omega_2'],
                                                 gamma=params[i, 'gamma'],
                                                 counts=counts)

                            }

    return(out)

  } else if (is.null(mod)) {

    if (n > 1) stop('Supply gravity model object for multiple simulations')

    return(
      sim_gravity_pt_est(D=D,
                         N=N,
                         theta=theta,
                         omega_1=omega_1,
                         omega_2=omega_2,
                         gamma=gamma,
                         counts=counts)
    )
  }
}

# Simulate the point estimate of gravity model values based only on mean parameter values

sim_gravity_pt_est <- function(
  D,
  N,
  theta,
  omega_1,
  omega_2,
  gamma,
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


##' Simulate parameter values
##'
##' This function takes the summary statistics of the estimated posterior distribution of a parameter
##' and simulates random values from a normal distribution with its mean and standard deviation. If CI_low and
##' CI_high are supplied, simulated values are truncated by the confidence bounds.
##'
##' @param n number of simulations
##' @param mean  mean of parameter posterior distribution
##' @param sd standard of parameter posterior distribution
##' @param CI_low lower confidence bound of estimated posterior (default = NULL)
##' @param CI_high upper confidence bound of estimated posterior (default = NULL)
##'
##' @return a vector of length n
##'
##' @author John Giles
##'
##' @example R/examples/sim_param.R
##'
##' @family simulation
##'
##' @export
##'

sim_param <- function(n,
                      mean,
                      sd,
                      CI_low=NULL,
                      CI_high=NULL
) {

  if (!is.null(c(CI_low, CI_high))) {

    return(truncnorm::rtruncnorm(n, a=CI_low, b=CI_high, mean=mean, sd=sd))

  } else {

    return(rnorm(n, mean, sd))
  }
}




##' Get parameters of Beta distribution
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
##' @family utility
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



##' Get parameters of Gamma distribution
##'
##' A function that finds the \code{shape} and \code{rate} parameters required by the Gamma distribution given the observed
##' mean \code{mu} and standard deviation \code{sigma} of the response variable. Parameters are found numerically using a
##' two-dimensional Nelder-Mead optimization algorithm.
##'
##' @param mu the desired mean of the Gamma distribution
##' @param sigma the desired standard deviation of the Gamma distribution
##'
##' @return a named numeric vector giving the \code{shape} and \code{rate} parameters of the Gamma distribution
##'
##' @author John Giles
##'
##' @example R/examples/get_gamma_params.R
##'
##' @family utility
##'
##' @export
##'

get_gamma_params <- function(mu, sigma) {

  suppressWarnings(
    params <- optim(par=c(mu*2, 2),
                    fn=function(x) abs(mu - x[1]/x[2]) + abs(sigma - sqrt(x[1]/(x[2]^2))),
                    method='Nelder-Mead')$par
  )

  names(params) <- c('shape', 'rate')
  return(params)
}



##' Make data set of travel and stays
##'
##' This function builds a data set containing the number of individuals that remain in their home location (stay) or travel to another location
##' during the time span of the survey. The output is designed to provide data for the \code{\link{fit_prob_travel}} function. The admin unit used to aggregate the travel/stay
##' counts depend on the arguemtns supplied:
##' \enumerate{
##' \item When \code{data_pred} is supplied and \code{agg_adm = NULL}, the lowest admin unit of \code{data_pred} is used
##' \item When both \code{data_pred} and \code{agg_adm} are \code{NULL}, the lowest admin unit of \code{data} is used
##' }
##'
##' @param data generalized data frame described in \code{\link{travel_data_sim}} or derivative thereof
##' @param data_pred generalized data frame containing the admin units at which to predict probability of travel
##' @param agg_adm optional argument (logical) giving an arbitarary admin unit over which to aggregate travel/stay counts
##'
##' @return dataframe with same columns as \code{\link{travel_data_sim}} data with columns for counts of travel/stay/total
##'
##' @author John Giles
##'
##' @example R/examples/get_stay_data.R
##'
##' @family data synthesis
##' @family travel probability
##'
##' @importFrom magrittr %>%
##'
##' @export
##'

get_stay_data <- function(data,
                          data_pred=NULL,
                          agg_adm=NULL
) {

  ##suppressMessages(
  #  require(dplyr, quietly=TRUE)
  #)

  if (all(is.null(data_pred), is.null(agg_adm))) agg_adm <- get_admin_level(data)
  if (!is.null(data_pred) & is.null(agg_adm)) agg_adm <- get_admin_level(data_pred)

  ids <- c('orig_id', 'dest_id')

  if (any(ids %in% colnames(data))) {

    data[,ids] <- get_unique_ids(data, adm_stop=agg_adm)

  } else {

    data <- cbind(data, get_unique_ids(data, adm_stop=agg_adm))
    message('Added missing unique location names.')
  }

  sel <- data$orig_id == data$dest_id
  data_trip <- data[!sel,]
  data_stay <- data[sel,]

  if (!is.null(data_pred)) {

    if (any(ids %in% colnames(data_pred))) {

      data_pred[,ids] <- get_unique_ids(data_pred, adm_stop=agg_adm)

    } else {

      data_pred <- cbind(data_pred, get_unique_ids(data_pred, adm_stop=agg_adm))
    }
  }

  trip_count_method <- all(is.na(data$indiv_id)) | !('indiv_id' %in% colnames(data))

  if (trip_count_method) {

    message('Using total trip count method')
    out <- merge(
      data_stay %>%
        dplyr::group_by(orig_id) %>%
        dplyr::mutate(stay=sum(trips, na.rm=T)) %>%
        dplyr::distinct(orig_id, .keep_all=T) %>%
        dplyr::select(-trips) %>%
        data.frame(),
      data_trip %>%
        dplyr::group_by(orig_id) %>%
        dplyr::mutate(travel=sum(trips, na.rm=T)) %>%
        dplyr::distinct(orig_id, .keep_all=T) %>%
        dplyr::select(orig_id, travel) %>%
        data.frame(),
      by='orig_id',
      all.y=T
    )

  } else if (!trip_count_method) {

    message('Using individual count method')
    out <- merge(
      data_stay %>%
        dplyr::group_by(orig_id) %>%
        dplyr::mutate(stay=n_distinct(indiv_id, na.rm=T)) %>%
        dplyr::distinct(orig_id, .keep_all=T) %>%
        data.frame(),
      data_trip %>%
        dplyr::group_by(orig_id) %>%
        dplyr::mutate(travel=n_distinct(indiv_id, na.rm=T)) %>%
        dplyr::mutate(travel=ifelse(travel == 0, NA, travel)) %>%
        dplyr::distinct(orig_id, .keep_all=T) %>%
        dplyr::select(orig_id, travel) %>%
        data.frame(),
      by='orig_id',
      all.y=T
    )

    out$indiv_id <- out$indiv_age <- out$indiv_sex <- NA
  }

  out$total <- out$stay + out$travel

  # If prediction data supplied, aggregate to level of prediction and merge with prediction locations
  if (!is.null(data_pred)) {

    # check all stays in prediction data
    if (all(out$orig_id %in% data_pred$orig_id)) warning('Not all locations in stay data are present in prediction data')

    sel_row <- !(data_pred$orig_id %in% out$orig_id) # only admins not in V already
    sel_col <- which(apply(data_pred, 2, function(x) !all(is.na(x)))) # only cols with data
    out <- dplyr::full_join(out, data_pred[sel_row, sel_col])
    out <- out[,c(colnames(data_trip), c('stay', 'travel', 'total'))]
  }

  out
}

##' Find the lowest admin unit
##'
##' This function checks which admin levels are in a generalized a data frame formatting like \code{\link{travel_data_sim}} and
##' returns the lowest admin unit.
##'
##' @param data generalized data frame described in \code{\link{travel_data_template}} or derivative thereof
##'
##' @return integer
##'
##' @author John Giles
##'
##' @example R/examples/get_admin_level.R
##'
##' @family utility
##'
##' @export
##'

get_admin_level <- function(data) {

  admins <- grep('orig_adm', colnames(data))

  keep <- apply(
    data[,admins],
    2,
    function(x) all(!is.na(x))
  )

  admins <- admins[keep]
  admins <- sort(colnames(data)[admins])
  agg_adm <- admins[length(admins)]
  as.integer(substr(agg_adm, nchar(agg_adm), nchar(agg_adm)))
}



##' Simulate probability of leaving origin
##'
##' This function simulates one stochastic realization of the proportion of individuals that leave origin \eqn{i}.
##' The function takes posterior estimates of the mean (\code{mu}) and standard deviation (\code{sigma})
##' for each origin \eqn{i}, and then derives the shape and rate parameters for the Beta distribution. Simulated values are random
##' draws from this Beta distribution.
##'
##' @param mu scalar or vector giving mean probability of leaving origin (defualt = 0.5)
##' @param sigma scalar or vector giving standard deviation of the probability of leaving origin (default = 0.1)
##' @param n number of simulations (default = 1)
##' @param id optional scalar or vector giving name(s) of origin (default = \code{NULL})
##'
##' @return a numeric scalar or vector with values between 0 and 1
##'
##' @author John Giles
##'
##' @example R/examples/sim_prob_travel.R
##'
##' @family simulation
##' @family travel probability
##'
##' @export
##'

sim_prob_travel <- function(mu=0.5,
                            sigma=0.1,
                            n=1,
                            id=NULL
){

  if (any(c(mu > 1, mu < 0, sigma > 1, sigma < 0))) stop('mu and sigma must be between 0 and 1')
  if (!(length(mu) == length(sigma))) stop('mu and sigma must have same length')

  bp <- do.call(rbind, get_beta_params(mu, sigma^2))
  if (!is.null(id)) colnames(bp) <- id

  out <- apply(bp, 2, function(x) {
    suppressWarnings(rbeta(n, x['shape1'], x['shape2']))
  })

  if (n == 1) {
    return(out)
  } else {
    return(t(out))
  }
}






##' Simulate a full mobility model
##'
##' This function simulates a mobility matrix based on the supplied model parameters. The full mobility model
##' estimates both quantity of travel within a location (along the matrix diagonal) as well as the quantity
##' among all origins and destinations (off-diagonals). For multiple stochastic simulations, the \code{mod}
##' argument must be supplied with a mobility model object produced by the \code{\link{fit_mobility}} function.
##' If mean values are supplied to the named parameter arguments (\code{theta}, \code{omega_1}, \code{omega_2}, \code{gamma}, \code{tau}),
##' a single point estimate of the mobility matrix will be returned. A null model (where all model parameters = 1) can be simulated by
##' supplying only population sizes (\code{N}) and pairwise distances (\code{D}).
##'
##' @param D matrix giving distances among the origins and destinations
##' @param N vector of population sizes
##' @param theta scalar giving the proportionality constant of gravity formula (default = 1)
##' @param omega_1 scalar giving exponential scaling of origin population size (default = 1)
##' @param omega_2 scalar giving exponential scaling of destination population size (default = 1)
##' @param gamma scalar giving the dispersal kernel paramater (default = 1)
##' @param tau scalar or vector giving the probability of travel outside origin
##' @param n number of simulations (requires argument \code{mod} to be supplied with a gravity model object)
##' @param mod a mobility model object produced by the \code{\link{fit_mobility}} or \code{\link{summarize_mobility}} functions
##' @param counts logical indicating whether or not to return a count variable by scaling the mobility matrix by origin population size (\eqn{N_i}) (default = FALSE)
##'
##' @return a matrix with values between 0 and 1 (if \code{counts = FALSE}) or positive integers (if \code{counts = TRUE}). If \code{n > 1} then returns and array with 3 dimensions
##'
##' @author John Giles
##'
##' @example R/examples/sim_mobility.R
##'
##' @family simulation
##'
##' @export
##'

sim_mobility <- function(
  D,
  N,
  theta = 1,
  omega_1 = 1,
  omega_2 = 1,
  gamma = 1,
  tau=0.5,
  n = 1,
  mod = NULL,
  counts = FALSE
){

  if (!(dim(D)[1] == length(N))) stop('Dimensions of D and N mismatch')

  if (!is.null(mod)) {

    if(coda::is.mcmc.list(mod)) mod <- summarize_mobility(mod)

    mod_gravity <- mod[which(rownames(mod) %in% c("gamma", "omega_1", "omega_2", "theta")),]
    mod_travel <- mod[grep('tau', rownames(mod)),]

    pi <- sim_gravity(D=D,
                      N=N,
                      mod=mod_gravity,
                      n=n)

    tau <- sim_prob_travel(mu=mod_travel$Mean,
                           sigma=mod_travel$SD,
                           id=names(N),
                           n=n)

    return(
      foreach::foreach(k=1:n, .combine=function(a, b) abind::abind(a, b, along=3)) %do% {

        calc_abs_probs(pi[,,k], tau[,k], N, counts=counts)
      }
    )

  } else if (is.null(mod)) {

    if (n > 1) warning('Can only return one simulation from point estimates of parameters')
    if ( length(tau) > 1 & length(tau) != length(N) ) stop('tau must be scalar or have dimensions to mathc D and N')
    if (!(dim(D)[1] == length(tau))) tau <- rep(tau, dim(D)[1])

    pi <- sim_gravity(D=D,
                      N=N,
                      theta=theta,
                      omega_1=omega_1,
                      omega_2=omega_2,
                      gamma=gamma,
                      n=1)

    return(calc_abs_probs(pi, tau, N, counts=counts))
  }

}


# Calculate the absolute probability of travel

# This function calculates the abcolute probability of a trip going from origin $i$ to destination $j$ given
# relative probabilities in matrix pi (from gravity model) and probability of travel outside origin (tau).

calc_abs_probs <- function(pi,
                           tau,
                           N=NULL,
                           counts=FALSE) {

  if (!(dim(pi)[1] == length(tau))) stop('Dimensions of pi and tau mismatch')

  out <- pi
  for(i in 1:nrow(out)) {
    for(j in 1:ncol(out)) {

      out[i,j] <- ifelse(
        i == j,
        (1 - tau[i]),
        tau[i] * pi[i,j]
      )
    }
  }

  if (counts) for (i in 1:nrow(out)) out[i,] <- round(N[i]*out[i,])
  out
}

