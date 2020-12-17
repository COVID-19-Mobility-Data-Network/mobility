
##' Calculate summary statistics for 'mobility.model' class
##'
##' This is a wrapper function of \code{\link[MCMCvis:MCMCsummary]{MCMCsummary}} that calculates summary statistics for each
##' parameter in a \code{mobility.model} object. Summary statistics are calculated for all parameters across
##' each chain along with convergance diagnosics like the Gelman-Rubin convergence diagnostic and (Rhat) and samples
##' auto-correlation foreach parameter. If the model object contains deviance and penalty parameters, then Deviance Information
##' Criterion (DIC) is calculated and appended to the summary.
##'
##' @param object a \code{mobility.model} object (can also accept a \code{\link[coda:mcmc.list]{mcmc.list}} object)
##' @param probs numeric vector giving the quantiles to calculate for each parameter (default = \code{c(0.025, 0.5, 0.975)})
##' @param ac_lags numeric vector of lags over which to calculate autocorrelation of samples within chains (default = \code{c(2,5,10)})
##' @param ... further arguments passed to or from other methods
##'
##' @return a dataframe with summary statistics
##'
##' @author John Giles
##'
##' @example R/examples/summary.R
##'
##' @family model
##'
##' @export
##'

summary <- function(object, probs, ac_lags, ...) UseMethod('summary')

##' @export

summary.mobility.model <- function(object,
                                   probs=c(0.025, 0.25, 0.75, 0.975),
                                   ac_lags=c(5, 10),
                                   ...) {

  if (all(
    !is.null(object$summary),
    identical(probs, c(0.025, 0.25, 0.75, 0.975)),
    identical(ac_lags, c(5,10)))
  ) {

    return(object$summary)

  } else {

    if (!(class(object) %in% c('mobility.model', 'mcmc.list'))) stop('Model object must be of class mobility.model or mcmc.list')
    if (class(object) == 'mobility.model') object <- object$params

    param_names <- dimnames(object[[1]])[[2]]
    param_DIC <- c('DIC', 'deviance', 'pD')

    out <- tryCatch({

      tmp <- MCMCvis::MCMCsummary(object,
                                  probs=probs,
                                  func=function(x, lags=ac_lags) {
                                    round(acf(x, lag.max=lags[length(lags)], plot=FALSE)$acf[lags], 2)
                                  },
                                  func_name=stringr::str_c('AC', ac_lags))


      if (all(param_DIC %in% param_names)) {

        tmp['pD', !(colnames(tmp) == 'mean')] <- NA
        tmp <- rbind(tmp[!(rownames(tmp) %in% param_DIC),], tmp[param_DIC,])
      }

      tmp

    }, error = function(e) {

      message('ERROR: cannot calculate DIC for this model')

      object <- coda::as.mcmc.list(
        lapply(object$params, function(x) coda::as.mcmc(x[,!(colnames(x) %in% param_DIC)]))
      )

      tmp <- MCMCvis::MCMCsummary(object,
                                  probs=probs,
                                  func=function(x, lags=ac_lags) {
                                    round(acf(x, lag.max=lags[length(lags)], plot=FALSE)$acf[lags], 2)
                                  },
                                  func_name=stringr::str_c('AC', ac_lags))
      tmp

    })

    sel <- 3:(3+length(probs)-1)
    names(out)[sel] <- stringr::str_c('Q', gsub('*%', '', names(out)[sel]))

    return(out)
  }
}
