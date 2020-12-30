##' Check goodness of fit for 'mobility.model' class
##'
##' This function takes a fitted mobility \code{mobility.model} object and calculates goodness of fit metrics. If the Deviance
##' Information Criterin (DIC) was calculated in the supplied model object, it is included in output.
##' When \code{plots = TRUE}, two plots are shown containing the posterior distribution of trip counts compared to observed data
##' and a Normal Q-Q plot showing the quantiles of model residuals against those
##' expected from a Normal distribution.
##'
##' @param object a \code{mobility.model} object produced by the \code{\link{mobility}} function
##' @param plots logical indicating whether to plot the Posterior Predictive Check and Normal Q-Q Plot (default = \code{TRUE})
##' @param ... further arguments passed to or from other methods
##'
##' @return a list of goodness of fit measures
##'
##' @details Goodness of fit metrics include:
##' \describe{
##'   \item{DIC}{\href{https://en.wikipedia.org/wiki/Deviance_information_criterion}{Deviance Information Criterion}}
##'   \item{RMSE}{\href{https://en.wikipedia.org/wiki/Root-mean-square_deviation}{Root Mean Squared Error}}
##'   \item{MAPE}{\href{https://en.wikipedia.org/wiki/Mean_absolute_percentage_error}{Mean Absolute Percent Error}}
##'   \item{R2}{\href{https://en.wikipedia.org/wiki/Coefficient_of_determination}{R-squared}}
##' }
##'
##' @author John Giles
##'
##' @example R/examples/check.R
##'
##' @family model
##'
##' @export

check <- function(object, plots, ...) UseMethod('check')

##' @export

check.mobility.model <- function(object,
                                 plots=TRUE,
                                 ...) {

  if (!(class(object) == 'mobility.model')) stop("Object must be class 'mobility.model'")

  if('M' %in% names(object$data) ) {

    M <- object$data$M

  }  else {

    stop("Check slot 'data' of mobility.model object")

  }

  if (object$model == 'radiation') {

    M_hat <- object$params

  } else {

    if (is.null(object$summary)) object$summary <- summary.mobility.model(object$params)
    M_hat <- predict(object)

  }

  M_res <- residuals(object, type='deviance')

  if (plots) {

    par(mfrow=c(2,4))

    # Posterior predictive check
    dens_M <- density(M[!is.na(M)])
    dens_M_hat <- density(M_hat[!is.na(M_hat)])
    plot(dens_M, lwd=2, col='red',
         xlab='Trip count',
         main='',
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

    # Residuals-Fitted
    plot(log(M_hat), M_res, xlab='Log(Fitted)', ylab='Deviance resiuduals')
    abline(h=0, lty=2, col='red')

    # Residuals distribution
    hist(M_res, main='', xlab='Deviance residuals', ylab='Frequency', col='lightblue')
    abline(v=0, lty=2, col='red')

    # Q-Q plot
    qqnorm(M_res, main='')
    qqline(M_res, col=2)

    # Log-log plot
    plot(log(M), log(M_hat), xlab='Log(Observed)', ylab='Log(Fitted)')
    abline(a=1, b=1, lty=2, col='red')

    # Residuals-covariates
    plot(object$data$D, M_res, xlab='Distance', ylab='Deviance resiuduals')
    abline(h=0, lty=2, col='red')

    N_orig <- object$data$N_orig
    for (i in 1:(ncol(M)-1)) N_orig <- rbind(N_orig, object$data$N_orig)
    plot(N_orig, M_res, xlab='Origin population size', ylab='Deviance resiuduals')
    abline(h=0, lty=2, col='red')

    N_dest <- object$data$N_dest
    for (i in 1:(nrow(M)-1)) N_dest <- rbind(N_dest, object$data$N_dest)
    plot(N_dest, M_res, xlab='Destination population size', ylab='Deviance resiuduals')
    abline(h=0, lty=2, col='red')

    mtext(paste("Goodness of fit:", object$type, object$model, 'model', paste0('(n=', sum(!is.na(M)), ')')),
          outer=TRUE,  cex=1.15, line=-2)

  }

  sel <- which(!is.na(M))
  M <- M[sel]
  M_hat <- M_hat[sel]
  M_res <- M_res[sel]

  SS_resid <- sum((M - M_hat)^2)
  SS_pred <- sum((M_hat - mean(M))^2)

  return(
    list(DIC=object$summary['DIC', 'mean'],
         RMSE=Metrics::rmse(M, M_hat),
         MAPE=Metrics::mape(M + 1e-03, M_hat),
         R2=SS_pred/(SS_resid + SS_pred))
  )
}


##' Extract model residuals
##'
##' Generic fundtion that extracts model residuals from a 'mobility.model' object.
##'
##' @param object a \code{mobility.model} object produced by the \code{\link{mobility}} function
##' @param type the type of residuals to be returned. Available residual types include: \code{'deviance'} (default), \code{'pearson'}, and \code{'raw'}.
##' @param ... further arguments passed to or from other methods
##'
##' @return a matrix containing model residuals
##'
##' @details Residual types are calculated as:
##' \describe{
##'   \item{raw}{\eqn{y_i - \mu_i}}
##'   \item{pearson}{\eqn{(y_i - \mu_i)/\sqrt{\mu_i}}}
##'   \item{deviance}{\eqn{sign(y_i - \mu_i) * \sqrt(2(log(y_i/\mu_i) - (y_i - \mu_i)))}}
##' }
##' Where, \eqn{y_i} is the observed data and \eqn{\mu_i} is the value predicted by the model using the mean of parameter posterior distributions
##'
##' @author John Giles
##'
##' @example R/examples/residuals.R
##'
##' @family model
##'
##' @export

residuals <- function(object, type, ...) UseMethod('residuals')

##' @export

residuals.mobility.model <- function(object,
                                     type='deviance',
                                     ...) {

  if (!(class(object) == 'mobility.model')) stop("Object must be class 'mobility.model'")

  if('M' %in% names(object$data) ) {

    M <- object$data$M

  }  else {

    stop("Check slot 'data' of mobility.model object")

  }

  if (object$model == 'radiation') {

    M_hat <- object$params

  } else {

    if (is.null(object$summary)) object$summary <- summary.mobility.model(object$params)
    M_hat <- predict(object)

  }

  if (type == 'raw') {

    res <- M - M_hat

  } else if (type == 'pearson') {

    res <- (M - M_hat)/sqrt(M_hat)

  } else if (type == 'deviance') {

    res <- sign(M-M_hat) * sqrt( 2 * (M * log(M/M_hat) - (M-M_hat)) )

  } else {

    stop('Unknown residual type')
  }

  attr(res, 'residuals') <- type
  return(res)
}
