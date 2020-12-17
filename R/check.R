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

    M <- c(object$data$M)

  }  else {

    stop("Check slot 'data' of mobility.model object")

  }

  if (object$model == 'radiation') {

    M_hat <- object$params

  } else {

    if (is.null(object$summary)) object$summary <- summary.mobility.model(object$params)
    M_hat <- c(predict(object))

  }

  if (plots) {

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

  sel <- which(!is.na(M))

  SS_resid <- sum((M[sel] - M_hat[sel])^2)
  SS_pred <- sum((M_hat[sel] - mean(M[sel]))^2)
  R2 <- SS_pred/(SS_resid + SS_pred)

  return(
    list(DIC=object$summary['DIC', 'mean'],
         RMSE=Metrics::rmse(M[sel], M_hat[sel]),
         MAPE=Metrics::mape(M[sel] + 1e-03, M_hat[sel]),
         R2=R2)
  )
}


