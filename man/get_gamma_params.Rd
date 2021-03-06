% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_gamma_params}
\alias{get_gamma_params}
\title{Get parameters of Gamma distribution}
\usage{
get_gamma_params(mu, sigma)
}
\arguments{
\item{mu}{the desired mean of the Gamma distribution}

\item{sigma}{the desired standard deviation of the Gamma distribution}
}
\value{
a named numeric vector giving the \code{shape} and \code{rate} parameters of the Gamma distribution
}
\description{
A function that finds the \code{shape} and \code{rate} parameters required by the Gamma distribution given the observed
mean \code{mu} and standard deviation \code{sigma} of the response variable. Parameters are found numerically using a
two-dimensional Nelder-Mead optimization algorithm.
}
\examples{
mu <- 2
sigma <- 0.5

prm <- get_gamma_params(mu, sigma)

prm[1]/prm[2] # mean
sqrt(prm[1]/(prm[2]^2)) # sd

curve(dgamma(x, prm[1], prm[2]), 0, 5, xlab='Response variable', ylab='Density')
abline(v=mu, lty=2, col='red')
}
\seealso{
Other utility: 
\code{\link{get_admin_level}()},
\code{\link{get_beta_params}()},
\code{\link{get_pop_vec}()},
\code{\link{get_unique_coords}()},
\code{\link{get_unique_ids}()}
}
\author{
John Giles
}
\concept{utility}
