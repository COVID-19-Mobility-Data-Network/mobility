% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_beta_params}
\alias{get_beta_params}
\title{Get parameters of Beta distribution}
\usage{
get_beta_params(mu, sigma)
}
\arguments{
\item{mu}{scalar or vector giving the mean of the proportion}

\item{sigma}{scalar or vector giving the standard deviation of the proportion}
}
\value{
A list containing the two shape parameters of the Beta distribution
}
\description{
This function finds the two shape parameters for the Beta distribution of a random variable between 0 and 1.
}
\examples{
get_beta_params(0.6, 0.03)

n <- 5
get_beta_params(mu=runif(n, 0, 1), sigma=runif(n, 0, 0.05))
}
\seealso{
Other utility: 
\code{\link{get_admin_level}()},
\code{\link{get_gamma_params}()},
\code{\link{get_pop_vec}()},
\code{\link{get_unique_coords}()},
\code{\link{get_unique_ids}()}
}
\author{
John Giles
}
\concept{utility}
