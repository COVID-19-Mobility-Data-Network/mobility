% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{compare}
\alias{compare}
\title{Compare the goodness of fit of multiple mobility models}
\usage{
compare(object)
}
\arguments{
\item{object}{a list of \code{mobility.model} objects}
}
\value{
a dataframe where the rows contain basic model information along with goodness of fit metrics for each model
}
\description{
Given two or more \code{mobility.model} objects, this function will parse out model information and goodness of fit metrics produced by the
\code{\link{check}} function and return them side by side for comparison.
}
\examples{
mods <- list(
  mobility(data=mobility_matrices, model='gravity', type='power', DIC=TRUE),
  mobility(data=mobility_matrices, model='radiation', type='basic'),
  mobility(data=mobility_matrices, model='departure-diffusion', type='power', DIC=TRUE)
)

compare(mods)
}
\seealso{
Other model: 
\code{\link{check}()},
\code{\link{fit_jags}()},
\code{\link{fit_prob_travel}()},
\code{\link{mobility}()},
\code{\link{predict}()},
\code{\link{residuals}()},
\code{\link{summary}()}
}
\author{
John Giles
}
\concept{model}
