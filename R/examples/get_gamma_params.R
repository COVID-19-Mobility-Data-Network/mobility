mu <- 2
sigma <- 0.5

prm <- get_gamma_params(mu, sigma)

prm[1]/prm[2] # mean
sqrt(prm[1]/(prm[2]^2)) # sd

curve(dgamma(x, prm[1], prm[2]), 0, 5,
      xlab='Response variable',
      ylab='Density')
abline(v=mu, lty=2, col='red')
