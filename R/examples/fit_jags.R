N <- 100
lambda <- 10
y <- rpois(n=N, lambda=lambda)

jags.data <- list(N=N, y=y)

jags.model <- "
model {

  for(i in 1:N){

    y[i] ~ dpois(lambda)

  }

  lambda ~ dgamma(1, 0.01)

}"

params <- c('lambda')

mod <- fit.jags(jags.data=jags.data,
                jags.model=jags.model,
                params=params)

str(mod)
