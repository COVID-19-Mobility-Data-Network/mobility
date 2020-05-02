N <- 100
lambda <- 10
y <- rpois(n=N, lambda=lambda)

jags_data <- list(N=N, y=y)

jags_model <- "
model {

  for(i in 1:N){

    y[i] ~ dpois(lambda)

  }

  lambda ~ dgamma(1, 0.01)

}"

params <- c('lambda')

mod <- fit_jags(jags_data=jags_data,
                jags_model=jags_model,
                params=params)
