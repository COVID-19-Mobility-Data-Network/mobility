# Gravity model
M <- mobility_matrices$M
D <- mobility_matrices$D
N <- mobility_matrices$N

mod <- fit_gravity(M, D, N)
summarize_mobility(mod)

mod <- fit_gravity(M, D, N, DIC=T)
summarize_mobility(mod)

# Probability of travel






# Other models
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

summarize_mobility(mod)


mod <- fit_jags(jags_data=jags_data,
                jags_model=jags_model,
                params=params,
                DIC=T)

summarize_mobility(mod)
