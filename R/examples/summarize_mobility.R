# Gravity model
M <- mobility_matrices$M
D <- mobility_matrices$D
N <- mobility_matrices$N

mod <- fit_gravity(M, D, N)
summarize_mobility(mod)

mod <- fit_gravity(M, D, N, DIC=T)
summarize_mobility(mod)

# Probability of travel
n_orig <- 6
orig_id <- LETTERS[1:n_orig]

N <- rpois(n_orig, 100)    # population size of each origin
tau <- rbeta(n_orig, 2, 2)   # probability of leaving origin

Y <- setNames(rbinom(n_orig, N, tau), orig_id)
N <- setNames(N, orig_id)

prob_trav <- fit_prob_travel(travel=Y,
                             total=N,
                             format_locations=FALSE)

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
