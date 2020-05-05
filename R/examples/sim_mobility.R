M <- mobility_matrices$M
D <- mobility_matrices$D
N <- mobility_matrices$N

# Simulate null model for given D and N
# assumes 0.5 probability of traveling outside origin and null gravity
# for probability of travel to all destinations
sim_mobility(D, N)

# Simulate point estimates for some mean parameter values
sim_mobility(D, N,
             theta = 3,
             omega_1 = 2,
             omega_2 = 0.7,
             gamma = 1.5,
             tau=rbeta(length(N), 2, 2))

# Simulate multiple stochastic realizations of a mobility matrix from
# fitted mobility model parameters
mod <- summarize_mobility(
  fit_mobility(M, D, N)
)

mod_sim <- sim_mobility(D,
             N,
             n=3,
             mod=mod,
             counts=TRUE)
