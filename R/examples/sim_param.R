M <- mobility_matrices$M
D <- mobility_matrices$D
N <- mobility_matrices$N

mod <- summarize_mobility(
  fit_gravity(M, D, N)
)

param <- 'gamma'
sim_param(n=10,
          mean=mod[param,'Mean'],
          sd=mod[param,'SD'],
          CI_low=mod[param,'HPD2.5'],
          CI_high=mod[param,'HPD97.5'])
