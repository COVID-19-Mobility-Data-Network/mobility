M <- mobility_matrices$M
D <- mobility_matrices$D
N <- mobility_matrices$N

mod <- fit_gravity(M, D, N)

mod <- fit_gravity(M, D, N, DIC=TRUE)

mod <- fit_gravity(M, D, N,
                   n_chain=4,
                   n_burn=2000,
                   n_samp=2000,
                   n_thin=2,
                   parallel=TRUE)
