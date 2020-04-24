M <- mobility_matrices$M
D <- mobility_matrices$D
N <- mobility_matrices$N

mod <- fit_gravity(M, D, N)
summarize_mobility(mod)

mod <- fit_gravity(M,
                   D,
                   N,
                   n_chain=2,
                   n_burn=1000,
                   n_samp=5000,
                   n_thin=5,
                   DIC=TRUE)

summarize_mobility(mod)
