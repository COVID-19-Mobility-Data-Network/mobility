M <- mobility_matrices$M
D <- mobility_matrices$D
N <- mobility_matrices$N

mod <- fit_gravity(M, D, N)
summarize_mobility(mod)

mod <- fit_gravity(M, D, N, DIC=T)
summarize_mobility(mod)

mod <- fit_gravity(M, D, N,
                   n_burn=1000,
                   n_chain=2000,
                   n_thin=10,
                   DIC=T,
                   parallel=T)
summarize_mobility(mod)
