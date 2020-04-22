M <- mobility_matrices$M
D <- mobility_matrices$D
N <- mobility_matrices$N

mod <- fit_gravity(M, D, N)
summarize_mobility(mod)

mod <- fit_gravity(M, D, N, DIC=T)
summarize_mobility(mod)

mod <- fit_gravity(M, D, N, DIC=T, parallel=T)
summarize_mobility(mod)
