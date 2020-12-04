# Gravity model
M <- mobility_matrices$M
D <- mobility_matrices$D
N <- mobility_matrices$N

mod <- fit_gravity(M, D, N, type='transport', DIC=TRUE)
summary(mod)
