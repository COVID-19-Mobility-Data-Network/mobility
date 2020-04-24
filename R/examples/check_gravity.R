M <- mobility_matrices$M
D <- mobility_matrices$D
N <- mobility_matrices$N

mod <- fit_gravity(M, D, N, DIC=TRUE)

check_gravity(M=M, N=N, D=D, mod=mod)
