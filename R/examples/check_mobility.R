M <- mobility_matrices$M
D <- mobility_matrices$D
N <- mobility_matrices$N

total <- rowSums(M, na.rm=TRUE)
mod_travel <- summarize_mobility(
  fit_prob_travel(total-diag(M), total)
)

mod_gravity <- summarize_mobility(
  fit_gravity(M, D, N)
)

mod_mobility <- summarize_mobility(
  fit_mobility(M, D, N)
)

check_mobility(M=M,
               mod=mod_travel)

check_mobility(M=M,
               D=D,
               N=N,
               mod=mod_gravity)

check_mobility(M=M,
               D=D,
               N=N,
               mod=mod_mobility)
