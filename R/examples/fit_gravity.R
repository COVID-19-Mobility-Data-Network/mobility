n <- 10
ids <- LETTERS[1:n]

# Distance matrix
D <- get_distance_matrix(x=rnorm(n, 100, 5),
                         y=rnorm(n, 20, 2),
                         id=ids)

# Vector of population sizes
N <- rpois(n, 1000)
names(N) <- ids

# Simulate trip counts based on model parameters
M <- sim_gravity(N=N,
                 D=D,
                 theta=20,
                 omega_1=15,
                 omega_2=0.75,
                 gamma=1.5,
                 counts=TRUE)

mod <- fit_gravity(M, D, N, DIC=TRUE, parallel = T)
summarize_mobility(mod)
