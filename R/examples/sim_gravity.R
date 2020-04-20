n <- 5
ids <- LETTERS[1:n]

# Distance matrix
D <- get_distance_matrix(x=rnorm(n, 100, 5),
                         y=rnorm(n, 20, 2),
                         id=ids)

# Vector of population sizes
N <- rpois(n, 1000)
names(N) <- ids

# Simulate null model connectivity matrix
pi_hat <- sim_gravity(N=N, D=D)

# Simulate connectivity matrix given fitted gravity model parameters
pi_hat <- sim_gravity(N=N,
                      D=D,
                      theta=14,
                      omega_1=13,
                      omega_2=0.7,
                      gamma=1.5)

# Simulate trip counts based on fitted model parameters
M_hat <- sim_gravity(N=N,
                     D=D,
                     theta=14,
                     omega_1=13,
                     omega_2=0.7,
                     gamma=1.5,
                     counts=TRUE)
