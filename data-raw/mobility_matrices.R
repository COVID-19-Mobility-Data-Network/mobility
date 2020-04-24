## code to prepare `mobility_matrices` dataset goes here

set.seed(1234)

n <- 10
ids <- LETTERS[1:n]

# Distance matrix
D <- get_distance_matrix(x=rnorm(n, -90, 5),
                         y=rnorm(n, 30, 3),
                         id=ids)*111.35

# Vector of population sizes
N <- rnbinom(n, size=1, mu=50000)
names(N) <- ids

# Simulate connectivity matrix given gravity model parameters
M <- sim_gravity(N=N,
                 D=D,
                 theta=20,
                 omega_1=17,
                 omega_2=0.7,
                 gamma=1.5,
                 counts=TRUE)

# add noise
for(i in 1:n) {
  for (j in 1:n) {
    M[i,j] <- rnbinom(1, size=5, mu=M[i,j])
  }
}

# missing observations
M[sample(1:(n^2), (n^2)*0.5)] <- NA

mobility_matrices <- list(M=M, D=D, N=N)

usethis::use_data(mobility_matrices, overwrite=TRUE)
