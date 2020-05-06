## code to prepare `mobility_matrices` dataset goes here

set.seed(1234)

n <- 10
ids <- LETTERS[1:n]

# Distance matrix
D <- get_distance_matrix(x=rnorm(n, -90, 9),
                         y=rnorm(n, 30, 3),
                         id=ids)*111.35

# Vector of population sizes
N <- rpois(n, lambda=10000)
#N <- rnbinom(n, size=5, mu=5000)
names(N) <- ids

tau <- rbeta(n, 2, 2) # probability of travel outside origin

# Simulate connectivity matrix given gravity model parameters
pi <- sim_gravity(N=N,
                  D=D,
                  theta=3,
                  omega_1=2,
                  omega_2=0.75,
                  gamma=1.5)

# Get trip counts
M <- pi; M[,] <- NA
for(i in 1:nrow(M)) {
  for(j in 1:ncol(M)) {

    M[i,j] <- ifelse(
      i == j,
      N[i] * (1 - tau[i]),
      N[i] * tau[i] * pi[i,j]
    )
  }
}

rowSums(M)
M <- round(M)

# add noise
for(i in 1:n) {
  for (j in 1:n) {
    M[i,j] <- rpois(1, lambda=M[i,j])
  }
}

# missing observations
M[sample(1:(n^2), (n^2)*0.3)] <- NA

mobility_matrices <- list(M=M, D=D, N=N)

usethis::use_data(mobility_matrices, overwrite=TRUE)
