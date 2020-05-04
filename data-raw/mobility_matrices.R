## code to prepare `mobility_matrices` dataset goes here

set.seed(1234)

n <- 10
ids <- LETTERS[1:n]

# Distance matrix
D <- get_distance_matrix(x=rnorm(n, -90, 2),
                         y=rnorm(n, 30, 1),
                         id=ids)*111.35

# Vector of population sizes
N <- rnbinom(n, size=1, mu=50000)
names(N) <- ids

# Add diagnoal
tau <- rbeta(n, 2, 2)                # probability of leaving origin
stay <- round(N*(1-tau))             # number that stays in origin

# Simulate connectivity matrix given gravity model parameters
pi <- sim_gravity(N=N,
                  D=D,
                  theta=20,
                  omega_1=17,
                  omega_2=0.7,
                  gamma=1.5,
                  counts=FALSE)

for (i in 1:nrow(pi)) pi[i,] <- pi[i,]*tau[i]
diag(pi) <- (1-tau)
rowSums(pi)

M <- D; M[,] <- NA
for (i in 1:nrow(M)) M[i,] <- round(pi[i,] * N[i])

# add noise
#for(i in 1:n) {
#  for (j in 1:n) {
#    M[i,j] <- rnbinom(1, size=5, mu=M[i,j])
#  }
#}

# missing observations
M[sample(1:(n^2), (n^2)*0.2)] <- NA

mobility_matrices <- list(M=M, D=D, N=N)

usethis::use_data(mobility_matrices, overwrite=TRUE)
