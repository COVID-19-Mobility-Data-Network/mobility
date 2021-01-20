mod <- mobility(data=mobility_matrices, model='gravity', type='transport')
mod <- mobility(data=mobility_matrices, model='departure-diffusion', type='power', hierarchical = T)
predict(object=mod)

n <- 5
ids <- letters[1:n]

# Distance matrix
D <- get_distance_matrix(x=rnorm(n, -100, 2),
                         y=rnorm(n, 20, 1),
                         id=ids)*111.35

# Vector of population sizes
N <- rnbinom(n, size=5, mu=5000)
names(N) <- ids

# Predict mobility model using new data
predict(object=mod, newdata=list(D=D, N=N))
