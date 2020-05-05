sim_prob_travel(0.5, 0.04)

sim_prob_travel(mu=c(0.8, 0.6),
                sigma=c(0.08, 0.06),
                id=c('A', 'B'))

# Simulate with estimated parameters
n_orig <- 6
n_missing <- 3
orig_id <- LETTERS[1:n_orig]

N <- rpois(n_orig, 100)    # population size of each origin
p <- rbeta(n_orig, 2, 2)   # probability of leaving origin

travel <- setNames(rbinom(n_orig, N, p), orig_id)
total <- setNames(N, orig_id)

miss <- sample(1:n_orig, n_missing) # missing observations
travel[miss] <- total[miss] <- NA

# Estimate probability of travel for each locations (missing locations regress to mean)
prob_trav <- summarize_mobility(
  fit_prob_travel(travel=travel, total=total)
)

sim_prob_travel(mu=prob_trav$Mean,
                sigma=prob_trav$SD,
                id=names(travel),
                n=5)
