sim_prob_travel(0.6, 0.04)


sim_prob_travel(mu=c(0.84, 0.64),
                sigma=c(0.038, 0.045),
                id=c('A', 'B'))


# Simulate with estimated parameters
n_orig <- 6
n_missing <- 3
orig_id <- LETTERS[1:n_orig]

N <- rpois(n_orig, 100)    # population size of each origin
p <- rbeta(n_orig, 2, 2)   # probability of leaving origin

V_travel <- setNames(rbinom(n_orig, N, p), orig_id)
V_tot <- setNames(N, orig_id)

miss <- sample(1:n_orig, n_missing) # missing observations
V_travel[miss] <- V_tot[miss] <- NA

# Estimate probability of travel for each locations (missing locations regress to mean)
prob_trav <- fit_prob_travel(travel=V_travel,
                             total=V_tot,
                             format_locations=TRUE)

sim_prob_travel(mu=prob_trav$Mean,
                sigma=prob_trav$SD,
                id=prob_trav$orig_id)
