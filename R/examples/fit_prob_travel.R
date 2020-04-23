n_orig <- 6
n_missing <- 3
orig_id <- LETTERS[1:n_orig]

N <- rpois(n_orig, 100)    # population size of each origin
p <- rbeta(n_orig, 2, 2)   # probability of leaving origin

V_travel <- setNames(rbinom(n_orig, N, p), orig_id)
V_tot <- setNames(N, orig_id)

miss <- sample(1:n_orig, n_missing) # missing observations
V_travel[miss] <- V_tot[miss] <- NA

# Estimate probability of travel for observed locations and population mean
prob_trav <- fit_prob_travel(travel=V_travel,
                             total=V_tot,
                             DIC=TRUE)
prob_trav

# Estimate probability of travel for each locations (missing locations regress to mean)
prob_trav <- fit_prob_travel(travel=V_travel,
                             total=V_tot,
                             format_locations=TRUE)
prob_trav


library(ggplot2)
ggplot(data=prob_trav) +
  geom_vline(aes(xintercept=Mean[ which(is.na(travel))[1] ]), color='red', linetype=2) +
  geom_point(aes(x=Mean, y=orig_id), size=2) +
  ggstance::geom_linerangeh(aes(y=orig_id, xmin=CI2.5, xmax=CI97.5)) +
  xlab('Probability of travel outside origin') + ylab('Origin') +
  theme_bw() + theme(axis.text.x=element_text(size=8),
                     axis.text.y=element_text(size=6),
                     axis.title.x=element_text(size=12, margin = margin(t = 15)),
                     axis.title.y=element_text(size=12, margin = margin(r = 15)),
                     legend.position='right')
