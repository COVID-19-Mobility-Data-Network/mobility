n_orig <- 20
orig_id <- LETTERS[1:n_orig]

N <- rpois(n_orig, 50)     # population size of each origin
p <- rbeta(n_orig, 2, 5)   # probability of leaving origin



V_travel <- setNames(rbinom(n_orig, N, p), orig_id)
V_tot <- setNames(N, orig_id)

i <- sample(1:n_orig, 10) # missing observations

V_travel[i] <- V_tot[i] <- NA

prob_trav <- fit_prob_travel(travel=V_travel,
                             tot=V_tot,
                             n_chain=4,
                             n_burn=1000,
                             n_samp=1000,
                             n_thin=2)

head(prob_trav)

library(ggplot2)
ggplot(data=prob_trav) +
  geom_point(aes(x=Mean, y=orig_id)) +
  ggstance::geom_linerangeh(aes(y=orig_id, xmin=CI2.5, xmax=CI97.5)) +
  geom_vline(aes(xintercept=Mean[ which(is.na(travel))[1] ]), color='red', linetype=2) +
  xlab('Probability of travel outside origin') + ylab('Origin') +
  theme_bw() + theme(axis.text.x=element_text(size=8),
                     axis.text.y=element_text(size=6),
                     axis.title.x=element_text(size=12, margin = margin(t = 15)),
                     axis.title.y=element_text(size=12, margin = margin(r = 15)),
                     legend.position='right')
