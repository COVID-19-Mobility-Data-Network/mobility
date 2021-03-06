n_orig <- 20
n_missing <- 4
orig_id <- LETTERS[1:n_orig]

N <- rpois(n_orig, 100)    # population size of each origin
p <- rbeta(n_orig, 1, 2)   # probability of leaving origin

travel <- setNames(rbinom(n_orig, N, p), orig_id)
total <- setNames(N, orig_id)

miss <- sample(1:n_orig, n_missing) # missing observations
travel[miss] <- total[miss] <- NA

# Estimate probability of travel for each locations (missing locations regress to mean)
prob_trav <- fit_prob_travel(travel=travel, total=total)
prob_trav_smry <- summary(prob_trav, probs=c(0.025, 0.25, 0.75, 0.975))

library(ggplot2)
ggplot(data=prob_trav_smry) +
  geom_vline(aes(xintercept=mean[ which(is.na(travel))[1] ]), color='red', linetype=2) +
  geom_point(aes(x=mean, y=orig_id), size=2) +
  ggstance::geom_linerangeh(aes(y=orig_id, xmin=Q2.5, xmax=Q97.5)) +
  ggstance::geom_linerangeh(aes(y=orig_id, xmin=Q25, xmax=Q75), size=1) +
  xlab('Probability of travel outside origin') + ylab('Origin') +
  xlim(0,1) +
  theme_bw() + theme(axis.text.x=element_text(size=10),
                     axis.text.y=element_text(size=10),
                     axis.title.x=element_text(size=12, margin = margin(t = 15)),
                     axis.title.y=element_text(size=12, margin = margin(r = 15)),
                     panel.border = element_rect(colour = "black", fill=NA, size=1),
                     legend.position='right')
