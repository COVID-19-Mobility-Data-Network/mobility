# A list containing shape and rate parameters of Gamma distribution
prior_list <- list(
  theta = get_gamma_params(gravity_priors$Mean[1],
                           gravity_priors$SD[1]),
  omega_1 = get_gamma_params(gravity_priors$Mean[2],
                             gravity_priors$SD[2]),
  omega_2 = get_gamma_params(gravity_priors$Mean[3],
                             gravity_priors$SD[3]*1000),
  gamma = get_gamma_params(gravity_priors$Mean[4],
                           gravity_priors$SD[4]*1000)
)

par(mfrow=c(2,2))
for (i in seq_along(prior_list)) {

  curve(dgamma(x,
               shape=prior_list[[i]][1],
               rate=prior_list[[i]][2]),
        c(0, 0, 0, 1)[i],
        c(100, 100, 1.5, 2)[i],
        ylab='Density',
        xlab=names(prior_list)[i])
}
