mods <- list(
  mobility(data=mobility_matrices, model='gravity', type='power', DIC=TRUE),
  mobility(data=mobility_matrices, model='radiation', type='basic'),
  mobility(data=mobility_matrices, model='departure-diffusion', type='power', DIC=TRUE)
)

compare(mods)
