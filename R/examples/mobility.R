mod <- mobility(data=mobility_matrices, model='gravity', type='transport')

mod <- mobility(data=mobility_matrices, model='radiation', type='finite')

mod <- mobility(data=mobility_matrices, model='departure-diffusion', type='power')
