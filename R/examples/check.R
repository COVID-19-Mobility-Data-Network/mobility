mod <- mobility(data=mobility_matrices, model='gravity', type='transport', DIC=TRUE)
check(mod)
