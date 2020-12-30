mod <- mobility(data=mobility_matrices, model='gravity', type='transport', DIC=TRUE)

residuals(mod, type='pearson')
