## code to prepare `gravity_priors` dataset goes here

gravity_priors <- data.frame(
  matrix(c('theta', '19.9065788', '1.990397e+01',
           'omega_1', '7.2883207', '1.486786e+01',
           'omega_2', '0.6769415', '1.326634e-04',
           'gamma', '1.4997876', '7.684512e-05'),
         ncol=3,
         byrow=TRUE),
  stringsAsFactors=FALSE)

colnames(gravity_priors) <- c('param', 'Mean', 'SD')
gravity_priors$param <- as.character(gravity_priors$param)
gravity_priors$Mean <- as.numeric(gravity_priors$Mean)
gravity_priors$SD <- as.numeric(gravity_priors$SD)

usethis::use_data(gravity_priors, overwrite=TRUE)
