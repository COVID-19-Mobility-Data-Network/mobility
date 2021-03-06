#--------------------------------
# Travel among locations
#--------------------------------

trip <- travel_data_template
n <- 3 # Add some observations
trip[1:n,] <- NA

# Time span of travel survey
trip$date_start <- as.Date("2020-01-01")
trip$date_stop <- trip$date_start + 30
trip$date_span <- difftime(trip$date_stop, trip$date_start, units='days')

# Participant info
trip$indiv_id <- sample(1:100, n)
trip$indiv_age <- round(runif(n, 5, 80))
trip$indiv_sex <- rbinom(n, 1, 0.5)

# Origin info
trip$orig_adm0 <- 'A'
trip$orig_adm1 <- 'A'
trip$orig_adm2 <- 'A'
trip$orig_adm3 <- LETTERS[1:n]
trip$orig_type <- 'Sub-district' # Type of admin unit for lowest admin level
trip$orig_x <- rnorm(n, 100, 5)
trip$orig_y <- rnorm(n, 20, 2)
trip$orig_pop <- rpois(n, 10000)

# Destination info
trip$dest_adm0 <- 'A'
trip$dest_adm1 <- 'A'
trip$dest_adm2 <- 'B'
trip$dest_adm3 <- LETTERS[(n+1):(n*2)]
trip$dest_type <- 'Sub-district' # Type of admin unit for lowest admin level
trip$dest_x <- rnorm(n, 100, 5)
trip$dest_y <- rnorm(n, 20, 2)
trip$dest_pop <- rpois(n, 5000)

# Number of reported trips
trip$trips <- rpois(n, 10)

head(trip)



#-----------------------
# Stays in home location
#-----------------------

stay <- travel_data_template
n <- 3 # add some observations
stay[1:n,] <- NA

# Time span of travel survey
stay$date_start <- as.Date("2020-01-01")
stay$date_stop <- stay$date_start + 30
stay$date_span <- difftime(trip$date_stop, trip$date_start, units='days')

# Participant info
stay$indiv_id <- sample(100:200, n)
stay$indiv_age <- round(runif(n, 5, 80))
stay$indiv_sex <- rbinom(n, 1, 0.5)

# Origin info
stay$orig_adm0 <- stay$dest_adm0 <- 'A'
stay$orig_adm1 <- stay$dest_adm1 <- 'A'
stay$orig_adm2 <- stay$dest_adm2 <- 'A'
stay$orig_adm3 <- stay$dest_adm3 <- LETTERS[1:n]
stay$orig_type <- stay$dest_type <- 'Sub-district'
stay$orig_x <- stay$dest_x <- rnorm(n, 100, 5)
stay$orig_y <- stay$dest_y <- rnorm(n, 20, 2)
stay$orig_pop <- stay$dest_pop <- rpois(n, 10000)

stay$trips <- NA

head(stay)

# Combine
survey_data <- dplyr::full_join(trip, stay)
head(survey_data)



#----------------------------------------
# Dataset with which to extrapolate model
#----------------------------------------

pred <- travel_data_template
n <- 6 # Add some observations
pred[1:n,] <- NA

# Time span of the interval over which to extrapolate the fitted model
pred$date_span <- as.difftime(7, units='days')

# Origin info
pred$orig_adm0 <- 'A'
pred$orig_adm1 <- 'A'
pred$orig_adm2 <- LETTERS[1:n]
pred$orig_type <- 'District' # Type of admin unit for lowest admin level
pred$orig_x <- rnorm(n, 100, 5)
pred$orig_y <- rnorm(n, 20, 2)
pred$orig_pop <- rpois(n, 1e+05)

# Number of reported trips (unobserved for extrapolation data)
trip$trips <- NA

head(pred)
