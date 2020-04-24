## code to prepare `travel_data_sim` dataset goes here

set.seed(1234)

# Travel among locations
trip <- travel_data_template
n <- 30 # number of locations
trip[1:n,] <- NA # add rows for each location

# Time span of travel data
trip$date_start <- as.Date("2020-01-01")
trip$date_stop <- trip$date_start + 7
trip$date_span <- difftime(stay$date_stop, stay$date_start, units='days')

# Origin info: some counties within the same state
trip$orig_adm0 <- trip$dest_adm0 <- 'A' # Country
trip$orig_adm1 <- trip$dest_adm1 <- 'B' # State
trip$orig_adm2 <- sample(LETTERS, n, replace=T)
trip$dest_adm2 <- sample(LETTERS, n, replace=T)
trip$orig_type <- trip$dest_type <- 'County' # Type of admin unit for lowest admin level

trip$orig_x <- rnorm(n, -90, 2)
trip$orig_y <- rnorm(n, 30, 1)
trip$dest_x <- rnorm(n, -90, 2)
trip$dest_y <- rnorm(n, 30, 1)

trip$orig_pop <- rnbinom(n, size=5, mu=5000)
trip$dest_pop <- rnbinom(n, size=10, mu=10000)

trip$trips <- rnbinom(n, size=1, mu=100) # Number of reported trips
trip <- trip[!(trip$orig_adm2 == trip$dest_adm2),]


# Stays in home location
stay <- travel_data_template
origins <- unique(c(trip$orig_adm2, trip$orig_adm2))
stay[1:length(origins),] <- NA

# Time span of travel survey
stay$date_start <- trip$date_start[1]
stay$date_stop <- trip$date_stop[1]
stay$date_span <- difftime(trip$date_stop, trip$date_start, units='days')

stay$orig_adm0 <- stay$dest_adm0 <- 'A' # Country
stay$orig_adm1 <- stay$dest_adm1 <- 'B' # State
stay$orig_adm2 <- stay$dest_adm2 <- origins
stay$orig_type <- stay$dest_type <- 'County'

for (i in 1:length(origins)) {

  sel <- which(trip$orig_adm2 == stay$orig_adm2[i])[1]
  stay$orig_x[i] <- stay$dest_x[i] <- trip$orig_x[sel]
  stay$orig_y[i] <- stay$dest_y[i] <- trip$orig_y[sel]
  stay$orig_pop[i] <- stay$dest_pop[i] <- trip$orig_pop[sel]
}

# Number of reported trip within home county
stay$trips <- rnbinom(length(origins), size=10, mu=1000)

# Combine
travel_data_sim <- dplyr::full_join(trip, stay)

usethis::use_data(travel_data_sim, overwrite=TRUE)
