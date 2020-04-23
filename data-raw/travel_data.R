travel_data_template <- data.frame(
     date_start=as.Date(character()),
     date_stop=as.Date(character()),
     date_span=as.difftime(numeric(), units='days'),
#     indiv_id=integer(),      # unique individual identifier
#     indiv_age=numeric(),     # Age of participant
#     indiv_sex=logical(),     # Gender of perticipant
#     indiv_type=character(),     # If individual participants belong to different groups
     orig_adm0=character(),   # Name of highest administration level of origin location (country)
     orig_adm1=character(),   # Name of administration level 1 of origin location
     orig_adm2=character(),   # Name of administration level 2 of origin location
     orig_adm3=character(),   # Name of administration level 3 of origin location
     orig_adm4=character(),   # Name of administration level 4 of origin location
     orig_adm5=character(),   # Name of administration level 5 of origin location
     orig_type=character(),   # What is the administrative type for the origin location? (e.g. sub-district, community vs town, or urban vs rural)
     orig_x=numeric(),        # Longitude of origin location in decimal degrees
     orig_y=numeric(),        # Latitude of origin location in decimal degrees
     orig_pop=numeric(),      # Population size of lowest administrative unit
     dest_adm0=character(),   # Name of highest administration level of destination location (country)
     dest_adm1=character(),   # Name of administration level 1 of destination location
     dest_adm2=character(),   # Name of administration level 2 of destination location
     dest_adm3=character(),   # Name of administration level 3 of destination location
     dest_adm4=character(),   # Name of administration level 4 of destination location
     dest_adm5=character(),   # Name of administration level 5 of destination location
     dest_type=character(),   # If origin and destination locations have different types (e.g. community vs town, or urban vs rural)
     dest_x=numeric(),        # Longitude of origin location in decimal degrees
     dest_y=numeric(),        # Latitude of origin location in decimal degrees
     dest_pop=numeric(),      # Population size of lowest administrative unit
     trips=numeric(),          # Total number of trips individual made from origin to destination during time span of travel survey
stringsAsFactors=FALSE)

save(travel_data_template, file='data/travel_data_template.rda')


# Simulate and example of travel data
set.seed(1234)

# Travel among locations
trip <- travel_data_template
n <- 10 # number of locations
trip[1:n,] <- NA # add rows for each location

# Time span of travel data
trip$date_start <- as.Date("2020-01-01")
trip$date_stop <- trip$date_start + 7
trip$date_span <- difftime(trip$date_stop, trip$date_start, units='days')

# Origin info: some counties within the same state
trip$orig_adm0 <- trip$dest_adm0 <- 'A' # Country
trip$orig_adm1 <- trip$dest_adm1 <- 'B' # State
x <- sample(LETTERS, n*2) # Some counties
trip$orig_adm2 <- x[1:n]
trip$dest_adm2 <- x[(n+1):(n*2)]
trip$orig_type <- trip$dest_type <- 'County' # Type of admin unit for lowest admin level

trip$orig_x <- rnorm(n, -90, 3)
trip$orig_y <- rnorm(n, 30, 2)
trip$dest_x <- rnorm(n, -90, 3)
trip$dest_y <- rnorm(n, 30, 2)

trip$orig_pop <- rpois(n, 5000)
trip$dest_pop <- rpois(n, 10000)

# Number of reported trips
trip$trips <- rnbinom(n, size=1, mu=100)


# Stays in home location
stay <- travel_data_template
x <- c(trip$orig_adm2, trip$dest_adm2)
stay[1:length(x),] <- NA

# Time span of travel survey
stay$date_start <- trip$date_start[1]
stay$date_stop <- trip$date_stop[1]
stay$date_span <- trip$date_span[1]

stay$orig_adm0 <- stay$dest_adm0 <- 'A' # Country
stay$orig_adm1 <- stay$dest_adm1 <- 'B' # State
stay$orig_adm2 <- stay$dest_adm2 <- x
stay$orig_type <- stay$dest_type <- 'County'

stay$orig_x <- stay$dest_x <- c(trip$orig_x, trip$dest_x)
stay$orig_y <- stay$dest_y <- c(trip$orig_y, trip$dest_y)
stay$orig_pop <- stay$dest_pop <- c(trip$orig_pop, trip$dest_pop)

# Number of reported trip within home county
stay$trips <- rnbinom(n, size=10, mu=1000)

head(stay)

# Combine
travel_data_sim <- dplyr::full_join(trip, stay)
head(travel_data_sim)

save(travel_data_sim, file='data/travel_data_sim.rda')
