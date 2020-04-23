n <- 5

xy <- data.frame(x=rnorm(n, -90, 1),
                 y=rnorm(n, 30, 1),
                 id=LETTERS[1:n])

D <- get_distance_matrix(x=xy[,1],
                         y=xy[,2],
                         id=xy[,3])


# From simulated travel data
xy <- get_unique_coords(travel_data_sim)
D <- get_distance_matrix(x=xy[,1],
                         y=xy[,2],
                         id=xy[,3])

D*111.35 # decimal degrees to km
