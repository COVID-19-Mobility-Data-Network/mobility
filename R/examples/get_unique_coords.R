x <- cbind(travel_data_sim, get_unique_ids(travel_data_sim, adm_start=1))

# Unique coordinates for all locations
get_unique_coords(x)

# Origins only
get_unique_coords(x, dest=FALSE)

# Destinations only
get_unique_coords(x, orig=FALSE)

# Without supplied ids
get_unique_coords(travel_data_sim)
