x <- cbind(travel_data_sim, get_unique_ids(travel_data_sim, adm_start=2))

# Unique coordinates for all locations
get_pop_vec(x)

# Origins only
get_pop_vec(x, dest=FALSE)

# Destinations only
get_pop_vec(x, orig=FALSE)

# Without supplied ids
get_pop_vec(travel_data_sim)
