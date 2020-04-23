get_unique_ids(travel_data_sim)

# Starting at admin level 1
get_unique_ids(travel_data_sim, adm_start=1)

# Use numeric facto levels
get_unique_ids(travel_data_sim, name_class='numeric')

# Numeric for only the origin locations
get_unique_ids(travel_data_sim, dest=F, name_class='numeric')
