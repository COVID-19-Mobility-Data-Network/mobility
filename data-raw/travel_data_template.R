## code to prepare `travel_data_template` dataset goes here

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

usethis::use_data(travel_data_template, overwrite=TRUE)
