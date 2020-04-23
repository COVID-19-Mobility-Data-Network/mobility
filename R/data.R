##' Example data matrices required by gravity functions
##'
##' This data object is a list containing named matrices required by the \code{\link{sim_gravity}} and \code{\link{fit_gravity}} functions.
##' The matrices provide hypothetical trip counts representing travel among 10 locations (\eqn{M}), along with distances (\eqn{D}) and population sizes (\eqn{N}).
##'
##' @format a list of 3
##' \describe{
##'   \item{M}{numeric matrix containing simulated trip counts}
##'   \item{D}{numeric matrix containing distances amopng locations in decimal degrees}
##'   \item{N}{numeric vector containing population sizes of each location}
##'   }
##'
##' @author John Giles
##'
"mobility_matrices"


##' Generalized template for travel data
##'
##' This template data frame provides a general structure for travel data that integrates with data synthesis and
##' modeling functions. Stays (individuals reported as not traveling outside home location) are to be included in this data frame,
##' where origin and destination are the same. Note that models fitted and then extrapolated
##' using other data assume that the same method for defining population size is used throughout. Either dates or time span must be filled.
##'
##' @format a data frame with empty columns and generalized column names
##' \describe{
##'   \item{date_start}{date: beginning of the time interval for the trip count}
##'   \item{date_stop}{date: end of the time interval for the trip count}
##'   \item{date_span}{integer: time span in days}
##'   \item{orig_adm0}{character: name of highest administration level of origin location (Country)}
##'   \item{orig_adm1}{character: name of administration level 1 of origin location (e.g. Division, State)}
##'   \item{orig_adm2}{character: name of administration level 2 of origin location (e.g. District, County)}
##'   \item{orig_adm3}{character: name of administration level 3 of origin location (e.g. Sub-district, Province)}
##'   \item{orig_adm4}{character: name of administration level 4 of origin location (e.g. City, Municipality)}
##'   \item{orig_adm5}{character: name of administration level 5 of origin location (e.g. Town, Village, Community, Ward)}
##'   \item{orig_type}{character: administrative type for the origin location (e.g. sub-district, community vs town, or urban vs rural)}
##'   \item{orig_x}{numeric: longitude of origin location centroid in decimal degrees (centroid of smallest admin unit)}
##'   \item{orig_y}{numeric: latitude of origin location centroid in decimal degrees (centroid of smallest admin unit)}
##'   \item{orig_pop}{numeric: population size of lowest administrative unit for origin location}
##'   \item{dest_adm0}{character: name of highest administration level of destination location (Country)}
##'   \item{dest_adm1}{character: name of administration level 1 of destination location (e.g. Division, State)}
##'   \item{dest_adm2}{character: name of administration level 2 of destination location (e.g. District, County)}
##'   \item{dest_adm3}{character: name of administration level 3 of destination location (e.g. Sub-district, Province)}
##'   \item{dest_adm4}{character: name of administration level 4 of destination location (e.g. City, Municipality)}
##'   \item{dest_adm5}{character: name of administration level 5 of destination location (e.g. Town, Village, Community, Ward)}
##'   \item{dest_type}{character: administrative type for the destination location (e.g. sub-district, community vs town, or urban vs rural)}
##'   \item{dest_x}{numeric: longitude of destination location in decimal degrees (centroid of smallest admin unit)}
##'   \item{dest_y}{numeric: latitude of destination location centroid in decimal degrees (centroid of smallest admin unit)}
##'   \item{dest_pop}{numeric: population size of lowest administrative unit for destination location}
##'   \item{trips}{numeric: total number of observed trips made from origin to destination during time span}
##'   }
##'
##' @author John Giles
##'
##' @example R/examples/travel_data_template.R
##'
"travel_data_template"


##' Simulated travel data
##'
##' This data set contains simulated values of location information and observed number of trips among locations and within
##' home locations. Data structure inherited from the \code{\link{travel_data_template}} object.
##'
##' @format a \code{\link{travel_data_template}} data frame
##'
##' @author John Giles
##'
"travel_data_sim"
