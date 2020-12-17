utils::globalVariables(
  c("date_start" ,"date_stop"  ,"date_span"  ,"indiv_id"   ,"indiv_age" ,
    "indiv_sex"  ,"indiv_type" ,"orig_adm0"  ,"orig_adm1"  ,"orig_adm2" ,
    "orig_adm3"  ,"orig_adm4"  ,"orig_adm5"  ,"orig_type"  ,"orig_x"    ,
    "orig_y"     ,"orig_pop"   ,"dest_adm0"  ,"dest_adm1"  ,"dest_adm2" ,
    "dest_adm3"  ,"dest_adm4"  ,"dest_adm5"  ,"dest_type"  ,"dest_x"    ,
    "dest_y"     ,"dest_pop"   ,"trips", "orig_id", "dest_id")
)

utils::globalVariables(c("pop", "x", "y", "id", "i", "j", "k", "travel", "n_distinct"))

##' Get mobility matrix from longform data
##'
##' Takes X and Y coordinates of two locations and returns cross distance for all entries.
##'
##' @param orig vector of origin names
##' @param dest vector of destination names
##' @param value vector of observed values for each origin/destination pair
##' @param missing_obs filler value for missing observations (default = \code{NA})
##'
##' @return named numeric matrix
##'
##' @author John Giles
##'
##' @example R/examples/get_mob_matrix.R
##'
##' @family data synthesis
##'
##' @export
##'

get_mob_matrix <- function(orig,
                           dest,
                           value,
                           missing_obs='NA'
){

  if (is.factor(orig)) orig <- as.character(orig)
  if (is.factor(dest)) dest <- as.character(dest)

  fac <- factor(sort(unique(c(orig, dest))))

  m <- Matrix::formatSpMatrix(
    Matrix::sparseMatrix(i=as.integer(factor(orig, levels=levels(fac))),
                         j=as.integer(factor(dest, levels=levels(fac))),
                         x=value,
                         dims=rep(length(fac), 2)),
    zero.print=missing_obs
  )

  suppressWarnings(class(m) <- "numeric")
  dimnames(m) <- list(origin=levels(fac), destination=levels(fac))
  m[order(dimnames(m)$origin), order(dimnames(m)$destination)]
}



##' Get unique origin and destination names
##'
##' This function builds unique character string identifiers for all origins and destinations.
##'
##' @param data generalized data frame described in \code{\link{travel_data_template}}
##' @param orig logical indicating whether to include unique coordinates from origin locations (default=TRUE)
##' @param dest logical indicating whether to include unique coordinates from destination locations (default=TRUE)
##' @param adm_start highest administrative level to include in unique names (default = NULL, which uses highest observed in the data)
##' @param adm_stop lowest administrative level to include in unique names (default = NULL, which uses lowest observed in the data)
##' @param name_class character indicating whether unique names should be either a unique character string (\code{name_class = "character"}) or a unique integer code (\code{name_class = "numeric"})
##'
##' @return two column dataframe containing unique names
##'
##' @author John Giles
##'
##' @example R/examples/get_unique_ids.R
##'
##' @family utility
##'
##' @export
##'

get_unique_ids <- function(data,
                           orig=TRUE,
                           dest=TRUE,
                           adm_start=NULL,
                           adm_stop=NULL,
                           name_class="character"
) {

  if (is.null(orig) & is.null(dest)) stop('Both orig and dest cannot be FALSE')

  pads <- data.frame(
    adm_level=0:6,
    n=c(3, # admin0
        3, # admin1
        3, # admin2
        4, # admin3
        5, # admin4
        5, # admin5
        5) # admin6
  )

  if (all(orig, dest)) {

    sel <- grep('orig_adm', colnames(data))
    if (is.null(adm_start)) adm_start <- 0
    if (is.null(adm_stop)) adm_stop <- length(sel)-1
    sel <- sel[order(colnames(data)[sel])]
    sel_orig <- sel[(adm_start+1):(adm_stop+1)]

    sel <- grep('dest_adm', colnames(data))
    if (is.null(adm_start)) adm_start <- 0
    if (is.null(adm_stop)) adm_stop <- length(sel)-1
    sel <- sel[order(colnames(data)[sel])]
    sel_dest <- sel[(adm_start+1):(adm_stop+1)]


    if (name_class == "numeric") {

      for (i in seq_along(sel_orig)) {

        n_pad <- pads$n[(adm_start+1) + (i-1)]
        fac <- factor(sort(unique(c(data[,sel_orig[i]], data[,sel_dest[i]]))))

        data[,sel_orig[i]] <- stringr::str_pad(as.numeric(factor(data[,sel_orig[i]], levels=levels(fac))),
                                               width=n_pad,
                                               side='left',
                                               pad="0")

        data[,sel_dest[i]] <- stringr::str_pad(as.numeric(factor(data[,sel_dest[i]], levels=levels(fac))),
                                               width=n_pad,
                                               side='left',
                                               pad="0")
      }
    }

    orig_id <- apply(data[,sel_orig], 1, function(x) paste(x[!is.na(x)], collapse='_'))
    dest_id <- apply(data[,sel_dest], 1, function(x) paste(x[!is.na(x)], collapse='_'))
    return(data.frame(orig_id, dest_id, row.names=NULL, stringsAsFactors=FALSE))

  } else if (all(orig, !dest)) {

    sel <- grep('orig_adm', colnames(data))
    if (is.null(adm_start)) adm_start <- 0
    if (is.null(adm_stop)) adm_stop <- length(sel)-1
    sel <- sel[order(colnames(data)[sel])]
    sel_orig <- sel[(adm_start+1):(adm_stop+1)]

    if (name_class == "numeric") {

      for (i in seq_along(sel_orig)) {

        n_pad <- pads$n[(adm_start+1) + (i-1)]
        data[,sel_orig[i]] <- stringr::str_pad(as.numeric(factor(data[,sel_orig[i]])),
                                               width=n_pad,
                                               side='left',
                                               pad="0")
      }
    }

    orig_id <- apply(data[,sel_orig], 1, function(x) paste(x[!is.na(x)], collapse='_'))
    return(data.frame(orig_id, row.names=NULL, stringsAsFactors=FALSE))

  } else if (all(!orig, dest)) {

    sel <- grep('dest_adm', colnames(data))
    if (is.null(adm_start)) adm_start <- 0
    if (is.null(adm_stop)) adm_stop <- length(sel)-1
    sel <- sel[order(colnames(data)[sel])]
    sel_dest <- sel[(adm_start+1):(adm_stop+1)]

    if (name_class == "numeric") {

      for (i in seq_along(sel_dest)) {

        n_pad <- pads$n[(adm_start+1) + (i-1)]
        data[,sel_dest[i]] <- stringr::str_pad(as.numeric(factor(data[,sel_dest[i]])),
                                               width=n_pad,
                                               side='left',
                                               pad="0")
      }
    }

    dest_id <- apply(data[,sel_dest], 1, function(x) paste(x[!is.na(x)], collapse='_'))
    return(data.frame(dest_id, row.names=NULL, stringsAsFactors=FALSE))
  }
}


##' Get unique coordinates
##'
##' This function returns the coordinates for all unique origins and/or destinations in supplied data.
##'
##' @param data generalized data frame described in \code{\link{travel_data_template}} or derivative thereof
##' @param orig logical indicating whether to include unique coordinates from origin locations (default=TRUE)
##' @param dest logical indicating whether to include unique coordinates from destination locations (default=TRUE)
##'
##' @return three column dataframe containing unique coordinates and location names
##'
##' @author John Giles
##'
##' @example R/examples/get_unique_coords.R
##'
##' @family utility
##'
##' @importFrom magrittr %>%
##'
##' @export
##'

get_unique_coords <- function(data,
                              orig=TRUE,
                              dest=TRUE
) {

  if (!all(c('orig_id', 'dest_id') %in% colnames(data))) {

    data <- cbind(data, get_unique_ids(data))
    message('Added missing unique location names.')
  }

  if (is.factor(data$orig_id)) data$orig_id <- as.character(data$orig_id)
  if (is.factor(data$dest_id)) data$dest_id <- as.character(data$dest_id)

  if (all(orig, dest)) {

    out <- rbind(
      data %>%
        dplyr::rename(x=orig_x, y=orig_y, id=orig_id) %>%
        dplyr::select(x, y, id) %>%
        data.frame(),
      data %>%
        dplyr::rename(x=dest_x, y=dest_y, id=dest_id) %>%
        dplyr::select(x, y, id) %>%
        data.frame()
    )

  } else if (all(orig, !dest)) {

    out <- data %>%
      dplyr::rename(x=orig_x, y=orig_y, id=orig_id) %>%
      dplyr::select(x, y, id) %>%
      data.frame()

  } else if (all(!orig, dest)) {

    out <- data %>%
      dplyr::rename(x=dest_x, y=dest_y, id=dest_id) %>%
      dplyr::select(x, y, id) %>%
      data.frame()

  } else {

    stop('At least one of the "orig" or "dest" arguments must be TRUE')
  }

  out %>%
    dplyr::group_by(id) %>%
    dplyr::distinct(id, .keep_all = T) %>%
    dplyr::arrange(id) %>%
    data.frame()
}


##' Get unique origin and destination names
##'
##' This function returns coordinates for all unique locations in supplied data.
##'
##' @param data generalized data frame described in \code{\link{travel_data_template}} or derivative thereof
##' @param orig logical indicating whether to include origin location in unique names (default= TRUE)
##' @param dest logical indicating whether to include destination location in unique names (default= TRUE)
##'
##' @return two column dataframe containing unique names
##'
##' @author John Giles
##'
##' @example R/examples/get_pop_vec.R
##'
##' @family utility
##'
##' @importFrom magrittr %>%
##'
##' @export
##'

get_pop_vec <- function(data,
                        orig=TRUE,
                        dest=TRUE
) {

  if (!all(c('orig_id', 'dest_id') %in% colnames(data))) {

    data <- cbind(data, get_unique_ids(data))
    message('Added missing unique location names.')
  }

  if (is.factor(data$orig_id)) data$orig_id <- as.character(data$orig_id)
  if (is.factor(data$dest_id)) data$dest_id <- as.character(data$dest_id)

  if (all(orig, dest)) {

    out <- rbind(
      data %>%
        dplyr::rename(pop=orig_pop, id=orig_id) %>%
        dplyr::select(pop, id) %>%
        data.frame(),
      data %>%
        dplyr::rename(pop=dest_pop, id=dest_id) %>%
        dplyr::select(pop, id) %>%
        data.frame()
    )

  } else if (all(orig, !dest)) {

    out <- data %>%
      dplyr::rename(pop=orig_pop, id=orig_id) %>%
      dplyr::select(pop, id) %>%
      data.frame()

  } else if (all(!orig, dest)) {

    out <- data %>%
      dplyr::rename(pop=dest_pop, id=dest_id) %>%
      dplyr::select(pop, id) %>%
      data.frame()

  } else {

    stop('At least one of the "orig" or "dest" arguments must be TRUE')
  }

  out <- out %>%
    dplyr::group_by(id) %>%
    dplyr::distinct(id, .keep_all=T) %>%
    dplyr::arrange(id) %>%
    data.frame()

  setNames(out$pop, out$id)
}


##' Build distance matrix from XY coordinates
##'
##' This function builds the pairwise distance matrix from vectors of XY coordinates and associated names.
##'
##' @param x vector giving X coordinates
##' @param y vector giving Y coordinates
##' @param id vector of names for each location
##'
##' @return a named matrix of pairwise distances among locations
##'
##' @author John Giles
##'
##' @example R/examples/get_distance_matrix.R
##'
##' @family data synthesis
##'
##' @export
##'

get_distance_matrix <- function(x,   # x coord
                                y,   # y coord
                                id   # name associated with each element
) {
  xy <- cbind(x, y)

  suppressMessages(
    out <- spatstat::pairdist(
      spatstat::as.ppp(xy,
                       spatstat::bounding.box.xy(xy),
                       check=FALSE)
    )
  )

  dimnames(out) <- list(origin=id, destination=id)
  out[order(dimnames(out)$origin), order(dimnames(out)$destination)]
}




##' Get distance matrix for two different set of coordinates
##'
##' Takes XY coordinates of two sets of locations and returns cross distance for all entries.
##'
##' @param xy1 two column matrix of XY coordinates for first group
##' @param xy2 two column matrix of XY coordinates for second group
##' @param id1 optional names for first group
##' @param id2 optional names for second group
##'
##' @return numeric scalar or vector
##'
##' @author John Giles
##'
##' @example R/examples/get_crossdist.R
##'
##' @family data synthesis
##'
##' @export
##'

get_crossdist <- function(xy1,
                          xy2,
                          id1=NULL,
                          id2=NULL
) {

  if (!is.null(xy1) | !is.null(xy2)) colnames(xy1) <- colnames(xy2) <- rep(NA, ncol(xy1))

  suppressWarnings(
    window <- spatstat::bounding.box.xy(rbind(xy1, xy2))
  )

  out <-  spatstat::crossdist(spatstat::as.ppp(xy1, window, check=FALSE),
                              spatstat::as.ppp(xy2, window, check=FALSE))

  dimnames(out) <- list(origin=id1, destination=id2)

  if (!is.null(id1)) out <- out[order(dimnames(out)$origin),]
  if (!is.null(id2)) out <- out[,order(dimnames(out)$destination)]

  out
}



##' Make data set of travel and stays
##'
##' This function builds a data set containing the number of individuals that remain in their home location (stay) or travel to another location
##' during the time span of the survey. The output is designed to provide data for the \code{\link{fit_prob_travel}} function. The admin unit used to aggregate the travel/stay
##' counts depend on the arguemtns supplied:
##' \enumerate{
##' \item When \code{data_pred} is supplied and \code{agg_adm = NULL}, the lowest admin unit of \code{data_pred} is used
##' \item When both \code{data_pred} and \code{agg_adm} are \code{NULL}, the lowest admin unit of \code{data} is used
##' }
##'
##' @param data generalized data frame described in \code{\link{travel_data_sim}} or derivative thereof
##' @param data_pred generalized data frame containing the admin units at which to predict probability of travel
##' @param agg_adm optional argument (logical) giving an arbitarary admin unit over which to aggregate travel/stay counts
##'
##' @return dataframe with same columns as \code{\link{travel_data_sim}} data with columns for counts of travel/stay/total
##'
##' @author John Giles
##'
##' @example R/examples/get_stay_data.R
##'
##' @family data synthesis
##' @family travel probability
##'
##' @importFrom magrittr %>%
##'
##' @export
##'

get_stay_data <- function(data,
                          data_pred=NULL,
                          agg_adm=NULL
) {

  ##suppressMessages(
  #  require(dplyr, quietly=TRUE)
  #)

  if (all(is.null(data_pred), is.null(agg_adm))) agg_adm <- get_admin_level(data)
  if (!is.null(data_pred) & is.null(agg_adm)) agg_adm <- get_admin_level(data_pred)

  ids <- c('orig_id', 'dest_id')

  if (any(ids %in% colnames(data))) {

    data[,ids] <- get_unique_ids(data, adm_stop=agg_adm)

  } else {

    data <- cbind(data, get_unique_ids(data, adm_stop=agg_adm))
    message('Added missing unique location names.')
  }

  sel <- data$orig_id == data$dest_id
  data_trip <- data[!sel,]
  data_stay <- data[sel,]

  if (!is.null(data_pred)) {

    if (any(ids %in% colnames(data_pred))) {

      data_pred[,ids] <- get_unique_ids(data_pred, adm_stop=agg_adm)

    } else {

      data_pred <- cbind(data_pred, get_unique_ids(data_pred, adm_stop=agg_adm))
    }
  }

  trip_count_method <- all(is.na(data$indiv_id)) | !('indiv_id' %in% colnames(data))

  if (trip_count_method) {

    message('Using total trip count method')
    out <- merge(
      data_stay %>%
        dplyr::group_by(orig_id) %>%
        dplyr::mutate(stay=sum(trips, na.rm=T)) %>%
        dplyr::distinct(orig_id, .keep_all=T) %>%
        dplyr::select(-trips) %>%
        data.frame(),
      data_trip %>%
        dplyr::group_by(orig_id) %>%
        dplyr::mutate(travel=sum(trips, na.rm=T)) %>%
        dplyr::distinct(orig_id, .keep_all=T) %>%
        dplyr::select(orig_id, travel) %>%
        data.frame(),
      by='orig_id',
      all.y=T
    )

  } else if (!trip_count_method) {

    message('Using individual count method')
    out <- merge(
      data_stay %>%
        dplyr::group_by(orig_id) %>%
        dplyr::mutate(stay=n_distinct(indiv_id, na.rm=T)) %>%
        dplyr::distinct(orig_id, .keep_all=T) %>%
        data.frame(),
      data_trip %>%
        dplyr::group_by(orig_id) %>%
        dplyr::mutate(travel=n_distinct(indiv_id, na.rm=T)) %>%
        dplyr::mutate(travel=ifelse(travel == 0, NA, travel)) %>%
        dplyr::distinct(orig_id, .keep_all=T) %>%
        dplyr::select(orig_id, travel) %>%
        data.frame(),
      by='orig_id',
      all.y=T
    )

    out$indiv_id <- out$indiv_age <- out$indiv_sex <- NA
  }

  out$total <- out$stay + out$travel

  # If prediction data supplied, aggregate to level of prediction and merge with prediction locations
  if (!is.null(data_pred)) {

    # check all stays in prediction data
    if (all(out$orig_id %in% data_pred$orig_id)) warning('Not all locations in stay data are present in prediction data')

    sel_row <- !(data_pred$orig_id %in% out$orig_id) # only admins not in V already
    sel_col <- which(apply(data_pred, 2, function(x) !all(is.na(x)))) # only cols with data
    out <- dplyr::full_join(out, data_pred[sel_row, sel_col])
    out <- out[,c(colnames(data_trip), c('stay', 'travel', 'total'))]
  }

  out
}

##' Find the lowest admin unit
##'
##' This function checks which admin levels are in a generalized a data frame formatting like \code{\link{travel_data_sim}} and
##' returns the lowest admin unit.
##'
##' @param data generalized data frame described in \code{\link{travel_data_template}} or derivative thereof
##'
##' @return integer
##'
##' @author John Giles
##'
##' @example R/examples/get_admin_level.R
##'
##' @family utility
##'
##' @export
##'

get_admin_level <- function(data) {

  admins <- grep('orig_adm', colnames(data))

  keep <- apply(
    data[,admins],
    2,
    function(x) all(!is.na(x))
  )

  admins <- admins[keep]
  admins <- sort(colnames(data)[admins])
  agg_adm <- admins[length(admins)]
  as.integer(substr(agg_adm, nchar(agg_adm), nchar(agg_adm)))
}



##' Get parameters of Beta distribution
##'
##' This function finds the two shape parameters for the Beta distribution of a random variable between 0 and 1.
##'
##' @param mu scalar or vector giving the mean of the proportion
##' @param sigma scalar or vector giving the standard deviation of the proportion
##'
##' @return A list containing the two shape parameters of the Beta distribution
##'
##' @author John Giles
##'
##' @example R/examples/get_beta_params.R
##'
##' @family utility
##'
##' @export
##'


get_beta_params <- function(
  mu,
  sigma
) {

  shape1 <- ((1-mu) / sigma - 1/mu) * mu^2
  list(shape1=shape1, shape2=shape1 * (1 / mu-1))
}



##' Get parameters of Gamma distribution
##'
##' A function that finds the \code{shape} and \code{rate} parameters required by the Gamma distribution given the observed
##' mean \code{mu} and standard deviation \code{sigma} of the response variable. Parameters are found numerically using a
##' two-dimensional Nelder-Mead optimization algorithm.
##'
##' @param mu the desired mean of the Gamma distribution
##' @param sigma the desired standard deviation of the Gamma distribution
##'
##' @return a named numeric vector giving the \code{shape} and \code{rate} parameters of the Gamma distribution
##'
##' @author John Giles
##'
##' @example R/examples/get_gamma_params.R
##'
##' @family utility
##'
##' @export
##'

get_gamma_params <- function(mu, sigma) {

  suppressWarnings(
    params <- optim(par=c(mu*2, 2),
                    fn=function(x) abs(mu - x[1]/x[2]) + abs(sigma - sqrt(x[1]/(x[2]^2))),
                    method='Nelder-Mead')$par
  )

  names(params) <- c('shape', 'rate')
  return(params)
}



##' Compare the goodness of fit of multiple mobility models
##'
##' Given two or more \code{mobility.model} objects, this function will parse out model information and goodness of fit metrics produced by the
##' \code{\link{check}} function and return them side by side for comparison.
##'
##' @param object a list of \code{mobility.model} objects

##' @return a dataframe where the rows contain basic model information along with goodness of fit metrics for each model
##'
##' @author John Giles
##'
##' @example R/examples/compare.R
##'
##' @family model
##'
##' @export
##'

compare <- function(object) {

  if (is.null(names(object))) names(object) <- stringr::str_c('model', seq_along(object))

  tmp <- lapply(object, function(x) {

    GOF <- mobility::check(x, plots=FALSE)

    data.frame(model=x$model,
               type=x$type,
               hierarchical=ifelse(is.null(x$hierarchical), NA, x$hierarchical),
               DIC=ifelse(is.null(GOF$DIC), NA, GOF$DIC),
               RMSE=ifelse(is.null(GOF$RMSE), NA, GOF$RMSE),
               MAPE=ifelse(is.null(GOF$MAPE), NA, GOF$MAPE),
               R2=ifelse(is.null(GOF$R2), NA, GOF$R2),
               row.names=FALSE)

  })

  return(do.call(rbind, tmp))
}
