## ----setup, include=FALSE-----------------------------------------------------
library(mobility)
library(ggplot2)
library(reshape2)
library(viridis)
library(ggstance)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  autodep=TRUE
)

## ----example IDs--------------------------------------------------------------
travel_data <- travel_data_sim

# Add unique identifiers using Country, State, and County
example_ids <- get_unique_ids(travel_data, adm_start=0, adm_stop=2)
head(example_ids)

# Assume all County names here are unique, we can use only County level as ID.
example_ids <- get_unique_ids(travel_data, adm_start=2)
head(example_ids)

## ----build M------------------------------------------------------------------
# Add unique identifiers using County
travel_data <- cbind(travel_data, get_unique_ids(travel_data, adm_start=2))

# Build mobility matrix from the longform data
M <- get_mob_matrix(orig=travel_data$orig_id,
                    dest=travel_data$dest_id,
                    value=travel_data$trips)

## ----plot M, fig.height=7, fig.width=7, echo=FALSE----------------------------
ggplot(data=melt(M)) +
  geom_tile(aes(x=factor(destination),
                y=factor(origin),
                fill=value)) +
  xlab('Destination') + ylab("Origin") +
  theme_bw() + theme(axis.text.x=element_text(size=10),
                     axis.text.y=element_text(size=10),
                     axis.title.x=element_text(size=12, margin = margin(t = 15)),
                     axis.title.y=element_text(size=12, margin = margin(r = 15)),
                     legend.position='bottom') +
  viridis::scale_fill_viridis(option='inferno', direction=1) +
  guides(fill=guide_colorbar(title='Observed trips',
                             title.position='top',
                             label.theme=element_text(size=9),
                             barwidth=20,
                             barheight=0.5,
                             frame.colour='black',
                             ticks=TRUE))

## -----------------------------------------------------------------------------
xy <- get_unique_coords(travel_data)

D <- get_distance_matrix(x=xy[,1],
                         y=xy[,2],
                         id=xy[,3])

D <- D*111.35 # decimal degrees to km
D[1:5, 1:5]

## -----------------------------------------------------------------------------
N <- get_pop_vec(travel_data)
head(N)

## -----------------------------------------------------------------------------
# check
all(
     sapply(list(dim(D)[1], length(N)), FUN = identical, dim(M)[1]),
     sapply(list(dimnames(D)$origin, names(N)), FUN = identical, dimnames(M)$origin)
)

