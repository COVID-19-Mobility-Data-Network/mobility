## ----setup, include=FALSE-----------------------------------------------------
library(mobility)
library(ggplot2)
library(reshape2)
library(viridis)
library(ggstance)
library(foreach)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  autodep=TRUE
)

set.seed(1234)

## ----data---------------------------------------------------------------------
stay_data <- get_stay_data(travel_data_sim, agg_adm=2)
Y <- setNames(stay_data$travel, stay_data$orig_id)
N <- setNames(stay_data$total, stay_data$orig_id)

n_orig <- length(Y)
miss <- sample(1:n_orig, n_orig*0.2) # missing observations
Y[miss] <- N[miss] <- NA

Y
N

## ----fit1---------------------------------------------------------------------
prob_trav <- summarize_mobility(
  fit_prob_travel(travel=Y,
                  total=N)
)

## ---- echo=FALSE, fig.width=5, fig.height=7-----------------------------------
ggplot(data=prob_trav) +
  geom_point(aes(x=Mean, y=rownames(prob_trav)), size=2) +
  ggstance::geom_linerangeh(aes(y=rownames(prob_trav), xmin=HPD2.5, xmax=HPD97.5)) +
  xlim(0,1) +
  xlab('Probability of travel outside origin') + ylab('Origin') +
  theme_bw() + theme(axis.text.x=element_text(size=9),
                     axis.text.y=element_text(size=9),
                     axis.title.x=element_text(size=12, margin = margin(t = 15)),
                     axis.title.y=element_text(size=12, margin = margin(r = 15)),
                     legend.position='right')

## -----------------------------------------------------------------------------
# Simulate n realizations of travel probability for each location
sim_prob_travel(mu=prob_trav$Mean,
                sigma=prob_trav$SD,
                id=names(Y),
                n=5)

