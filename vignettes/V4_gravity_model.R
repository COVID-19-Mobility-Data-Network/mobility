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

## ----data---------------------------------------------------------------------
M <- mobility_matrices$M
D <- mobility_matrices$D
N <- mobility_matrices$N

## ----data_plot, fig.height=6.5, fig.width=6.5, echo=FALSE---------------------
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

## ----fit----------------------------------------------------------------------
mod <- fit_gravity(M, D, N, DIC=TRUE)

## ----chains-------------------------------------------------------------------
str(mod)

## ----summary------------------------------------------------------------------
mod <- summarize_mobility(mod)
mod

## ----check, fig.height=4.5, fig.width=7.25------------------------------------
check_mobility(M, D, N, mod)

## ----sim----------------------------------------------------------------------
pi_hat <- sim_gravity(N=N,
                      D=D,
                      theta=mod['theta', 'Mean'],
                      omega_1=mod['omega_1', 'Mean'],
                      omega_2=mod['omega_2', 'Mean'],
                      gamma=mod['gamma', 'Mean'])

## ----sim_plot, fig.height=6.5, fig.width=6.5, echo=FALSE----------------------
diag(pi_hat) <- NA

ggplot(data=melt(pi_hat)) +
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
  guides(fill=guide_colorbar(title='Observed proportion of trips',
                             title.position='top',
                             label.theme=element_text(size=9),
                             barwidth=20,
                             barheight=0.5,
                             frame.colour='black',
                             ticks=TRUE))

