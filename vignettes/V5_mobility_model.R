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

## ----fit_prob_travel----------------------------------------------------------
total <- rowSums(M, na.rm=TRUE)
prob_trav <- summarize_mobility(
  fit_prob_travel(travel=total-diag(M), 
                  total=total)
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

## ----fit mobility-------------------------------------------------------------
mod <- summarize_mobility(
  fit_mobility(M, D, N),
  ac_lags=5
)
mod

## ----check mobility, fig.height=4, fig.width=7--------------------------------
check_mobility(M=M,
               D=D,N=N,
               mod=mod)

## ----sim mobility point-------------------------------------------------------
mod_sim <- sim_mobility(D,
                        N,
                        theta = mod['theta','Mean'],
                        omega_1 = mod['omega_1','Mean'],
                        omega_2 = mod['omega_2','Mean'],
                        gamma = mod['gamma','Mean'],
                        tau = mod[grep('tau', rownames(mod)),'Mean'])
mod_sim[1:5,1:5]

## ----sim mobility stoch-------------------------------------------------------
mod_sim <- sim_mobility(D,
                        N,
                        mod=mod,
                        n=3)
mod_sim[1:5,1:5,]

