# `mobility`: an R package for modeling human mobility patterns

This GitHub repository contains source code to build the `mobility` R package which provides tools for analyzing human mobility data, including functions for parsing travel data, fitting mobility models using Bayesian inference, and simulating mobility patterns. This package is currently in development and maintained by John Giles ([@gilesjohnr](https://github.com/gilesjohnr)) and Amy Wesolowski.

## Rationale
Recently there has been a surge in research that incorporates human mobility data into epidemiological models. Depending on the context, mobility data can be derived from multiple sources such as GPS tracking studies, travel surveys, call data records, or mobile phone applications with location services. In addition, widespread social distancing measures that have been implemented to slow COVID-19 transmission have also spurred several technology companies in the private sector to make data collected by mobile phone applications publicly available. A particular advantage lended by these data is that they offer real-time or near-real-time measures of human mobility at a high spatial resolution, leading to an increased interest in incorporating real-time data on human mobility into spatial models of disease transmission. Given the multitude of mobility data types now available and the impetus to use them in disease models, we developed the R package `mobility` which provides tools to help public health researchers fit and simulate mobility models using different sources of human mobility data. The package will undergo continued development as methods evolve to meet current demand.

## Challenges
Although the initial format of each data type may be different, there are general challenges regarding data sparsity, spatial scale, and statistical uncertainty that make direct use of mobility data in modeling pipelines challenging:

  * **Data sparsity** occurs when observations for some routes of travel are missing, making it difficult to generalize observed mobility patterns for routes of travel that are unobserved. When mobility data are sparse, a fitted mobility model can provide estimates of travel for unobserved routes. 

  * The **spatial scale** of the mobility data is often different from that needed by the epidemiological model. In this case a mobility model may need to aggregate travel patterns to a larger spatial scale or extrapolate them to a smaller one.
  
  * **Statistical uncertainty** in the travel patterns found in human mobility data results from the variance in a naturally stochastic process. Measurement error and observation bias also have an impact based on the method used to collect the mobility data and the subset of the population that are represented by it. One way to account for these sources of uncertainty is to fit a model to the data and simulate realizations of the process that vary according to the distribution of estimated parameters.


## Tools
To help facilitate modeling studies that wish to use mobility data and address the challenges above, we have developed a suite of tools that can be used to do some of the heavy lifting required to fit and simulate mobility models.


<img src='images/workflow/workflow.001.png'>



#### 1. Generalized mobility data format
There are many sources of travel data that researchers wish to fit models to. So, we have designed a generalized data frame template to standardize travel data from various sources into a longform format that is compatible with the modeling and simulation tools in this package. This data template can be populated by starting with the `travel_data_template` object and adding rows.
```r
library(mobility)
travel_data_template
```


#### 2. Build data matrices

#### 3. Fit a mobility model

#### 4. Simulate a mobility model

We have made vignettes in the package that explore each of these in more detail.

## Installation
To install the install the development version, first install the `devtools` package and then install `mobility` from source via the COVID-19-Mobility-Data-Network GitHub repository:
```r
install.packages('devtools')
devtools::install_github('COVID-19-Mobility-Data-Network/mobility')
```

The model fitting functions in the package perform parameter estimation using the Bayesian MCMC (Markov Chain Monte Carlo) algorithm called JAGS (Just Another Gibbs Sampler). The JAGS 4.3.0 library must be installed [here](http://mcmc-jags.sourceforge.net/). To check your installation of JAGS, you can open your console and type `jags` which should give you the following message.
```console
user@computer:~$ jags
Welcome to JAGS 4.3.0 on Fri May  1 16:05:10 2020
JAGS is free software and comes with ABSOLUTELY NO WARRANTY
Loading module: basemod: ok
Loading module: bugs: ok
. 
```



## Troubleshooting
For general questions, contact John Giles (giles@jhu.edu) and/or Amy Wesolowski (awesolowski@jhu.edu). For technical questions, contact package maintainer John Giles (giles@jhu.edu). To report bugs or problems with code or documentation, please go to the [Issues](https://github.com/COVID-19-Mobility-Data-Network/mobility/issues) page associated with this GitHub page and click *new issue*.

