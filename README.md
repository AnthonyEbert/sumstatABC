
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/protoABC)](https://CRAN.R-project.org/package=protoABC) [![Build Status](https://travis-ci.org/AnthonyEbert/protoABC.svg)](https://travis-ci.org/AnthonyEbert/protoABC) [![codecov](https://codecov.io/gh/AnthonyEbert/protoABC/branch/master/graph/badge.svg)](https://codecov.io/gh/AnthonyEbert/protoABC)

<!-- README.md is generated from README.Rmd. Please edit that file -->
protoABC
========

The goal of protoABC is to provide a way to perform ABC (approximate Bayesian computation) inference as flexibly as possible. That is with arbitrarily complex simulation algorithms and distance functions.

The way we implement this is to consider the distance as the output of the simulation and leave the internal details to the user. Parameters in, distance out.

You need to supply a function which returns a positive number (the distance) with up to two arguments, the first is a vector of your parameters of interest, the second is a list of additional inputs to your function.

Installation
------------

You can install protoABC from github with:

``` r
# install.packages("devtools")
devtools::install_github("AnthonyEbert/protoABC")
```

Example
-------

The simplest example, a normal distribution. The summary statistic is the sample mean.

``` r
library(protoABC)

sample <- rnorm(1000, 3, 1.5)

inp <- list(
  sample_mean = mean(sample), 
  sample_sd   = sd(sample)
)

prior <- function(n){data.frame(mu = rnorm(n, 5))}

distance <- function(theta, inp){
  sim <- rnorm(1000, theta)
  output <- abs(mean(sim) - inp$sample_mean)
  return(output)
}

abc_post_1 <- abc_start(
  prior,
  distance,
  inp,
  method = "rejection",
  control = list(epsilon = 0.1)
)

summary(abc_post_1)
#>        mu       
#>  Min.   :2.865  
#>  1st Qu.:2.988  
#>  Median :3.041  
#>  Mean   :3.037  
#>  3rd Qu.:3.089  
#>  Max.   :3.179
```

The simplest example, a normal distribution. The summary statistics are the sample mean and the standard deviation.

``` r

prior <- function(n){
  data.frame(mu = runif(n, 2, 4), sd = rgamma(n, 1, 1))
}

prior_eval <- function(theta){
  prior_value <- dunif(theta["mu"], 2, 4) * dgamma(theta["sd"], 1, 1)
  
  return(prior_value)
}


distance <- function(theta, inp){
  
  sim <- rnorm(1000, theta["mu"], theta["sd"])
  
  output <- sqrt( (mean(sim) - inp$sample_mean)^2 + (sd(sim) - inp$sample_sd)^2)
  return(output)
}

abc_post_2 <- abc_start(
  prior,
  distance,
  inp,
  method = "RABC",
  control = list(n = 1000, prior_eval = prior_eval, pacc_final = 0.1), 
  output_control = list(print_output = FALSE)
)

## Summary

summary(abc_post_2)
#>        mu              sd       
#>  Min.   :2.887   Min.   :1.361  
#>  1st Qu.:2.993   1st Qu.:1.428  
#>  Median :3.026   Median :1.449  
#>  Mean   :3.027   Mean   :1.451  
#>  3rd Qu.:3.059   3rd Qu.:1.474  
#>  Max.   :3.156   Max.   :1.582
```
