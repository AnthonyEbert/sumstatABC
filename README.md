
<!-- README.md is generated from README.Rmd. Please edit that file -->
protoABC
========

The goal of protoABC is to provide an interface to ABC inference which is as flexible as possible.

Installation
------------

You can install protoABC from github with:

``` r
# install.packages("devtools")
devtools::install_github("AnthonyEbert/protoABC")
```

Example
-------

``` r

library(protoABC)

# Infer mean parameter for normal distribution from some observed data

distance_args <- rnorm(1000, 3, 1.5)

summary(distance_args)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  -1.957   1.967   3.009   3.009   4.029   7.594

prior <- function(n){data.frame(mu = rnorm(n, 5))}

distance <- function(theta, distance_args){
  sim <- rnorm(1000, theta)
  output <- abs(mean(sim) - mean(distance_args))
  return(output)
}

abc_post_1 <- abc_start(
  prior,
  distance,
  distance_args,
  method = "rejection",
  control = list(epsilon = 0.1)
)

summary(abc_post_1)
#>        mu       
#>  Min.   :2.859  
#>  1st Qu.:2.969  
#>  Median :3.022  
#>  Mean   :3.021  
#>  3rd Qu.:3.074  
#>  Max.   :3.192
```

``` r
# Infer mean and standard deviation for normal distribution given some observed data

prior <- function(n){
  data.frame(mu = runif(n, 2, 4), sd = rgamma(n, 1, 1))
}

prior_eval <- function(theta){
  prior_value <- dunif(theta["mu"], 2, 4) * dgamma(theta["sd"], 1, 1)
  
  return(prior_value)
}


distance <- function(theta, distance_args){
  sim <- rnorm(1000, theta["mu"], theta["sd"])
  output <- sqrt( (mean(sim) - mean(distance_args))^2 + (sd(sim) - sd(distance_args))^2)
  return(output)
}

abc_post_2 <- abc_start(
  prior,
  distance,
  distance_args,
  method = "RABC",
  control = list(n = 1000, prior_eval = prior_eval, pacc_final = 0.1), 
  output_control = list(print_output = FALSE)
)

## Summary

summary(abc_post_2)
#>        mu              sd       
#>  Min.   :2.871   Min.   :1.374  
#>  1st Qu.:2.976   1st Qu.:1.449  
#>  Median :3.009   Median :1.473  
#>  Mean   :3.008   Mean   :1.473  
#>  3rd Qu.:3.041   3rd Qu.:1.496  
#>  Max.   :3.146   Max.   :1.563
```
