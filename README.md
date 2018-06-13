
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
#>  -1.947   2.066   3.013   3.007   3.991   7.736

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
#>  Min.   :2.833  
#>  1st Qu.:2.963  
#>  Median :3.016  
#>  Mean   :3.013  
#>  3rd Qu.:3.066  
#>  Max.   :3.159
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
#>  Min.   :2.870   Min.   :1.391  
#>  1st Qu.:2.974   1st Qu.:1.468  
#>  Median :3.007   Median :1.492  
#>  Mean   :3.004   Mean   :1.493  
#>  3rd Qu.:3.038   3rd Qu.:1.515  
#>  Max.   :3.140   Max.   :1.599
```
