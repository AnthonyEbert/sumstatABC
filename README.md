
<!-- README.md is generated from README.Rmd. Please edit that file -->
protoABC
========

The goal of protoABC is to provide an interface to ABC inference which is as flexible as possible.

Installation
------------

You can install protoABC from github with:

``` r
# install.packages("devtools")
devtools::install_github("AnthonyEbert/protoABC", auth_token = "0f7acf8a9c7faa1c678ce5fd8afb195badbca24b")
```

Example
-------

``` r

library(protoABC)

# Infer mean parameter for normal distribution from some observed data

data <- rnorm(1000, 3, 1.5)

summary(data)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  -1.628   1.997   3.031   2.985   4.026   7.700

prior <- function(n){data.frame(mu = rnorm(n, 5))}

distance <- function(theta, data){
  sim <- rnorm(1000, theta)
  output <- abs(mean(sim) - mean(data))
  return(output)
}

abc_post_1 <- abc_start(
  prior,
  distance,
  data,
  method = "rejection",
  control = list(epsilon = 0.1)
)

summary(abc_post_1)
#>        mu       
#>  Min.   :2.825  
#>  1st Qu.:2.946  
#>  Median :2.997  
#>  Mean   :2.996  
#>  3rd Qu.:3.048  
#>  Max.   :3.153
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


distance <- function(theta, data){
  sim <- rnorm(1000, theta["mu"], theta["sd"])
  output <- sqrt( (mean(sim) - mean(data))^2 + (sd(sim) - sd(data))^2)
  return(output)
}

abc_post_2 <- abc_start(
  prior,
  distance,
  data,
  method = "RABC",
  control = list(n = 1000, prior_eval = prior_eval, pacc_final = 0.1), 
  output_control = list(print_output = FALSE)
)

summary(abc_post_2)
#>        mu              sd       
#>  Min.   :2.837   Min.   :1.419  
#>  1st Qu.:2.951   1st Qu.:1.487  
#>  Median :2.984   Median :1.512  
#>  Mean   :2.985   Mean   :1.513  
#>  3rd Qu.:3.020   3rd Qu.:1.537  
#>  Max.   :3.109   Max.   :1.632
```
