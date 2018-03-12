

#' ABC algorithm
#' @param prior function for prior
#' @param distance function for distance comp
#' @param transition function for transition
#' @param method algorithm to use
#' @param options list of options to use in algorithm
#' @examples
#' prior <- function(n){data.frame(mean = rnorm(n, 5))}
#' distance <- function(x){
#'   data <- rnorm(1000, x)
#'   output <- abs(mean(data) - 3)
#'   return(output)
#' }
#'
#' abc_post_1 <- abc_start(prior, distance, method = "rejection", parallel = FALSE, control = list(epsilon = 0.1))
#'
#' hist(abc_post_1$mean)
#'
#' prior <- function(n){data.frame(mean = runif(n, 2, 4), sd = rgamma(n, 1, 1))}
#' distance <- function(x){
#'   data <- rnorm(1000, x[1], x[2])
#'   output <- sqrt( (mean(data) - 3)^2 + (sd(data) - 1)^2)
#'   return(output)
#' }
#'
#' abc_post_2 <- abc_start(prior, distance, method = "rejection", parallel = FALSE, control = list(epsilon = 0.1))
#'
#' hist(abc_post_2$mean)
#' hist(abc_post_2$mean)
#'
#' abc_post_2 <- abc_start(prior, distance, method = "RABC", parallel = FALSE, control = list(prior_eval = function(x){return(ifelse(x[1] < 2 | x[1] > 4 | x[2] <= 0, 0, 1))}))
#'
#' @export
abc_start <- function(prior, distance, transition = NA, method = "rejection", control = list(), parallel = FALSE, cl = NULL){

  algorithm <- NA
  class(algorithm) <- method

  output <- abc_algorithm(prior, distance, transition, algorithm, control, parallel, cl)

  return(output)
}


make_lfunc <- function(parallel){
  output <- ifelse(!parallel,
         apply,
         function(X, MARGIN, FUN, ...){
           return(parallel::parApply(cl, X, MARGIN, FUN, ...))
         }
  )

  if(parallel){
    parallel::clusterExport(cl, "distance")
  }

  return(output)
}






