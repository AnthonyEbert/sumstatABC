

#' ABC algorithm
#' @param prior function for prior
#' @param distance function for distance comp
#' @param transition function for transition
#' @param method algorithm to use
#' @param options list of options to use in algorithm
#' @examples
#' prior <- function(n){data.frame(mean = rnorm(n, 5))}
#' distance <- function(x){
#' data <- rnorm(1000, x)
#' output <- abs(mean(data) - 3)
#' }
#'
#' abc_post_1 <- abc_start(prior, distance, method = "rejection", parallel = FALSE, control = list(epsilon = 0.1))
#'
#' hist(abc_post_1$mean)
#'
#' prior <- function(n){data.frame(mean = runif(n, 2, 4), sd = rgamma(n, 1, 1))}
#' distance <- function(x){
#'     data <- rnorm(1000, x[1], x[2])
#'     output <- sqrt( (mean(data) - 3)^2 + (sd(data) - 1)^2)
#' }
#'
#' abc_post_2 <- abc_start(prior, distance, method = "rejection", parallel = FALSE, control = list(epsilon = 0.1))
#'
#' hist(abc_post_2$mean)
#' hist(abc_post_2$mean)
#'
#' @export
abc_start <- function(prior, distance, transition = NA, method = "rejection", control = list(), parallel = FALSE, cl = NULL){

  algorithm <- NA
  class(algorithm) <- method

  output <- abc_algorithm(prior, distance, transition, algorithm, control, parallel, cl)

  return(output)
}

abc_algorithm <- function(prior, distance, transition, algorithm, control, parallel, cl){
  UseMethod("abc_algorithm", algorithm)
}

abc_algorithm.rejection <- function(prior, distance, transition, algorithm, control, parallel, cl){

  control <- do.call("abc_control.rejection", control)
  lfunc <- make_lfunc(parallel)

  n <- control$n
  param <- prior(n)

  dist_col <- dim(param)[2] + 1
  output <- matrix(ncol = dist_col, nrow = 0)

  while(dim(output)[1] < control$n){

    param <- prior(n)

    new_output <- lfunc(as.matrix(param), 1, function(i, ...) {

      out <- distance(i)

      return(c(i, out))

    })

    #new_output <- matrix(unlist(new_output), ncol = dist_col, byrow = TRUE)
    new_output <- t(new_output)

    new_output <- new_output[which(new_output[, dist_col] <= control$epsilon),]

    output <- rbind(output, new_output)

    n <- (control$n - (dim(output)[1]))/(dim(new_output)[1] / n + control$delta)

    #print(dim(output)[1])
  }

  output <- as.data.frame(output)
  output <- output[sample.int(control$n), ]
  names(output) <- c(names(param), "distance")


  return(output[, - dist_col, drop = FALSE])

}


abc_control.rejection <- function(n = 1000, epsilon = 0.05, full_output = FALSE, delta = 1e-3){
  list(n = n, epsilon = epsilon, full_output = full_output, delta = delta)
}


make_lfunc <- function(parallel){
  output <- ifelse(!parallel,
         apply,
         function(X, MARGIN, FUN){
           return(parApply(cl, X, MARGIN, FUN))
         }
  )

  if(parallel){
    clusterExport(cl, "distance")
  }

  return(output)
}






