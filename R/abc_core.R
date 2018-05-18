

#' Start abc inference
#' @param prior function to sample from prior.
#' @param distance function to compute distance for proposal.
#' @param data some R object (anything) which is passed as an argument to the distance function. Usually used to pass the observed data to the distance function.
#' @param method a character string specifying the algorithm to use.
#' @param control list of options to use in algorithm.
#' @param output_control list of options for controlling output of algorithm.
#' @param cl an object of class "cluster".
#' @details
#' \code{abc_interface} takes a function for the prior and a function for the distance as arguments. The \code{prior} function must take a single integer \eqn{n} as the argument and return a dataframe with \eqn{n} rows, the number of columns represents the number of parameters in your system. The \code{distance} function must take a numeric vector of length equal to the number of parameters in your system and return a single positive number, the distance.
#'
#' Note that other things normally considered part of ABC inference such as: observed data, simulated data and summary statistics are not seen by the algorithm but are defined beforehand by the user. For instance the distance returned by the distance function would normally be considered to be a distance between observed and simulated summary statistics, rather than a mapping from the space of parameters. In the design of this package we have simply skipped the intermediary steps so that the interface for the user is as flexible as possible.
#'
#' The \code{transition} argument has yet to be implemented. The \code{method} argument specifies the ABC algorithm to use to infer parameters, current options include \code{"rejection"} and \code{"RABC"}. The \code{control} argument takes a list of optional input values to be used in the ABC algorithm specified. The \code{output_control} argument takes a list of option input values to control how the output is returned to the user.
#'
#' The function is parallelised in much the same way as \code{\link{parLapply}} for instance. The user creates a cluster object \code{cl} with the \code{\link{makeCluster}} command and inputs \code{cl} as an argument to the function. The same troubleshooting procedures can be used as with \code{\link{parLapply}}, for instance if a node does not have access to objects in your environment use \code{\link{clusterExport}}.
#'
#' @examples
#'
#' observed_data <- rnorm(1000, 3)
#'
#' prior <- function(n){
#'   data.frame(mean = rnorm(n, 5))
#' }
#'
#' distance <- function(theta, data){
#'   sim <- rnorm(1000, theta)
#'   output <- abs(mean(sim) - mean(data))
#'   return(output)
#' }
#'
#' abc_post_1 <- abc_start(
#'   prior,
#'   distance,
#'   data = observed_data,
#'   method = "rejection",
#'   control = list(epsilon = 0.1)
#' )
#'
#' hist(abc_post_1$mean)
#'
#' prior <- function(n){
#'   data.frame(
#'     mean = runif(n, 2, 4),
#'     sd = rgamma(n, 1, 1)
#'   )
#' }
#'
#' distance <- function(theta, data){
#'   sim <- rnorm(1000, theta["mean"], theta["sd"])
#'   output <- sqrt( (mean(sim) - mean(data))^2 + (sd(sim) - sd(data))^2)
#'   return(output)
#' }
#'
#' abc_post_2 <- abc_start(
#'   prior,
#'   distance,
#'   data = observed_data,
#'   method = "rejection",
#'   control = list(epsilon = 0.1)
#' )
#'
#' hist(abc_post_2$mean)
#' hist(abc_post_2$sd)
#'
#' abc_post_3 <- abc_start(
#'   prior,
#'   distance,
#'   data = observed_data,
#'   method = "RABC",
#'   control = list(prior_eval = function(theta){return(ifelse(theta["mean"] < 2 | theta["mean"] > 4 | theta["sd"] <= 0, 0, 1))})
#' )
#'
#' hist(abc_post_3$mean)
#' hist(abc_post_3$sd)
#' @export
abc_start <- function(prior, distance, data = list(), method = "rejection", control = list(), output_control = list(), cl = list()){

  algorithm <- NA
  class(algorithm) <- method

  output <- abc_algorithm(prior, distance, data, algorithm, control, output_control, cl)

  return(output)
}


make_lfunc <- function(parallel){
  output <- ifelse(!parallel,
         apply,
         function(X, MARGIN, FUN, ...){
           return(parallel::parApply(cl, X, MARGIN, FUN, ...))
         }
  )

  # if(parallel){
  #   parallel::clusterExport(cl, "distance")
  # }

  return(output)
}






