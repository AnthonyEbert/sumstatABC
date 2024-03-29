

#' Start abc inference
#' @param prior function to sample from prior.
#' @param distance function to compute distance for proposal.
#' @param distance_args some R object (anything) which is passed as an argument to the distance function. Usually used to pass the observed data to the distance function.
#' @param method a character string specifying the algorithm to use.
#' @param control list of options to use in algorithm.
#' @param output_control list of options for controlling output of algorithm.
#' @param cl an object of class "cluster" to use \code{parApply} internally or the string "mclapply" to use \code{mclapply} internally. A NULL input (the default) will use \code{apply} internally, a single core.
#' @details
#' \code{abc_start} takes a function for the prior and a function for the distance as arguments. The \code{prior} function must take a single integer \eqn{n} as the argument and return a dataframe with \eqn{n} rows, the number of columns represents the number of parameters in your system. The \code{distance} function must take a numeric vector of length equal to the number of parameters in your system and return a single positive number, the distance.
#'
#' Note that other things normally considered part of ABC inference such as: observed data, simulated data and summary statistics are not seen by the algorithm but are defined beforehand by the user. For instance the distance returned by the distance function would normally be considered to be a distance between observed and simulated summary statistics, rather than a mapping from the space of parameters. In the design of this package we have simply skipped the intermediary steps so that the interface for the user is as flexible as possible.
#'
#' The \code{method} argument specifies the ABC algorithm to use to infer parameters, current options include \code{"rejection"} and \code{"RABC"}. The \code{control} argument takes a list of optional input values to be used in the ABC algorithm specified. The \code{output_control} argument takes a list of option input values to control how the output is returned to the user.
#'
#' The function can be parallelised in two ways. The first much the same way as \code{\link{parLapply}}. The user creates a cluster object \code{cl} with the \code{\link{makeCluster}} command and inputs \code{cl} as an argument to the function. The same troubleshooting procedures can be used as with \code{\link{parLapply}}, for instance if a node does not have access to objects in your environment use \code{\link{clusterExport}}. Alternatively if the character string "mclapply" is used as input to \code{cl} then \code{\link{mclapply}} will be used internally.
#'
#' @examples
#'
#'sample <- rnorm(1000, 3, 1.5)
#'
#'inp <- list(
#'  sample_mean = mean(sample),
#'  sample_sd   = sd(sample)
#')
#'
#' prior <- function(n){
#'   data.frame(mean = rnorm(n, 5))
#' }
#'
#' distance <- function(theta, inp){
#'   sim <- rnorm(1000, theta)
#'   output <- abs(mean(sim) - inp$sample_mean)
#'   return(output)
#' }
#'
#' # ABC with mean parameter ---------------
#'
#' abc_post_1 <- abc_start(
#'   prior,
#'   distance,
#'   distance_args = inp,
#'   method = "rejection",
#'   control = list(epsilon = 0.2, n = 10)
#' )
#'
#' hist(abc_post_1$mean)
#'
#' # ABC with mean and sd parameters ---------
#'
#' ## Rejection ABC
#'
#' prior <- function(n){
#'   data.frame(
#'     mean = runif(n, 2, 4),
#'     sd = rgamma(n, 1, 1)
#'   )
#' }
#'
#' distance <- function(theta, inp){
#'   sim <- rnorm(1000, theta["mean"], theta["sd"])
#'   output <- sqrt( (mean(sim) - inp$sample_mean)^2 + (sd(sim) - inp$sample_sd)^2)
#'   return(output)
#' }
#'
#' abc_post_2 <- abc_start(
#'   prior,
#'   distance,
#'   distance_args = inp,
#'   method = "rejection",
#'   control = list(epsilon = 0.45, n = 10)
#' )
#'
#' hist(abc_post_2$mean)
#' hist(abc_post_2$sd)
#'
#' ## Replenishment ABC
#'
#' prior_eval = function(theta){
#'   return(
#'     ifelse(
#'       theta[["mean"]] < 2 |
#'         theta[["mean"]] > 4 |
#'         theta[["sd"]] <= 0, 0, 1*dgamma(theta["sd"], 1, 1)
#'       )
#'   )
#' }
#'
#' abc_post_3 <- abc_start(
#'   prior,
#'   distance,
#'   distance_args = inp,
#'   method = "RABC",
#'   control = list(prior_eval = prior_eval, n = 10, pacc_final = 0.05)
#' )
#'
#' hist(abc_post_3$mean)
#' hist(abc_post_3$sd)
#'
#' # ABC in parallel! ---------------
#'
#' \dontrun{
#' library(parallel)
#' cl <- makeCluster(detectCores())
#'
#' abc_post_4 <- abc_start(
#'   prior,
#'   distance,
#'   distance_args = inp,
#'   cl = cl,
#'   control = list(n = 100)
#' )
#'
#' }
#'
#'
#' @export
abc_start <- function(prior, distance, distance_args = NULL, method = "rejection", control = list(), output_control = list(), cl = NULL){

  algorithm <- NA
  class(algorithm) <- method

  parallel <- ifelse(is.null(cl), 1,
                     ifelse("cluster" %in% class(cl), 2,
                            ifelse(cl == "mclapply", 3,
                                   ifelse(cl == "test", 4, 5)))
  )

  lfunc <- make_lfunc(parallel, cl)

  if(is.null(distance_args)){
    distance <- function(i, distance_args){distance(i)}
  }

  output <- abc_algorithm(prior, distance, distance_args, algorithm, control, output_control, lfunc)

  return(output)
}


make_lfunc <- function(parallel, cl){
  output <- switch(parallel,
         apply,
         function(X, MARGIN, FUN, ...){
           return(parallel::parApply(cl, X, MARGIN, FUN, ...))
         },
         function(X, MARGIN, FUN, ...){
           Y <- split(t(X), rep(1:ncol(t(X)), each = nrow(t(X))))
           Y <- lapply(Y, function(i, names_x){
             names(i) <- names_x
             return(i)
            }, names_x = colnames(X))

           output <- parallel::mclapply(Y, FUN, ...)
           output_mat <- matrix(unlist(output), length(output[[1]]))
           rownames(output_mat) <- names(output[[1]])

           return(output_mat)
         },
         function(X, MARGIN, FUN, ...){
           Y <- split(t(X), rep(1:ncol(t(X)), each = nrow(t(X))))
           Y <- lapply(Y, function(i, names_x){
             names(i) <- names_x
             return(i)
           }, names_x = colnames(X))

           output <- lapply(Y, FUN, ...)
           output_mat <- matrix(unlist(output), length(output[[1]]))
           rownames(output_mat) <- names(output[[1]])

           return(output_mat)
         }
  )

  return(output)
}






