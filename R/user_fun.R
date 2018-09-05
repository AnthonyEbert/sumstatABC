

## User friendly functions

#' Make a uniform prior
#' @param lhs_lim vector of lower bounds for uniform prior
#' @param rhs_lim vector of upper bounds for uniform prior
#' @param var_names optional character vector of parameter names
#' @param eval boolean if FALSE outputs a prior sampler function (similar to \code{runif}), otherwise outputs an evaluation function (similar to \code{dunif}).
#' @examples
#' prior <- prior_unif(c(0, 0.5), c(1, 1), var_names = c("mu", "sd"))
#' prior_eval <- prior_unif(c(0, 0.5), c(1, 1), var_names = c("mu", "sd"), eval = TRUE)
#'
#' sample <- rnorm(1000, mean = 0.2, sd = 0.7)
#' inp <- list(
#'     sample_mean = mean(sample),
#'     sample_sd = sd(sample)
#' )
#'
#'
#' distance <- function(theta, inp){
#'
#'   sim <- rnorm(1000, theta["mu"], theta["sd"])
#'
#'   output <- sqrt( (mean(sim) - inp$sample_mean)^2 + (sd(sim) - inp$sample_sd)^2)
#'   return(output)
#' }
#'
#' abc_post <- abc_start(
#'   prior,
#'   distance,
#'   inp,
#'   method = "RABC",
#'   control = list(n = 20, prior_eval = prior_eval, pacc_final = 0.1),
#'   output_control = list(print_output = FALSE)
#' )
#' @export
prior_unif <- function(lhs_lim, rhs_lim, var_names = NULL, eval = FALSE){

  stopifnot(is.numeric(lhs_lim) && is.numeric(rhs_lim))
  stopifnot(length(lhs_lim) == length(rhs_lim))
  if(!is.null(var_names)){stopifnot(length(var_names) == length(lhs_lim))}

  if(eval){
    output <- function(n){
      stopifnot(length(n) == length(lhs_lim))

      return(ifelse(any(n < lhs_lim) | any(n > rhs_lim), 0, 1))
    }
  } else {
    output <- function(n){
      stopifnot(n %% 1 == 0 && n > 0)

      out <-
        as.data.frame(
          matrix(
            stats::runif(length(lhs_lim) * n, lhs_lim, rhs_lim),
            ncol = length(lhs_lim), byrow = TRUE
          )
        )

      if(!is.null(var_names)){names(out) <- var_names}
      return(out)
    }
  }

  return(output)
}
