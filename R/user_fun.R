

## User friendly functions

#' Make a uniform prior
#' @param lhs_lim vector
#' @param rhs_lim vector
#' @param var_names optional character vector of parameter names
#' @param eval boolean
#' @importFrom magrittr %>%
#' @export
prior_unif <- function(lhs_lim, rhs_lim, var_names = NULL, eval = FALSE){

  stopifnot(is.numeric(lhs_lim) && is.numeric(rhs_lim))
  stopifnot(length(lhs_lim) == length(rhs_lim))
  if(!is.null(var_names)){stopifnot(length(var_names) == length(lhs_lim))}

  if(eval){
    output <- function(x){
      stopifnot(length(x) == length(lhs_lim))

      return(ifelse(any(x < lhs_lim) | any(x > rhs_lim), 0, 1))
    }
  } else {
    output <- function(n){
      stopifnot(n %% 1 == 0 && n > 0)

      out <- stats::runif(length(lhs_lim) * n, lhs_lim, rhs_lim) %>%
        matrix(ncol = length(lhs_lim), byrow = TRUE) %>%
        as.data.frame()

      if(!is.null(var_names)){names(out) <- var_names}
      return(out)
    }
  }

  return(output)
}
