
library(protoABC)
set.seed(1)

context("RABC")

observed_data <- rnorm(100, 3)

prior <- function(n){
  data.frame(mean = rnorm(n, 5))
}

distance <- function(theta, distance_args){
  sim <- rnorm(100, theta)
  output <- abs(mean(sim) - mean(distance_args))
  return(output)
}

# ABC with mean parameter ---------------

RABC_1 <- abc_start(
  prior,
  distance,
  distance_args = observed_data,
  method = "RABC",
  control = list(pacc_final = 0.1, n = 100)
)

# test_that("RABC_1", {
#   expect_known_value(RABC_1, file = "RABC_1.RData", print = TRUE, update = FALSE)
# })

test_that("RABC_1_close", {
  expect_lte(abs(mean(RABC_1$mean) -3), 1)
})
