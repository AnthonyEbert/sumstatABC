
library(protoABC)
set.seed(1)

context("RABC 2")

prior <- prior_unif(c(0, 0.5), c(1, 1), var_names = c("mu", "sd"))
prior_eval <- prior_unif(c(0, 0.5), c(1, 1), var_names = c("mu", "sd"), eval = TRUE)

sample <- rnorm(1000, mean = 0.2, sd = 0.7)
inp <- list(
  sample_mean = mean(sample),
  sample_sd = sd(sample)
)


distance <- function(theta, inp){

  sim <- rnorm(1000, theta["mu"], theta["sd"])

  output <- sqrt( (mean(sim) - inp$sample_mean)^2 + (sd(sim) - inp$sample_sd)^2)
  return(output)
}

abc_RABC_2 <- abc_start(
  prior,
  distance,
  inp,
  method = "RABC",
  control = list(n = 20, prior_eval = prior_eval, pacc_final = 0.1),
  output_control = list(print_output = FALSE, include_dist = TRUE),
  cl = "test"
)

summary(abc_RABC_2)
