

abc_algorithm <- function(prior, distance, transition, algorithm, control, parallel, cl){
  UseMethod("abc_algorithm", algorithm)
}

# Rejection

abc_algorithm.rejection <- function(prior, distance, transition, algorithm, control, parallel, cl){

  param <- prior(n)

  control <- do.call("abc_control.rejection", control)
  lfunc <- make_lfunc(parallel)

  n <- control$n


  dist_col <- dim(param)[2] + 1
  output <- matrix(ncol = dist_col, nrow = 0)

  while(dim(output)[1] < control$n){

    new_output <- rejection_core(n, prior, distance, lfunc)

    #new_output <- matrix(unlist(new_output), ncol = dist_col, byrow = TRUE)

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

rejection_core <- function(n, prior, distance, lfunc){
  param <- prior(n)

  new_output <- lfunc(as.matrix(param), 1, function(i, ...) {

    out <- distance(i)

    return(c(i, out))

  })

  return(t(new_output))
}


# RABC

abc_control.RABC <- function(n = 1000, a = 0.5, c1 = 0.01, R = 10, eps_final = 0, pacc_final = 0.02, num_drop = n * a, num_keep = n - num_drop, n_param, cov_func = ifelse(n_param == 1, var, cov), prior_eval = function(x,y){return(1)}){
  list(n = n, a = a, c1 = c1, R = R, eps_final = eps_final, pacc_final = pacc_final, num_drop = num_drop, num_keep = num_keep, n_param = n_param, cov_func = cov_func, prior_eval = prior_eval)
}

abc_algorithm.RABC <- function(prior, distance, transition, algorithm, control, parallel, cl){

  param <- prior(1)
  control$n_param <- dim(param)[2]
  control <- do.call("abc_control.RABC", control)

  lfunc <- make_lfunc(parallel)



  if(identical(control$cov_func, cov) && dim(param)[2] == 1){
    print("Changing control$cov_func to var because problem is 1D")
    control$cov_func <- var
  }

  dist_col <- dim(param)[2] + 1
  output <- matrix(ncol = dist_col, nrow = 0)

  trial_run <- rejection_core(control$n, prior, distance, lfunc)

  input_params_s <- trial_run[order(trial_run[,dist_col]), ]

  dist_next <- input_params_s[control$num_keep, dist_col]
  dist_max <- input_params_s[control$n, dist_col]

  R = control$R

  while(dist_max > control$eps_final){

    rw_cov <- control$cov_func(input_params_s[1:control$num_keep, -dist_col])

    iacc <- 0

    index_resample_f <- sample(control$num_keep, control$n - control$num_keep, replace = TRUE)

    output <- lfunc(
      input_params_s[index_resample_f, ],
      1,
      RABC_core,
      dist_next = dist_next,
      num_keep = control$num_keep,
      R = R,
      rw_cov = rw_cov,
      distance = distance,
      prior_eval = control$prior_eval
    )

    output <- t(output)

    input_params_s[(control$num_keep + 1):control$n, ] <- output[,-dim(output)[2]]
    iacc <- sum(output[,dim(output)[2]])

    p_acc <- iacc / (control$num_drop * R)
    R = floor(log(control$c1) / log(1 - p_acc) + 1)

    if (p_acc < control$pacc_final){
      break;
    }

    print("********************************");

    cat("Aceptance prob of MCMC was ",p_acc,"\n");
    cat("Number of MCMC moves for next iteration is ",R,"\n");
    cat("number of unique particles is ", length(unique(input_params_s[,1])),"\n")

    # order the particles according to the distance
    input_params_s <- input_params_s[order(input_params_s[,dist_col]), ]

    dist_next <- input_params_s[control$num_keep, dist_col]
    dist_max <- input_params_s[control$n, dist_col]

    cat("dist_max is ",dist_max,"\n");
    cat("dist_next is ",dist_next,"\n");


  }

  output <- as.data.frame(input_params_s[,-dist_col])
  names(output) <- names(param)

  return(output)

}

RABC_core <-
  function(input_params_s,
           dist_next,
           num_keep,
           R,
           rw_cov,
           distance,
           prior_eval) {
    # resample from the particle population
    input_params_s <- as.numeric(input_params_s)

    iacc <- 0
    input_params <- as.numeric(input_params_s[-length(input_params_s)])
    input_s      <- as.numeric(input_params_s[length(input_params_s)])

    # attempt to move particle i with MCMC kernel (R iterations)
    for (j in 1:R) {
      # repeat

      prop <- as.numeric(MASS::mvrnorm(n = 1, as.matrix(input_params, ncol = 1), rw_cov))

      #check if its within the prior distribution

      if (prior_eval(prop) == 0) {
        next
      }

      dist_prop = distance(as.numeric(prop))


      if (dist_prop <= dist_next && prior_eval(prop) / prior_eval(input_params) > runif(1)) {
        # Metropolis-Hastings Ratio
        iacc <- iacc + 1

        input_params <- prop

        input_s <- dist_prop

      }
    }

    return(c(input_params, input_s, sum(iacc)))
}














