

abc_algorithm <- function(prior, distance, distance_args, algorithm, control, output_control, lfunc){
  UseMethod("abc_algorithm", algorithm)
}

# Rejection

abc_algorithm.rejection <- function(prior, distance, distance_args, algorithm, control, output_control, lfunc){

  control <- do.call("abc_control.rejection", control)
  output_control <- do.call("abc_output.rejection", output_control)

  param <- prior(1)

  n <- control$n

  input_param <- as.numeric(as.matrix(param)); names(input_param) <- names(param);

  test_run <- distance(input_param, distance_args)


  all_col <- dim(param)[2] + length(test_run)
  dist_col <- dim(param)[2] + 1
  output <- matrix(ncol = all_col, nrow = 0)

  while(dim(output)[1] < control$n){

    new_output <- rejection_core(n, prior, distance, lfunc, distance_args = distance_args)

    #new_output <- matrix(unlist(new_output), ncol = dist_col, byrow = TRUE)

    new_output <- new_output[which(new_output[, dist_col] <= control$epsilon),]

    output <- rbind(output, new_output)

    n <- max(1, as.integer((control$n - (dim(output)[1]))/(dim(new_output)[1] / n + control$delta)))

    #print(dim(output)[1])
  }

  output <- as.data.frame(output)
  output <- output[sample.int(control$n), ]
  rownames(output) <- seq(length=nrow(output))

  names(output) <- c(names(param), names(test_run))

  if(output_control$include_dist){
    names(output)[dist_col] <- "distance"
    return(output)
  } else {
    return(output[, - dist_col, drop = FALSE])
  }

}

rejection_core <- function(n, prior, distance, lfunc, distance_args){
  param <- prior(n)

  new_output <- lfunc(as.matrix(param), 1, function(i, distance_args, distance) {

    out <- distance(i, distance_args)

    return(c(i, out))

  }, distance_args = distance_args, distance = distance)

  return(t(new_output))
}


# RABC



abc_algorithm.RABC <- function(prior, distance, distance_args, algorithm, control, output_control, lfunc){

  param <- prior(1)
  control$n_param <- dim(param)[2]
  control <- do.call("abc_control.RABC", control)
  output_control <- do.call("abc_output.RABC", output_control)

  input_param <- as.numeric(as.matrix(param)); names(input_param) <- names(param);

  test_run <- distance(input_param, distance_args)

  all_col <- dim(param)[2] + length(test_run)
  dist_col <- dim(param)[2] + 1

  output <- matrix(ncol = all_col, nrow = 0)

  trial_run <- rejection_core(control$n, prior, distance, lfunc, distance_args = distance_args)

  # print(trial_run)

  input_params_s <- trial_run[order(trial_run[,dist_col]), ]

  # print(input_params_s)

  dist_next <- input_params_s[control$num_keep, dist_col]
  dist_max <- input_params_s[control$n, dist_col]

  R = control$R

  while(dist_max > control$eps_final){

    rw_cov <- control$cov_func(input_params_s[1:control$num_keep, 1:dim(param)[2]])

    iacc <- 0

    index_resample_f <- sample(control$num_keep, control$n - control$num_keep, replace = TRUE)

    output <- lfunc(
      input_params_s[index_resample_f, ],
      1,
      RABC_core,
      distance_args = distance_args,
      dist_next = dist_next,
      num_keep = control$num_keep,
      R = R,
      rw_cov = rw_cov,
      distance = distance,
      prior_eval = control$prior_eval
    )

    output <- t(output)
    # To_user <<- output
    #
    # print(output)
    # print(input_params_s)

    input_params_s[(control$num_keep + 1):control$n, ] <- output[,-dim(output)[2]]
    iacc <- sum(output[,dim(output)[2]])

    p_acc <- iacc / (control$num_drop * R)
    R = floor(log(control$c1) / log(1 - p_acc) + 1)

    if (p_acc < control$pacc_final){
      break;
    }

    if ((dist_max - dist_next)/dist_max < control$d_eps_final){
      break;
    }

      # order the particles according to the distance
    input_params_s <- input_params_s[order(input_params_s[,dist_col]), ]

    dist_next <- input_params_s[control$num_keep, dist_col]
    dist_max <- input_params_s[control$n, dist_col]

    if(output_control$print_output){

      message("********************************");

      message("Acceptance prob of MCMC was ",p_acc);
      message("Number of MCMC moves for next iteration is ",R);
      message("Number of unique particles is ", length(unique(input_params_s[,1])))

      message("dist_max is ",dist_max);
      message("dist_next is ",dist_next);
    }


  }

  output <- as.data.frame(input_params_s)
  names(output) <- c(names(param), names(test_run))

  if(output_control$include_dist){
    names(output)[dist_col] <- "distance"
  } else {
    output <- output[,-dist_col]
  }

  return(output)

}

RABC_core <- function(
  input_params_s,
  distance_args,
  dist_next,
  num_keep,
  R,
  rw_cov,
  distance,
  prior_eval) {

    num_params <- dim(rw_cov)[1]

    if(is.null(num_params)){num_params <- 1}

    # resample from the particle population
    param_names <- names(input_params_s)


    input_params_s <- as.numeric(input_params_s)

    iacc <- 0
    input_params <- input_params_s[1:num_params]
    input_s      <- as.numeric(input_params_s[-c(1:num_params)])

    # attempt to move particle i with MCMC kernel (R iterations)
    for (j in 1:R) {
      # repeat

      prop <- as.numeric(MASS::mvrnorm(n = 1, as.matrix(as.numeric(input_params), ncol = 1), rw_cov))

      names(prop) <- param_names[1:num_params]
      names(input_params) <- names(prop)

      #check if its within the prior distribution

      if (prior_eval(prop) == 0) {
        next
      }

      dist_prop = as.numeric(distance(prop, distance_args))


      if (dist_prop[1] <= dist_next && prior_eval(prop) / prior_eval(input_params) > stats::runif(1)) {
        # Metropolis-Hastings Ratio
        iacc <- iacc + 1

        input_params <- prop

        input_s <- dist_prop

      }
    }

    output <- c(input_params, input_s, sum(iacc))

    #stopifnot(length(output) == length(input_params) + 3 + 1)

    return(output)
}














