#' @title sim_lur_strf
#' @description Simulate LUR covariates and multivariate space-time
#' random fields
#' @param n_spatial_xy The number of locations in the x and y dimension.
#' [scalar, numeric]
#' @param st_metric temporal scaling for the space-time covariates.
#' [scalar, numeric]
#' @param nug2sill The ratio of the nugget (noise) to sill (space-time
#' @param signal2noise The mean trend (X *beta) to marginal variance ratio
#' [scalar, numeric]
#' @param st_metric temporal scaling for the space-time
#' covariates. [scalar, numeric]
#' @param marg_smoothness smoothness of Matern kernel for space-time error
#' @param ranges space and time range parameters
#' [length ny * 2, numeric; s1, t1, s2, t2, ...]
#' @param nug2sill The ratio of the nugget (noise) to sill
#' (space-time variance) [scalar, numeric]
#' @param covariate_rho_bounds bounds for covariate correlations via
#' @param beta_multiplier Optionally, non-zero coefficients can be multiplied
#' @param censor_type Type of censoring. [NULL is not censored, "random" or
#' @param censoring Is the data censored? [NULL is not censored; if "random",
#' then it should be a scalar or ny of probabilities; if "fixed", then it is
#' scalar or ny of censoring cutoffs]
#' @param outcome_rho_bounds Range for outcome correlations [length ny, numeric]
#' @param marg_var Outcome marginal space-time variances [length ny, numeric]
#' @param marg_smoothness smoothness of Matern kernel for
#' space-time error [length ny, numeric]
#' @param ranges space and time range parameters
#'  [length ny * 2, numeric; s1, t1, s2, t2, ...]
#' @param nz_vars number of zero variables [scalar, numeric]
#' @param n_nz_vars number of non-zero variables [scalar, numeric]
#' @param covariate_rho_bounds bounds forcovariate correlations via
#' conditional simulation [length 2, numeric]
#' @param beta_multiplier Optionally, non-zero coefficients can
#' be multiplied by this -otherwise they are 1. [length ny , numeric]
#' @param censor_type Type of censoring. [NULL is not censored,
#' "random" or "fixed", fixed is length 1 or ny]
#' @param censoring Is the data censored? [NULL is not censored;
#' if "random", then it should be a scalar or ny of probabalities;
#' if "fixed", then it scalar or ny of censoring cutoffs]
sim_lur_strf <- function(
  ny = NULL,
  n_spatial_xy = NULL,
  n_temporal = NULL,
  st_metric = NULL,
  intercepts = NULL,
  nug2sill = NULL,
  signal2noise = NULL,
  strf_smoothness = NULL,
  strf_alpha = NULL,
  strf_var = NULL,
  outcome_rho_bounds = NULL,
  marg_var = NULL,
  marg_smoothness = NULL,
  ranges = NULL,
  nz_vars = NULL,
  n_nz_vars = NULL,
  covariate_rho_bounds = NULL,
  beta_multiplier = NULL,
  censor_type = NULL,
  censoring = NULL
) {
  # Space-time N
  n_st <- n_spatial_xy^2 * n_temporal

  #### Set up the correlation (rho) across multivariate outcomes
  n_rho <- choose(ny, 2)
  rho_vec <- runif(n_rho, outcome_rho_bounds[1], outcome_rho_bounds[2])
  rho <- matrix(0, nrow = ny, ncol = ny)
  rho[upper.tri(rho)] <- rho_vec
  rho <- rho + t(rho) + diag(1, ny)

  ####
  ################ SET UP LOCATIONS ###############

  locs_list <- list()
  locs_listb <- list()
  for (i in 1:ny) {
    loc1 <- seq(0, 1, length.out = n_spatial_xy) + rnorm(n_spatial_xy, 0, 0.001)
    loc2 <- seq(0, 1, length.out = n_spatial_xy) + rnorm(n_spatial_xy, 0, 0.001)
    loc3 <- round(seq(1, n_temporal, length.out = n_temporal))
    loc1b <- loc1 / ranges[2 * i - 1]
    loc2b <- loc2 / ranges[2 * i - 1]
    loc3b <- loc3 / ranges[2 * i]
    locs_list[[i]] <- as.matrix(expand.grid(loc1, loc2, loc3))
    locs_listb[[i]] <- as.matrix(expand.grid(loc1b, loc2b, loc3b))
  }
  ####### END LOCATIONS ###############

  ###### COVARIATES #############

  ##### Covariates as space-time random fields
  n_total_vars <- n_nz_vars + nz_vars
  if (is.null(strf_smoothness)) {
    strf_smoothness <- 0.5
  }

  if (is.null(strf_alpha)) {
    strf_alpha <- 0.5
  }

  if (is.null(strf_var)) {
    strf_var <- 1
  }

  if (is.null(st_metric)) {
    st_metric <- 2 # Time distance is 1/2 spatial distance
  }

  if (is.null(beta_multiplier)) {
    beta_multiplier <- 1
  }

  locs_st <- locs_list[[1]]
  locs_st[, 3] <- locs_st[, 3] / st_metric
  dijk_covariates <- fields::rdist(locs_st, locs_st)

  covariate_field <- matrix(NA, nrow = n_st, ncol = n_total_vars)
  # Do the first covariate simulation as a Matern space-time random field
  sigma_strf <- strf_var *
    fields::Matern(
      dijk_covariates,
      smoothness = strf_smoothness,
      alpha = strf_alpha
    )
  l_c <- t(chol(sigma_strf))
  covariate_field[, 1] <- l_c %*% rnorm(n_st, mean = 0, sd = 1)

  covariate_correlations <- covariate_rho_bounds[1] +
    (covariate_rho_bounds[2] - covariate_rho_bounds[1]) *
      rbeta(n_total_vars - 1, 0.5, 0.5)

  for (i in 2:n_total_vars) {
    sigma_strf <- runif(1, 0.1, 5) *
      fields::Matern(
        dijk_covariates,
        smoothness = runif(1, 0.499, 2.5001),
        alpha = runif(1, 0.01, 3)
      )
    l_c <- t(chol(sigma_strf))
    field_residual <- l_c %*% rnorm(n_st, mean = 0, sd = 1) |> as.numeric()
    covariate_field[, i] <- covariate_correlations[i - 1] *
      covariate_field[, 1] +
      field_residual
  }
  covariate_field_scaled <- scale(covariate_field)[, 1:n_total_vars]

  beta_init <- c(rep(1 * beta_multiplier, n_nz_vars), rep(0, nz_vars)) |>
    rep(ny)

  X_all <- rep(list(covariate_field_scaled), ny) |> psych::superMatrix()
  mean_trend <- X_all %*% beta_init

  ##### END COVARIATES

  ###### SPATIAL RANDOM EFFECTS ###############

  # the nugget is based on the nug2sill ratio
  nuggets <- marg_var * nug2sill
  marg_error <- marg_var + nuggets

  sigma_all <- matrix(
    nrow = ny * n_spatial_xy * n_spatial_xy * n_temporal,
    ncol = ny * n_spatial_xy * n_spatial_xy * n_temporal
  )
  for (i in 1:ny) {
    for (j in i:ny) {
      if (i == j) {
        ndx1 <- ((i - 1) *
          n_spatial_xy *
          n_spatial_xy *
          n_temporal +
          1):(n_spatial_xy * n_spatial_xy * n_temporal * i)
        dij <- fields::rdist(locs_listb[[i]])
        sigma_all[ndx1, ndx1] <- marg_error[i] *
          fields::Matern(dij, range = 1, smoothness = marg_smoothness[i])
      } else {
        ndx1 <- ((i - 1) *
          n_spatial_xy *
          n_spatial_xy *
          n_temporal +
          1):(n_spatial_xy * n_spatial_xy * n_temporal * i)
        ndx2 <- ((j - 1) *
          n_spatial_xy *
          n_spatial_xy *
          n_temporal +
          1):(n_spatial_xy * n_spatial_xy * n_temporal * j)
        dij <- fields::rdist(locs_listb[[i]], locs_listb[[j]])
        vii <- marg_smoothness[i]
        vjj <- marg_smoothness[j]
        vij <- (vii + vjj) / 2
        aii <- 1
        ajj <- 1
        aij <- sqrt((aii^2 + ajj^2) / 2)
        sigma_all[ndx1, ndx2] <- rho[i, j] *
          sqrt(marg_error[i]) *
          sqrt(marg_error[j]) *
          aii^vii *
          ajj^vjj *
          gamma(vij) /
          (aij^(2 * vij) * sqrt(gamma(vii) * gamma(vjj))) *
          fields::Matern(dij, smoothness = vij, alpha = aij)
        sigma_all[ndx2, ndx1] <- t(sigma_all[ndx1, ndx2])
      }
    }
  }

  L_C <- t(chol(sigma_all)) # Cholesky decomposition of sigma_all

  st_error <- L_C %*% rnorm(n_spatial_xy^2 * n_temporal * ny) |> as.numeric()
  ##### END SPACE-TIME ERROR

  ##### NUGGET  ########################

  nug_error <- NULL
  for (i in 1:ny) {
    nug_error <- c(
      nug_error,
      sqrt(nuggets[i]) * rnorm(n_spatial_xy^2 * n_temporal)
    )
  }
  ###### END NUGGET   ########################

  ###### Calculate the signal to noise ratio and adjust the betas to match the desired value ########

  current_s2n <- var(mean_trend) / var(st_error + nug_error)
  scaling_factor <- as.numeric(sqrt(signal2noise / current_s2n))
  beta_scaled <- beta_init * scaling_factor

  mean_trend <- X_all %*% beta_scaled

  # check signal to noise
  new_s2n <- var(mean_trend) / var(st_error + nug_error)
  #### END scaling according signal to noise ratio

  #### MAIN SIMULATION: GENERATE DATA #######################
  y_list <- list()
  for (i in 1:ny) {
    ndx1 <- ((i - 1) * n_spatial_xy^2 * n_temporal + 1):(n_spatial_xy^2 *
      n_temporal *
      i)
    y_list[[i]] <- mean_trend[ndx1] +
      st_error[ndx1] +
      nug_error[ndx1] +
      intercepts[i]
  }

  y_final <- unlist(y_list)
  N_final <- length(y_final)

  #### END MAIN SIMULATION: GENERATE DATA ###############

  #### CENSORING #####

  if (!is.null(censoring)) {
    if (length(censoring) == ny) {
      if (censor_type == "random") {
        censored <- lapply(1:ny, \(x) {
          sample(
            c(TRUE, FALSE),
            size = length(y_list[[x]]),
            replace = TRUE,
            prob = c(censoring[x], 1 - censoring[x])
          )
        })
      } else if (censor_type == "fixed") {
        censored <- lapply(1:ny, \(x) {
          y_vals <- y_list[[x]]
          rep(FALSE, length(y_vals)) | (y_vals < censoring[x])
        })
      } else {
        stop("censor_type should be fixed or random")
      }
    } else if (length(censoring) == 1) {
      if (censor_type == "random") {
        censored <- sample(
          c(TRUE, FALSE),
          size = N_final,
          replace = TRUE,
          prob = c(censoring, 1 - censoring)
        )
      } else if (censor_type == "fixed") {
        censored <- y_final < censoring
      } else {
        stop("censor_type should be fixed or random")
      }
    }
  } else {
    censored <- NULL
  }

  #### END CENSORING #####

  ### PUT ALL THE DATA INTO A LIST  #####

  group_indices <- rep(1:ny, each = n_spatial_xy * n_spatial_xy * n_temporal)

  results <- list(
    "Ylist" = y_list,
    "Yvec" = y_final,
    "Xlist" = covariate_field_scaled,
    "Xvec" = X_all,
    "mean_trend" = mean_trend,
    "st_error" = st_error,
    "nug_error" = nug_error,
    "beta_scaled" = beta_scaled,
    "locs" = locs_list,
    "locs_scaled" = locs_listb,
    "group_indices" = group_indices,
    "censored" = censored
  )

  return(results)
}

#' @title LMC_likelihood- based on implementation in https://arxiv.org/pdf/2402.08877 Equation 2
#' @description Linear Model of Coregionalization likelihood assuming fixed Covariance- estiamtes the LMC matrix A
#' @param a The LMC matrix A in vector form [vector, length(p x p) numeric]
#' @param K The number of locations in the x and y dimension. [length(p),K_j is [n x n], list]
#' @param sigma2  spatial variance for each K_j [vector, length(p), numeric]
#' @param Y The responses  [list, length(p), p_j is [n x 1], numeric]
LMC_likelihood <- function(p0, locs.list, Y) {
  #####
  ##### Y IS V IN THE EQUATION!!!
  #####
  P <- length(locs.list)
  param_sequence <- create_param_sequence(P)
  a <- p0[param_sequence[1, 1]:param_sequence[1, 2]]
  A <- matrix(a, nrow = P, ncol = P, byrow = TRUE)
  range <- log(1 + exp(p0[param_sequence[2, 1]:param_sequence[2, 2]]))
  smoothness <- gtools::inv.logit(
    p0[param_sequence[3, 1]:param_sequence[3, 2]],
    0.45,
    2.55
  )
  nugget <- log(1 + exp(p0[param_sequence[4, 1]:param_sequence[4, 2]]))
  # a <- p0[1:3]
  # A <- lower_tri_to_A(a, 2)
  # # A <- matrix(a, nrow = length(Y), ncol = length(Y))
  # range <- log(1 + exp(p0[4:5]))
  # smoothness <- gtools::inv.logit(p0[6:7],0.45, 2.55)
  # nugget <- log(1 + exp(p0[8:9]))

  V <- do.call(rbind, Y)

  n <- length(Y[[1]])
  d <- lapply(1:2, \(x) {
    fields::rdist.earth(locs.list[[x]], locs.list[[x]], miles = FALSE)
  })
  R <- lapply(1:2, \(x) {
    fields::Matern(d[[x]], range = range[x], smoothness = smoothness[x])
  }) # + diag(length(Y[[x]])) * nugget[x]^2})

  # Initialize the sum in the numerator
  sum_term <- 0
  # Loop over all components (j)
  A_inv <- solve(A)
  for (j in seq_along(R)) {
    R_inv <- solve(R[[j]]) # Inverse of R_j
    a_j_inv <- matrix(A_inv[j, ], nrow = 1)
    a_j_inv_t <- t(a_j_inv)

    # Compute the quadratic form for this component
    quad_term <- a_j_inv %*% V %*% R_inv %*% t(V) %*% a_j_inv_t

    # Add the trace of the quadratic form to the sum
    sum_term <- sum(sum_term, quad_term, na.rm = TRUE)
  }
  print(sum_term)
  # Compute the numerator
  numerator <- exp(-0.5 * sum_term)

  print(numerator)
  # Compute the denominator
  A_det <- abs(det(A)) # Determinant of A (assuming A is block-diagonal)
  R_dets <- sapply(R, det)
  print(R_dets) # Determinants of R_j
  denominator <- (2 * pi)^((n * P) / 2) * (A_det^n) * prod(R_dets^(1 / 2))
  print(denominator)
  # Compute the full likelihood, estimate A given R and Y
  likelihood <- -1 * log(numerator / denominator)
  print(likelihood)
  return(likelihood)
}


#' @title LMC_fit
#' @description Fit the LMC of the [p x n] matrix
#' @param Y [p x n] matrix
LMC_fit <- function(Y, locs) {
  K <- list()
  sigma2 <- rep(NA, nrow = nrow(Y))
  a_old <- diag(nrow(Y)) |> as.numeric()
  X <- list(matrix(rnorm(ncol(Y) * 2), ncol = nrow(Y)))
  tolerance <- 1e-6
  converged <- FALSE
  iter <- 1
  while (!converged) {
    print(iter)
    for (i in 1:nrow(Y)) {
      mdl <- new("MultivariateVecchiaModel", n_neighbors = 25)
      ylist <- list(as.matrix(Y[i, ]))
      fitted_mdl <- prestogp_fit(
        mdl,
        Y = ylist,
        X = X,
        locs = list(as.matrix(locs)),
        scaling = c(1, 1),
        family = "gaussian",
        impute.y = FALSE,
        quiet = TRUE
      )
      d_locs <- fields::rdist(locs)
      theta <- get_theta(fitted_mdl)
      K[[i]] <- (theta$sigma) *
        fields::Matern(
          d = d_locs,
          range = theta$scale,
          smoothness = theta$smoothness
        ) +
        theta$nugget
      sigma2[i] <- theta$sigma
    }
    result <- optim(
      a_old,
      fn = LMC_likelihood,
      K = K,
      sigma2 = sigma2,
      Y = Y,
      method = "Nelder-Mead"
    )
    print(paste("LMC likelihood:", result$value))
    print(paste("A matrix:", gtools::inv.logit(result$par, -1, 1)))

    a <- result$par

    if (sum(mean((a_old - a)^2)) < tolerance) {
      a <- a_old
      converged <- TRUE
    } else {
      a_old <- a
      iter <- iter + 1
      print(converged)
    }
  }
  output <- list(a = a, K = K, sigma2 = sigma2)
  return(output)
}

# y is a p x n
negloglik.LMC <- function(p0, d, Y) {
  # a <- gtools::inv.logit(p0[1:4])
  a <- p0[1:4]
  A <- matrix(a, nrow = length(Y), ncol = length(Y))
  range <- p0[5:6]
  smoothness <- p0[7:8]
  nugget <- p0[9:10]
  # params <- c(exp(range), gtools::inv.logit(smoothness,0.4, 2.6), max(0,nugget[1]), max(0, nugget[2]))
  # params <- c(exp(range), gtools::inv.logit(smoothness,0.45, 2.55), nugget)
  params <- c(
    exp(range),
    gtools::inv.logit(smoothness, 0.45, 2.55),
    log(1 + exp(nugget))
  )

  # params <- c(exp(range), gtools::inv.logit(smoothness,0.4, 2.6), exp(nugget))
  Z <- unlist(Y)
  N <- length(Z)
  n <- length(Y[[1]])
  cor1 <- fields::Matern(d / params[1], range = 1, smoothness = params[3])

  cor2 <- fields::Matern(d / params[2], range = 1, smoothness = params[4])

  cov.ii <- (A[1, 1]^2 * cor1) + (A[1, 2]^2 * cor2) + (params[5]^2 * diag(n))
  cov.ij <- (A[1, 1] * A[2, 1] * cor1) +
    (A[1, 2] * A[2, 2] * cor2) +
    (params[6]^2 * diag(n))
  cov.ji <- t(cov.ij)
  cov.jj <- (A[2, 1]^2 * cor1) + (A[2, 2]^2 * cor2)
  cov.mat <- rbind(cbind(cov.ii, cov.ij), cbind(cov.ji, cov.jj))

  return(-1 * mvtnorm::dmvnorm(Z, rep(0, N), cov.mat, log = TRUE))
}

nll_LMC <- function(p0, d, Y) {
  P = length(Y)
  param_sequence <- create_param_sequence(P)
  a <- p0[param_sequence[1, 1]:param_sequence[1, 2]]
  A <- matrix(a, nrow = length(Y), ncol = length(Y), byrow = TRUE)
  range <- log(1 + exp(p0[param_sequence[2, 1]:param_sequence[2, 2]]))
  smoothness <- gtools::inv.logit(
    p0[param_sequence[3, 1]:param_sequence[3, 2]],
    0.45,
    2.55
  )
  nugget <- log(1 + exp(p0[param_sequence[4, 1]:param_sequence[4, 2]]))
  # params <- c(log(1 + exp(range)), gtools::inv.logit(smoothness,0.45, 2.55), log(1 + exp(nugget)))

  n <- sapply(Y, length)

  cor_matrices <- lapply(1:P, function(i) {
    fields::Matern(d = d[[i]] / range[i], range = 1, smoothness = smoothness[i])
  })

  cov_list <- lapply(1:P, function(i) {
    Reduce(`+`, lapply(1:P, function(j) A[i, j]^2 * cor_matrices[[j]])) +
      diag(n[i]) * nugget[i]^2
  })

  ll <- sapply(1:P, function(i) {
    -1 * mvtnorm::dmvnorm(Y[[i]], rep(0, n[i]), cov_list[[i]], log = TRUE)
  }) |>
    sum()

  return(ll)
}


nll_LMC_vecchia <- function(p0, locs, Y) {
  P = length(Y)
  param_sequence <- create_param_sequence(P)
  a <- p0[param_sequence[1, 1]:param_sequence[1, 2]]
  A <- matrix(a, nrow = length(Y), ncol = length(Y), byrow = TRUE)
  range <- log(1 + exp(p0[param_sequence[2, 1]:param_sequence[2, 2]]))
  smoothness <- gtools::inv.logit(
    p0[param_sequence[3, 1]:param_sequence[3, 2]],
    0.45,
    2.55
  )
  nugget <- log(1 + exp(p0[param_sequence[4, 1]:param_sequence[4, 2]]))
  # params <- c(log(1 + exp(range)), gtools::inv.logit(smoothness,0.45, 2.55), log(1 + exp(nugget)))

  n <- sapply(Y, length)

  cor_matrices <- lapply(1:P, function(i) {
    fields::Matern(d = d[[i]] / range[i], range = 1, smoothness = smoothness[i])
  })

  cov_list <- lapply(1:P, function(i) {
    Reduce(`+`, lapply(1:P, function(j) A[i, j]^2 * cor_matrices[[j]])) +
      diag(n[i]) * nugget[i]^2
  })

  ll <- sapply(1:P, function(i) {
    -1 * mvtnorm::dmvnorm(Y[[i]], rep(0, n[i]), cov_list[[i]], log = TRUE)
  }) |>
    sum()

  return(ll)
}

create_param_sequence_ind <- function(P) {
  param.sequence.begin <- c(1, seq(P^2 + 1, length = 3, by = P))
  param.sequence.end <- c(P^2, P, P, P) |> cumsum()
  param.sequence <- cbind(param.sequence.begin, param.sequence.end)

  return(param.sequence)
}
