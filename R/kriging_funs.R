#' @title negloglik_full_spatial
#' @description Ordinary Kriging Negative Log-Likelihood Function
#' @details The `fields::rdist` function computes the Euclidean distance
#' between rows of a matrix. It is used here to determine the spatial
#' relationships between locations.
#' @param locs A matrix or data frame where each row represents a location
#' (e.g., coordinates in 2D or 3D space).
#' @return negative log likelihood.
#' @importFrom fields rdist
negloglik_full_spatial <- function(logparms, locs, y, sample_size) {
  parms <- logparms
  parms[1:3] <- exp(parms[1:3])
  parms[4] <- gtools::inv.logit(parms[4], 0, 2.5)

  d <- fields::rdist(locs)
  cov_mat <- parms[1] *
    fields::Matern(d, range = parms[2], smoothness = parms[4]) +
    parms[3] * diag(sample_size)
  -mvtnorm::dmvnorm(y, rep(0, sample_size), cov_mat, log = TRUE)
}

neg_loglik_smoothness <- function(smoothness_value, x_train, y_train) {
  fit <- fields::Krig(
    x_train,
    y_train,
    Covariance = "Matern",
    smoothness = smoothness_value,
    theta = c(0.05, 0.5)
  ) # starting values; Krig() optimizes theta

  # Return negative log-likelihood (because optimize minimizes)
  return(-fit$lnProfileLike.FULL)
}

################################################################################
# Ordinary Spatiotemporal Kriging Prediction with a Local S-T neigborhood
################################################################################
Kr_pred = function(new_coords, obs_coords, Y_obs, cov.pars, NN) {
  Kr.prediction = matrix(NA, nrow = nrow(new_coords), ncol = 1)
  Kr.Var = matrix(NA, nrow = nrow(new_coords), ncol = 1)

  new_coords_scaled <- cbind(
    new_coords[, 1] / cov.pars[2],
    new_coords[, 2] / cov.pars[2],
    new_coords[, 3] / cov.pars[3]
  )
  obs_coords_scaled <- cbind(
    obs_coords[, 1] / cov.pars[2],
    obs_coords[, 2] / cov.pars[2],
    obs_coords[, 3] / cov.pars[3]
  )
  df_new <- new_coords_scaled
  df_obs <- obs_coords_scaled

  for (i in 1:nrow(new_coords)) {
    # print(i)
    locs.test.i <- rep.row(df_new[i, ], nrow(df_obs))
    dist.op <- fields::rdist.vec(locs.test.i, df_obs)
    Sigma.op <- cov.pars[1] * fields::Exponential(dist.op, range = 1)
    s.op <- sort(dist.op, index.return = TRUE)
    s.idx <- s.op$ix[1:NN]
    # Get the closest "NN" observations
    dist.oo <- fields::rdist(df_obs[s.idx, ], df_obs[s.idx, ])
    Sigma.oo <- cov.pars[4] *
      diag(NN) +
      cov.pars[1] * fields::Exponential(dist.oo, range = 1)
    oo <- Y_obs[s.idx]
    ## kriging predictor (posterior mean)
    mean_trend <- mean(oo)
    Kr.prediction[i] <- mean_trend +
      t(Sigma.op[s.idx]) %*% solve(Sigma.oo, oo - mean_trend)

    Kr.Var[i] = cov.pars[1] -
      t(Sigma.op[s.idx]) %*% solve(Sigma.oo, Sigma.op[s.idx])
  }

  Kr.Data <- data.frame(Kr.prediction, Kr.Var)

  return(Kr.Data)
}
