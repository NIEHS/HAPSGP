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
