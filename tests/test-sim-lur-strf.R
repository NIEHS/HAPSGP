library(testthat)
library(fields)
library(psych)

test_that("sim_lur_strf generates valid outputs for basic inputs", {
  result <- sim_lur_strf(
    ny = 2,
    n_spatial_xy = 10,
    n_temporal = 5,
    st_metric = 2,
    intercepts = c(0, 1),
    nug2sill = 0.1,
    signal2noise = 2,
    strf_smoothness = 0.5,
    strf_alpha = 0.5,
    strf_var = 1,
    outcome_rho_bounds = c(0.1, 0.9),
    marg_var = c(1, 1),
    marg_smoothness = c(0.5, 0.5),
    ranges = c(0.1, 0.2, 0.1, 0.2),
    nz_vars = 2,
    n_nz_vars = 2,
    covariate_rho_bounds = c(0.1, 0.9),
    beta_multiplier = c(1, 1),
    censor_type = NULL,
    censoring = NULL
  )

  expect_type(result, "list")
  expect_true("Ylist" %in% names(result))
  expect_true("Yvec" %in% names(result))
  expect_true("Xlist" %in% names(result))
  expect_true("Xvec" %in% names(result))
  expect_true("mean_trend" %in% names(result))
  expect_true("st_error" %in% names(result))
  expect_true("nug_error" %in% names(result))
  expect_true("beta_scaled" %in% names(result))
  expect_true("locs" %in% names(result))
  expect_true("locs_scaled" %in% names(result))
  expect_true("group_indices" %in% names(result))
})

test_that("sim_lur_strf handles censoring correctly", {
  result <- sim_lur_strf(
    ny = 2,
    n_spatial_xy = 10,
    n_temporal = 5,
    st_metric = 2,
    intercepts = c(0, 1),
    nug2sill = 0.1,
    signal2noise = 2,
    strf_smoothness = 0.5,
    strf_alpha = 0.5,
    strf_var = 1,
    outcome_rho_bounds = c(0.1, 0.9),
    marg_var = c(1, 1),
    marg_smoothness = c(0.5, 0.5),
    ranges = c(0.1, 0.2, 0.1, 0.2),
    nz_vars = 2,
    n_nz_vars = 2,
    covariate_rho_bounds = c(0.1, 0.9),
    beta_multiplier = c(1, 1),
    censor_type = "random",
    censoring = c(0.2, 0.3)
  )

  expect_type(result$censored, "list")
  expect_length(result$censored, 2)
  expect_true(all(sapply(result$censored, is.logical)))
})

test_that("sim_lur_strf adjusts beta coefficients for signal-to-noise ratio", {
  result <- sim_lur_strf(
    ny = 2,
    n_spatial_xy = 10,
    n_temporal = 5,
    st_metric = 2,
    intercepts = c(0, 1),
    nug2sill = 0.1,
    signal2noise = 2,
    strf_smoothness = 0.5,
    strf_alpha = 0.5,
    strf_var = 1,
    outcome_rho_bounds = c(0.1, 0.9),
    marg_var = c(1, 1),
    marg_smoothness = c(0.5, 0.5),
    ranges = c(0.1, 0.2, 0.1, 0.2),
    nz_vars = 2,
    n_nz_vars = 2,
    covariate_rho_bounds = c(0.1, 0.9),
    beta_multiplier = c(1, 1),
    censor_type = NULL,
    censoring = NULL
  )

  expect_true(all(result$beta_scaled != 0))
  expect_true(
    var(result$mean_trend) / var(result$st_error + result$nug_error) - 2 < 1e-2
  )
})

test_that("sim_lur_strf throws errors for invalid inputs", {
  expect_error(sim_lur_strf(
    ny = 2,
    n_spatial_xy = 10,
    n_temporal = 5,
    st_metric = 2,
    intercepts = c(0, 1),
    nug2sill = -0.1, # Invalid nug2sill
    signal2noise = 2,
    strf_smoothness = 0.5,
    strf_alpha = 0.5,
    strf_var = 1,
    outcome_rho_bounds = c(0.1, 0.9),
    marg_var = c(1, 1),
    marg_smoothness = c(0.5, 0.5),
    ranges = c(0.1, 0.2, 0.1, 0.2),
    nz_vars = 2,
    n_nz_vars = 2,
    covariate_rho_bounds = c(0.1, 0.9),
    beta_multiplier = c(1, 1),
    censor_type = NULL,
    censoring = NULL
  ))
  expect_error(sim_lur_strf(
    ny = 2,
    n_spatial_xy = 10,
    n_temporal = 5,
    st_metric = 2,
    intercepts = c(0, 1),
    nug2sill = 0.1,
    signal2noise = 2,
    strf_smoothness = 0.5,
    strf_alpha = 0.5,
    strf_var = 1,
    outcome_rho_bounds = c(0.1, 0.9),
    marg_var = c(1, 1),
    marg_smoothness = c(0.5, 0.5),
    ranges = c(0.1, 0.2), # Invalid ranges length
    nz_vars = 2,
    n_nz_vars = 2,
    covariate_rho_bounds = c(0.1, 0.9),
    beta_multiplier = c(1, 1),
    censor_type = NULL,
    censoring = NULL
  ))
})
