##### unit tests for pgp_cv.R  ######

#' @title Unit tests for pgp_cv function
#' @description This script contains unit tests for the pgp_cv function in the HAPSGP package.

library(testthat)
library(dplyr)
library(sf)
library(rsample)
library(spatialsample)
library(PrestoGP)

test_that("pgp_cv handles valid inputs correctly", {
  # Create a mock dataset
  mock_data <- data.frame(
    time = rep(
      seq.Date(
        from = as.Date("2023-01-01"),
        to = as.Date("2023-01-10"),
        by = "days"
      ),
      each = 2
    ),
    lon = runif(20, -100, -90),
    lat = runif(20, 30, 40),
    CONC_DAILY_STD = runif(20, 1, 100),
    MDL_DAILY_STD_UG_M3 = runif(20, 0.1, 10),
    AQS_PARAMETER_NAME = rep(c("Chem1", "Chem2"), 10),
    cov1 = rnorm(20),
    cov2 = rnorm(20)
  )

  # Test with random cross-validation
  result <- pgp_cv(
    data = mock_data,
    dates = c("2023-01-01", "2023-01-05"),
    vars = c("cov1"),
    logscale = FALSE,
    cv_splits = 2,
    cv_method = "random"
  )

  expect_type(result, "list")
  expect_true("dates_vec" %in% names(result))
  expect_true("chemlist" %in% names(result))
  expect_true("otr" %in% names(result))
  expect_true("otst" %in% names(result))
  expect_true("model.list" %in% names(result))
  expect_true("pred.list" %in% names(result))
})

test_that("pgp_cv applies log transformation when logscale = TRUE", {
  mock_data <- data.frame(
    time = rep(
      seq.Date(
        from = as.Date("2023-01-01"),
        to = as.Date("2023-01-10"),
        by = "days"
      ),
      each = 2
    ),
    lon = runif(20, -100, -90),
    lat = runif(20, 30, 40),
    CONC_DAILY_STD = runif(20, 1, 100),
    MDL_DAILY_STD_UG_M3 = runif(20, 0.1, 10),
    AQS_PARAMETER_NAME = rep(c("Chem1", "Chem2"), 10),
    cov1 = rnorm(20),
    cov2 = rnorm(20)
  )

  result <- pgp_cv(
    data = mock_data,
    dates = c("2023-01-01", "2023-01-05"),
    vars = c("cov1"),
    logscale = TRUE,
    cv_splits = 2,
    cv_method = "random"
  )

  expect_true(all(result$logscale))
  expect_true(all(log(mock_data$CONC_DAILY_STD) %in% result$otr[[1]]))
})

test_that("pgp_cv handles spatial cross-validation", {
  mock_data <- data.frame(
    time = rep(
      seq.Date(
        from = as.Date("2023-01-01"),
        to = as.Date("2023-01-10"),
        by = "days"
      ),
      each = 2
    ),
    lon = runif(20, -100, -90),
    lat = runif(20, 30, 40),
    CONC_DAILY_STD = runif(20, 1, 100),
    MDL_DAILY_STD_UG_M3 = runif(20, 0.1, 10),
    AQS_PARAMETER_NAME = rep(c("Chem1", "Chem2"), 10),
    cov1 = rnorm(20),
    cov2 = rnorm(20)
  )

  result <- pgp_cv(
    data = mock_data,
    dates = c("2023-01-01", "2023-01-05"),
    vars = c("cov1"),
    logscale = FALSE,
    cv_splits = 2,
    cv_method = "spatialrandom"
  )

  expect_type(result, "list")
  expect_true("otr" %in% names(result))
  expect_true("otst" %in% names(result))
  expect_true(length(result$otr) == 2)
  expect_true(length(result$otst) == 2)
})

test_that("pgp_cv throws errors for invalid inputs", {
  mock_data <- data.frame(
    time = rep(
      seq.Date(
        from = as.Date("2023-01-01"),
        to = as.Date("2023-01-10"),
        by = "days"
      ),
      each = 2
    ),
    lon = runif(20, -100, -90),
    lat = runif(20, 30, 40),
    CONC_DAILY_STD = runif(20, 1, 100),
    MDL_DAILY_STD_UG_M3 = runif(20, 0.1, 10),
    AQS_PARAMETER_NAME = rep(c("Chem1", "Chem2"), 10),
    cov1 = rnorm(20),
    cov2 = rnorm(20)
  )

  expect_error(pgp_cv(
    data = NULL,
    dates = c("2023-01-01"),
    vars = c("cov1"),
    logscale = FALSE,
    cv_splits = 2
  ))
  expect_error(pgp_cv(
    data = mock_data,
    dates = NULL,
    vars = c("cov1"),
    logscale = FALSE,
    cv_splits = 2
  ))
  expect_error(pgp_cv(
    data = mock_data,
    dates = c("2023-01-01"),
    vars = c("cov1"),
    logscale = "not_logical",
    cv_splits = 2
  ))
  expect_error(pgp_cv(
    data = mock_data,
    dates = c("2023-01-01"),
    vars = c("cov1"),
    logscale = FALSE,
    cv_splits = 2,
    cv_method = "invalid_method"
  ))
})
