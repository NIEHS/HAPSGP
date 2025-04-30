# unit tests for a product of experts (POE) based on
# a bagging of PrestoGP models
library(testthat)
###
testthat::test_that("pgp-poe produces posterior around ML", {
  withr::local_package("PrestoGP")
  withr::local_package("MCMCpack")
  withr::local_package("ggplot2")
  data(soil)
  soil <- soil[!is.na(soil[, 5]), ] # remove rows with NA's

  locs <- as.matrix(soil[, 1:2])

  # Multivariate model
  y_m <- list()
  y_m[[1]] <- soil[, 5] # predict two nitrogen concentration levels
  y_m[[2]] <- soil[, 7]
  x_m <- list()
  x_m[[1]] <- x_m[[2]] <- as.matrix(soil[, c(4, 6, 8, 9)])
  locs_m <- list()
  locs_m[[1]] <- locs_m[[2]] <- locs

  soil_mvm <- new("MultivariateVecchiaModel", n_neighbors = 10)

  soil_mvm <- prestogp_fit(soil_mvm, y_m, x_m, locs_m)

  beta_mvm <- get_beta(soil_mvm)
  theta_mvm <- get_theta(soil_mvm) |> unlist()

  # Do the bagging of PrestoGP models
  b <- 100
  y1_betas <- matrix(NA, nrow = b, ncol = 4)
  y2_betas <- matrix(NA, nrow = b, ncol = 4)
  y1_thetas <- matrix(NA, nrow = b, ncol = 5)
  y2_thetas <- matrix(NA, nrow = b, ncol = 5)
  for (i in seq_len(b)) {
    y_w <- list()
    x_w <- list()
    locs_w <- list()
    int1 <- sample.int(length(y_m[[1]]), 100, replace = FALSE)
    int2 <- sample.int(length(y_m[[2]]), 100, replace = FALSE)
    y_w[[1]] <- y_m[[1]][int1]
    y_w[[2]] <- y_m[[2]][int2]
    x_w[[1]] <- x_m[[1]][int1, ]
    x_w[[2]] <- x_m[[1]][int2, ]
    locs_w[[1]] <- locs_m[[1]][int1, , drop = FALSE]
    locs_w[[2]] <- locs_m[[2]][int2, , drop = FALSE]

    soil_wbb <- new("MultivariateVecchiaModel", n_neighbors = 10)

    soil_wbb <- prestogp_fit(
      soil_wbb,
      y_w,
      x_w,
      locs_w,
      quiet = TRUE,
      verbose = TRUE
    )

    betas <- get_beta(soil_wbb)
    thetas <- get_theta(soil_wbb) |> unlist()

    y1_betas[i, ] <- betas$Y1
    y2_betas[i, ] <- betas$Y2
    y1_thetas[i, ] <- thetas[c(1, 3, 5, 7, 10)]
    y2_thetas[i, ] <- thetas[c(2, 4, 6, 8, 11)]
  }
  df <- data.frame(
    "beta1" = c(y1_betas[, 1], y2_betas[, 1]),
    "beta2" = c(y1_betas[, 2], y2_betas[, 2]),
    "beta3" = c(y1_betas[, 3], y2_betas[, 3]),
    "beta4" = c(y1_betas[, 4], y2_betas[, 4]),
    "theta1" = c(y1_thetas[, 1], y2_thetas[, 1]),
    "theta2" = c(log10(y1_thetas[, 2]), log10(y2_thetas[, 2])),
    "theta3" = c(y1_thetas[, 3], y2_thetas[, 3]),
    "theta4" = c(y1_thetas[, 4], y2_thetas[, 4]),
    "theta5" = c(y1_thetas[, 5], y2_thetas[, 5]),
    "Y" = c(rep(1, b), rep(2, b))
  ) |>
    pivot_longer(
      cols = starts_with("beta") | starts_with("theta"),
      names_to = "X",
      values_to = "Value"
    )

  df_single <- data.frame(
    "X" = rep(
      c(
        "beta1",
        "beta2",
        "beta3",
        "beta4",
        "theta1",
        "theta2",
        "theta3",
        "theta4",
        "theta5"
      ),
      each = 2
    ),
    "Value" = c(
      beta_mvm$Y1[1],
      beta_mvm$Y2[1],
      beta_mvm$Y1[2],
      beta_mvm$Y2[2],
      beta_mvm$Y1[3],
      beta_mvm$Y2[3],
      beta_mvm$Y1[4],
      beta_mvm$Y2[4],
      theta_mvm[1],
      theta_mvm[2],
      log10(theta_mvm[3]),
      log10(theta_mvm[4]),
      theta_mvm[5],
      theta_mvm[6],
      theta_mvm[7],
      theta_mvm[8],
      theta_mvm[10],
      theta_mvm[11]
    ),
    "Y" = rep(c(1, 2), times = 9) # Repeat Y = 1 and Y = 2 for each variable
  )

  p1 <- ggplot(df) +
    geom_density(aes(x = Value, color = as.factor(Y)), linewidth = 1.1) +
    geom_point(
      data = df_single,
      aes(x = Value, y = 0, color = as.factor(Y)),
      size = 3,
      shape = 4
    ) + # Add points for the whole model instance
    facet_wrap(. ~ X, scales = "free") +
    theme_bw()

  # Calculate that the single model is wiwthin the 95% CI of the bagged model
  library(dplyr)

  # Calculate 99.9% confidence intervals for each group in df
  ci_df <- df %>%
    group_by(X) %>%
    summarize(
      lower = quantile(Value, 0.001),
      upper = quantile(Value, 0.999)
    )

  # Merge df_single with the confidence intervals
  df_single_with_ci <- df_single %>%
    left_join(ci_df, by = "X")

  # Check if df_single values fall within the 95% CI
  df_single_with_ci <- df_single_with_ci %>%
    mutate(
      coverage = Value >= lower & Value <= upper
    )

  # A few checks on the prestogp_fit object
  expect_true(validObject(soil_mvm))
  expect_equal(get_Y(soil_mvm), y_m)
  # Check ggplot object
  expect_s3_class(p1, "ggplot")
  expect_true(all(df_single_with_ci$coverage))
})

testthat::test_that("PrestoGP out-of-sample MSE on Soil Data", {
})


testthat::test_that("Elastic-Net out-of-sample MSE on Soil Data", {
})

testthat::test_that("Ordinary Kriging out-of-sample MSE on Soil Data", {
})


testthat::test_that("PrestogP-PoE (simple) out-of-sample MSE on Soil Data", {
  withr::local_package("PrestoGP")
  withr::local_package("ggplot2")
  withr::local_package("tidyverse")
  data(soil)
  soil <- soil[!is.na(soil[, 5]), ] # remove rows with NA's

  locs <- as.matrix(soil[, 1:2])

  # Multivariate model
  y_m <- list()
  y_m[[1]] <- soil[, 5] # predict two nitrogen concentration levels
  y_m[[2]] <- soil[, 7]
  x_m <- list()
  x_m[[1]] <- x_m[[2]] <- as.matrix(soil[, c(4, 6, 8, 9)])
  locs_m <- list()
  locs_m[[1]] <- locs_m[[2]] <- locs

  # Create a small hold-out set
  index_train <- sample.int(nrow(soil), 0.8 * nrow(soil))
  index_test <- setdiff(seq_len(nrow(soil)), index_train)

  y_m_train <- list()
  y_m_train[[1]] <- y_m[[1]][index_train]
  y_m_train[[2]] <- y_m[[2]][index_train]
  y_m_test <- list()
  y_m_test[[1]] <- y_m[[1]][index_test]
  y_m_test[[2]] <- y_m[[2]][index_test]
  x_m_train <- list()
  x_m_train[[1]] <- x_m[[1]][index_train, ]
  x_m_train[[2]] <- x_m[[2]][index_train, ]
  x_m_test <- list()
  x_m_test[[1]] <- x_m[[1]][index_test, ]
  x_m_test[[2]] <- x_m[[2]][index_test, ]
  locs_m_train <- list()
  locs_m_train[[1]] <- locs_m[[1]][index_train, , drop = FALSE]
  locs_m_train[[2]] <- locs_m[[2]][index_train, , drop = FALSE]
  locs_m_test <- list()
  locs_m_test[[1]] <- locs_m[[1]][index_test, , drop = FALSE]
  locs_m_test[[2]] <- locs_m[[2]][index_test, , drop = FALSE]

  # Do the bagging of PrestoGP models
  b <- 100
  y1_betas <- matrix(NA, nrow = b, ncol = 4)
  y2_betas <- matrix(NA, nrow = b, ncol = 4)
  y1_thetas <- matrix(NA, nrow = b, ncol = 5)
  y2_thetas <- matrix(NA, nrow = b, ncol = 5)
  mse_pred <- matrix(NA, nrow = b, ncol = 2)
  y1_pred <- matrix(NA, nrow = b, ncol = length(y_m_test[[1]]))
  y2_pred <- matrix(NA, nrow = b, ncol = length(y_m_test[[2]]))
  for (i in seq_len(b)) {
    print(i)
    y_w <- list()
    x_w <- list()
    locs_w <- list()
    int1 <- sample.int(length(y_m_train[[1]]), 100, replace = FALSE)
    int2 <- sample.int(length(y_m_train[[2]]), 100, replace = FALSE)
    y_w[[1]] <- y_m_train[[1]][int1]
    y_w[[2]] <- y_m_train[[2]][int2]
    x_w[[1]] <- x_m_train[[1]][int1, ]
    x_w[[2]] <- x_m_train[[1]][int2, ]
    locs_w[[1]] <- locs_m_train[[1]][int1, , drop = FALSE]
    locs_w[[2]] <- locs_m_train[[2]][int2, , drop = FALSE]

    soil_poe <- new("MultivariateVecchiaModel", n_neighbors = 10)

    soil_poe <- prestogp_fit(
      soil_poe,
      y_w,
      x_w,
      locs_w,
      quiet = TRUE,
      verbose = TRUE
    )

    # Get the betas and thetas for the bagged model for
    # exploratory purposes
    betas <- get_beta(soil_poe)
    thetas <- get_theta(soil_poe) |> unlist()

    y1_betas[i, ] <- betas$Y1
    y2_betas[i, ] <- betas$Y2
    y1_thetas[i, ] <- thetas[c(1, 3, 5, 7, 10)]
    y2_thetas[i, ] <- thetas[c(2, 4, 6, 8, 11)]

    # Do the prediction on the test set
    soil_pred <- PrestoGP::prestogp_predict(
      soil_poe,
      x_m_test,
      locs_m_test,
      return.values = "mean"
    )

    y1_pred[i, ] <- soil_pred$means$Y1
    y2_pred[i, ] <- soil_pred$means$Y2
    mse_pred[i, 1] <- mean((y_m_test[[1]] - soil_pred$means$Y1)^2)
    mse_pred[i, 2] <- mean((y_m_test[[2]] - soil_pred$means$Y2)^2)
  }

  # PoE prediction
  y1_pred_mean <- apply(y1_pred, 2, mean)
  y2_pred_mean <- apply(y2_pred, 2, mean)
  mse_poe1 <- mean((y_m_test[[1]] - y1_pred_mean)^2)
  mse_poe2 <- mean((y_m_test[[2]] - y2_pred_mean)^2)

  # Full data set estiamtion and prediction
  soil_mvm <- new("MultivariateVecchiaModel", n_neighbors = 10)
  soil_mvm <- prestogp_fit(
    soil_mvm,
    y_m_train,
    x_m_train,
    locs_m_train,
    quiet = TRUE,
    verbose = TRUE
  )

  mvm_pred <- PrestoGP::prestogp_predict(
    soil_mvm,
    x_m_test,
    locs_m_test,
    return.values = "mean"
  )

  y1_pred_mvm <- mvm_pred$means$Y1
  y2_pred_mvm <- mvm_pred$means$Y2
  mse_mvm1 <- mean((y_m_test[[1]] - y1_pred_mvm)^2)
  mse_mvm2 <- mean((y_m_test[[2]] - y2_pred_mvm)^2)

  df <- data.frame(
    "beta1" = c(y1_betas[, 1], y2_betas[, 1]),
    "beta2" = c(y1_betas[, 2], y2_betas[, 2]),
    "beta3" = c(y1_betas[, 3], y2_betas[, 3]),
    "beta4" = c(y1_betas[, 4], y2_betas[, 4]),
    "theta1" = c(y1_thetas[, 1], y2_thetas[, 1]),
    "theta2" = c(log10(y1_thetas[, 2]), log10(y2_thetas[, 2])),
    "theta3" = c(y1_thetas[, 3], y2_thetas[, 3]),
    "theta4" = c(y1_thetas[, 4], y2_thetas[, 4]),
    "theta5" = c(y1_thetas[, 5], y2_thetas[, 5]),
    "Y" = c(rep(1, b), rep(2, b))
  ) |>
    pivot_longer(
      cols = starts_with("beta") | starts_with("theta"),
      names_to = "X",
      values_to = "Value"
    )

  p1 <- ggplot(df) +
    geom_density(aes(x = Value, color = as.factor(Y)), linewidth = 1.1) +
    facet_wrap(. ~ X, scales = "free") +
    theme_bw()

  # Calculate that the single model is wiwthin the 95% CI of the bagged model
  library(dplyr)

  # Calculate 99.9% confidence intervals for each group in df
  ci_df <- df %>%
    group_by(X) %>%
    summarize(
      lower = quantile(Value, 0.001),
      upper = quantile(Value, 0.999)
    )

  # Merge df_single with the confidence intervals
  df_single_with_ci <- df_single %>%
    left_join(ci_df, by = "X")

  # Check if df_single values fall within the 95% CI
  df_single_with_ci <- df_single_with_ci %>%
    mutate(
      coverage = Value >= lower & Value <= upper
    )

  # A few checks on the prestogp_fit object
  expect_true(validObject(soil_mvm))
  expect_equal(get_Y(soil_mvm), y_m)
  # Check ggplot object
  expect_s3_class(p1, "ggplot")
  expect_true(all(df_single_with_ci$coverage))
})

testthat::test_that("PrestoGP with censoring 
  out-of-sample MSE on Soil Data", {
})

testthat::test_that("Tobit-Net out-of-sample MSE on Soil Data", {
})

testthat::test_that("PrestogP-PoE (simple) with censoring 
  out-of-sample MSE on Soil Data", {
})
