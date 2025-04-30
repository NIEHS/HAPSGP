########################  model fit and predict   ##########################
target_soil_example <-
  list(
    targets::tar_target(
      name = mc_iter,
      command = 1:100,
      description = "soil | example | MC | 1:100"
    ),
    targets::tar_target(
      name = soil_mc,
      command = {
        data(soil)
        soil <- soil[!is.na(soil[, 5]), ] # remove rows with NA's
        locs <- as.matrix(soil[, 1:2])
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

        results <- list(
          "y_m_train" = y_m_train,
          "y_m_test" = y_m_test,
          "x_m_train" = x_m_train,
          "x_m_test" = x_m_test,
          "locs_m_train" = locs_m_train,
          "locs_m_test" = locs_m_test
        )
      },
      pattern = map(mc_iter),
      iteration = "list",
      description = "soil | example | MC"
    ),
    targets::tar_target(
      name = soil_prestogp_model,
      command = {
        soil_mvm <- new("MultivariateVecchiaModel", n_neighbors = 10)
        soil_mvm <- prestogp_fit(
          soil_mvm,
          soil_mc$y_m_train,
          soil_mc$x_m_train,
          soil_mc$locs_m_train,
          quiet = TRUE,
          verbose = TRUE
        )

        mvm_pred <- PrestoGP::prestogp_predict(
          soil_mvm,
          soil_mc$x_m_test,
          soil_mc$locs_m_test,
          return.values = "mean"
        )

        mvm_mse_1 <- mean(
          (soil_mc$y_m_test[[1]] - mvm_pred$means$Y1)^2
        )
        mvm_mse_2 <- mean(
          (soil_mc$y_m_test[[2]] - mvm_pred$means$Y2)^2,
        )
        mvm_mse <- c(mvm_mse_1, mvm_mse_2)
        mvm_mse
      },
      pattern = map(soil_mc),
      iteration = "list",
      description = "soil | example |  MC | prestogp model",
    ),
    targets::tar_target(
      name = soil_prestogp_poe_model,
      command = {
        b <- 100

        y1_pred <- matrix(NA, nrow = b, ncol = length(soil_mc$y_m_test[[1]]))
        y2_pred <- matrix(NA, nrow = b, ncol = length(soil_mc$y_m_test[[2]]))

        for (i in seq_len(b)) {
          print(i)
          y_w <- list()
          x_w <- list()
          locs_w <- list()
          int1 <- sample.int(
            length(soil_mc$y_m_train[[1]]),
            145,
            replace = FALSE
          )
          int2 <- sample.int(
            length(soil_mc$y_m_train[[2]]),
            145,
            replace = FALSE
          )
          y_w[[1]] <- soil_mc$y_m_train[[1]][int1]
          y_w[[2]] <- soil_mc$y_m_train[[2]][int2]
          x_w[[1]] <- soil_mc$x_m_train[[1]][int1, ]
          x_w[[2]] <- soil_mc$x_m_train[[1]][int2, ]
          locs_w[[1]] <- soil_mc$locs_m_train[[1]][int1, , drop = FALSE]
          locs_w[[2]] <- soil_mc$locs_m_train[[2]][int2, , drop = FALSE]

          soil_poe <- new("MultivariateVecchiaModel", n_neighbors = 10)

          soil_poe <- prestogp_fit(
            soil_poe,
            y_w,
            x_w,
            locs_w,
            quiet = TRUE,
            verbose = TRUE
          )

          # Do the prediction on the test set
          soil_pred <- prestogp_predict(
            soil_poe,
            soil_mc$x_m_test,
            soil_mc$locs_m_test,
            return.values = "mean"
          )

          y1_pred[i, ] <- soil_pred$means$Y1
          y2_pred[i, ] <- soil_pred$means$Y2
        }
        #
        # PoE prediction
        y1_pred_mean <- apply(y1_pred, 2, mean)
        y2_pred_mean <- apply(y2_pred, 2, mean)
        mse_poe1 <- mean((soil_mc$y_m_test[[1]] - y1_pred_mean)^2)
        mse_poe2 <- mean((soil_mc$y_m_test[[2]] - y2_pred_mean)^2)
        mse_pgp_poe <- c(mse_poe1, mse_poe2)
        mse_pgp_poe
      },
      pattern = map(soil_mc),
      iteration = "list",
      description = "soil | example |  MC | PoE model",
    ),
    targets::tar_target(
      name = soil_relaxo_model,
      command = {
        mdl1 <- glmnet::cv.glmnet(
          x = soil_mc$x_m_train[[1]],
          y = soil_mc$y_m_train[[1]],
          alpha = 1,
          relax = TRUE,
          nfolds = 5
        )
        pred1 <- predict(
          mdl1,
          newx = soil_mc$x_m_test[[1]],
          s = "lambda.1se"
        )

        mdl2 <- glmnet::cv.glmnet(
          x = soil_mc$x_m_train[[2]],
          y = soil_mc$y_m_train[[2]],
          alpha = 1,
          relax = TRUE,
          nfolds = 5
        )
        pred2 <- predict(
          mdl2,
          newx = soil_mc$x_m_test[[2]],
          s = "lambda.1se"
        )

        lasso_mse_1 <- mean(
          (soil_mc$y_m_test[[1]] - pred1)^2
        )
        lasso_mse_2 <- mean(
          (soil_mc$y_m_test[[2]] - pred2)^2,
        )
        lasso_mse <- c(lasso_mse_1, lasso_mse_2)
        lasso_mse
      },
      pattern = map(soil_mc),
      iteration = "list",
      description = "soil | example |  MC | lasso model",
    ),

    targets::tar_target(
      name = soil_OK_model,
      command = {
        mean_trend1 <- mean(soil_mc$y_m_train[[1]])
        mean_trend2 <- mean(soil_mc$y_m_train[[2]])
        mean_trend <- list(mean_trend1, mean_trend2)
        res1 <- soil_mc$y_m_train[[1]] - mean_trend1
        res2 <- soil_mc$y_m_train[[2]] - mean_trend2
        res <- list(res1, res2)
        # Fit the model
        init1 <- c(log(var(res1) * 0.9), 1, log(var(res1) * 0.1), 0.5)
        init2 <- c(log(var(res2) * 0.9), 1, log(var(res2) * 0.1), 0.5)

        mdl1 <- optim(
          init1,
          negloglik_full_spatial,
          method = "Nelder-Mead",
          locs = soil_mc$locs_m_train[[1]],
          y = res1,
          sample_size = length(res1)
        )
        mdl2 <- optim(
          init2,
          negloglik_full_spatial,
          method = "Nelder-Mead",
          locs = soil_mc$locs_m_train[[2]],
          y = res2,
          sample_size = length(res2)
        )

        mdl <- list(mdl1, mdl2)

        # Predict the test set
        for (i in 1:2) {
          # Extract the parameters
          cov_pars <- c(
            exp(mdl[[i]]$par[1:3]),
            gtools::inv.logit(mdl[[i]]$par[4], 0, 2.5)
          )

          d_oo <- fields::rdist(soil_mc$locs_m_train[[i]])
          d_op <- fields::rdist(
            soil_mc$locs_m_test[[i]],
            soil_mc$locs_m_train[[i]]
          )
          # sigma_oo <- cov_pars[3] *
          #   diag(nrow(soil_mc$locs_m_train[[i]])) +
          #   cov_pars[1] *
          #     fields::Matern(
          #       d_oo,
          #       range = cov_pars[2],
          #       smoothness = cov_pars[4]
          sigma_oo <- cov_pars[1] *
            fields::Matern(
              d_oo,
              range = cov_pars[2],
              smoothness = cov_pars[4]
            )
          sigma_op <- cov_pars[1] *
            fields::Matern(d_op, range = cov_pars[2], smoothness = cov_pars[4])
          mean_trend_test <- mean(soil_mc$y_m_test[[i]])
          pred <- mean_trend_test + (sigma_op) %*% solve(sigma_oo, res[[i]])
        }
      },
      pattern = map(soil_mc),
      iteration = "list",
      description = "soil | example |  MC | lasso model",
    ),

    targets::tar_target(
      name = "summary_soil_results",
      command = {
        pgp <- soil_prestogp_model |>
          as.data.frame() |>
          t() |>
          colMeans()
        pgp_poe <- soil_prestogp_poe_model |>
          as.data.frame() |>
          t() |>
          colMeans()
        relaxo <- soil_relaxo_model |>
          as.data.frame() |>
          t() |>
          colMeans()
        soil_results <- data.frame(
          pgp = pgp,
          pgp_poe = pgp_poe,
          relaxo = relaxo
        )
        soil_results
      },
      description = "soil  | example  |  MC  | results"
    )
  )
