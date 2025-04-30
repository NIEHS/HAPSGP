library(PrestoGP)
library(targets)
library(ggplot2)
library(tidyverse)

soil_mc <- tar_read(
  soil_mc,
  store = "/ddn/gs1/home/messierkp/projects/HAPSGP/_targets/"
)
b <- 100

for (k in seq_len(length(soil_mc))) {
  print(k)

  y1_pred <- matrix(NA, nrow = b, ncol = length(soil_mc[[k]]$y_m_test[[1]]))
  y2_pred <- matrix(NA, nrow = b, ncol = length(soil_mc[[k]]$y_m_test[[2]]))

  for (i in seq_len(b)) {
    print(i)
    y_w <- list()
    x_w <- list()
    locs_w <- list()
    int1 <- sample.int(
      length(soil_mc[[k]]$y_m_train[[1]]),
      80,
      replace = FALSE
    )
    int2 <- sample.int(
      length(soil_mc[[k]]$y_m_train[[2]]),
      80,
      replace = FALSE
    )
    y_w[[1]] <- soil_mc[[k]]$y_m_train[[1]][int1]
    y_w[[2]] <- soil_mc[[k]]$y_m_train[[2]][int2]
    x_w[[1]] <- soil_mc[[k]]$x_m_train[[1]][int1, ]
    x_w[[2]] <- soil_mc[[k]]$x_m_train[[1]][int2, ]
    locs_w[[1]] <- soil_mc[[k]]$locs_m_train[[1]][int1, , drop = FALSE]
    locs_w[[2]] <- soil_mc[[k]]$locs_m_train[[2]][int2, , drop = FALSE]

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
    soil_pred <- PrestoGP::prestogp_predict(
      soil_poe,
      soil_mc[[k]]$x_m_test,
      soil_mc[[k]]$locs_m_test,
      return.values = "mean",
      m = 3
    )
  }
}
