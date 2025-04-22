# Compare validation metrics for different cross-validation methods
library(targets)
library(Metrics)

# Helper function to evaluate one CV object
evaluate_cv <- function(cv_obj, log_scale = FALSE) {
  ytest.list <- cv_obj$ytest.list
  pred.list <- cv_obj$pred.list

  metrics <- data.frame(
    nrmse_Benzene = numeric(5),
    nrmse_Hexane = numeric(5),
    r2_Benzene = numeric(5),
    r2_Hexane = numeric(5),
    tau_Benzene = numeric(5),
    tau_Hexane = numeric(5)
  )

  for (i in 1:5) {
    yvec <- ytest.list[[i]]
    pvec <- pred.list[[i]][[1]]

    y1 <- yvec[[1]]
    y2 <- yvec[[2]]
    p1 <- pvec[[1]]
    p2 <- pvec[[2]]

    if (log_scale) {
      y1 <- exp(y1)
      y2 <- exp(y2)
      p1 <- exp(p1)
      p2 <- exp(p2)
    }

    metrics[i, "nrmse_Benzene"] <- rmse(y1, p1) / mean(y1)
    metrics[i, "nrmse_Hexane"] <- rmse(y2, p2) / mean(y2)
    metrics[i, "r2_Benzene"] <- 1 - sum((y1 - p1)^2) / sum((y1 - mean(y1))^2)
    metrics[i, "r2_Hexane"] <- 1 - sum((y2 - p2)^2) / sum((y2 - mean(y2))^2)
    metrics[i, "tau_Benzene"] <- cor(y1, p1, method = "kendall")
    metrics[i, "tau_Hexane"] <- cor(y2, p2, method = "kendall")
  }

  return(metrics)
}

# Define all CV types and names
cv_methods <- list(
  random = list(obj = tar_read(pgp_crossvalidation_nc_random), log = FALSE),
  randomlog = list(
    obj = tar_read(pgp_crossvalidation_nc_randomlog),
    log = TRUE
  ),
  spatrandom = list(
    obj = tar_read(pgp_crossvalidation_nc_spatrandom),
    log = FALSE
  ),
  spatrandomlog = list(
    obj = tar_read(pgp_crossvalidation_nc_spatrandomlog),
    log = TRUE
  ),
  spatsnake = list(
    obj = tar_read(pgp_crossvalidation_nc_spatsnake),
    log = FALSE
  ),
  spatsnakelog = list(
    obj = tar_read(pgp_crossvalidation_nc_spatsnakelog),
    log = TRUE
  )
)

# Evaluate all methods and combine results
results <- lapply(names(cv_methods), function(name) {
  method_info <- cv_methods[[name]]
  metrics <- evaluate_cv(method_info$obj, log_scale = method_info$log)
  metrics$method <- name
  return(metrics)
})

# Combine and reorder columns
results_df <- do.call(rbind, results)
results_df <- results_df[, c(
  "method",
  "nrmse_Benzene",
  "nrmse_Hexane",
  "r2_Benzene",
  "r2_Hexane",
  "tau_Benzene",
  "tau_Hexane"
)]

# Print the result
View(results_df)
