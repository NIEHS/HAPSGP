# Try imputation separately
library(targets)
data=tar_read("filter_covs_pred")
pattern = "FUGITIVE|STACK"

data_tri <- data[
    , grep(pattern, names(data)), with = FALSE
  ]
   data_trim <- data[
    , !grep(pattern, names(data)), with = FALSE
  ]
covs_imputed=missRanger::missRanger(
        data = data_tri,
        maxiter = 10L,
        num.trees = 300L,
       # num.threads = 32,
       # mtry = 20L,
        sample.fraction = 0.1
      )

data_return <- data.table::data.table(cbind(data_trim, covs_imputed))

saveRDS(data_return,"covs_imputed_pred.RDS")      


