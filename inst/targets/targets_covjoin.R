#######################    Join and impute covariates    ###########################
#nolint start
target_covjoin <-
  list(
    targets::tar_target(
      covariates_full,
      command=join_covariates(gmet_process,dt_feat_calc_tri,dt_feat_calc_nlcd),
      description = "join all covariate data"
  )#,
 # targets::tar_target(
 #     covariates_imputed,
      #command = beethoven::impute_all(
      #  covariates_full,
      #  period = chr_daterange,
      #  nthreads_dt = 32,
      #  nthreads_collapse = 32,
      #  nthreads_imputation = 32
  #    command= 
  #     missRanger::missRanger(
  #      data = covariates_full,
  #      maxiter = 10L,
  #      num.trees = 300L,
  #      num.threads = 32,
  #      mtry = 100L,
  #      sample.fraction = 0.1
  #    ),
  #    description = "Imputed features + HAPS data"
  #  )
  )