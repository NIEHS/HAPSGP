########################  model fit and predict   ##########################
target_fit <-
  list(
    # targets::tar_target(
    #     name = model_fit,
    #    command = pgp_fit(data=dt_feat_calc_xyt,
    #                     dates=model_dates,
    #                     vars=vars, logscale=TRUE),
    #     description = "PGP fit on full data"
    # ),
    targets::tar_target(
      name = pgp_cleanup_nc,
      command = pgp_preprocessing(
        data = covariates_nc
      ),
      description = "Clean up HAPS data for PGP fit"
    ),
    targets::tar_target(
      name = model_fit_nc,
      command = pgp_fit(
        data = pgp_cleanup_nc,
        dates = model_dates,
        vars = vars,
        logscale = TRUE
      )
    ),
    targets::tar_target(
      name = pgp_crossvalidation_nc,
      command = pgp_cv(
        data = pgp_cleanup_nc,
        dates = model_dates,
        vars = vars,
        logscale = TRUE,
        cv_method = "random",
        cv_splits = 5
      )
    )
  )
