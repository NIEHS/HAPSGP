########################  model fit and predict   ##########################
target_fit <-
  list(
    # targets::tar_target(
    #     name = model_fit,
    #    command = pgp_fit(data=xyt_reduce,
    #                     dates=model_dates,
    #                     vars=vars, logscale=TRUE),
    #     description = "PGP fit on full USA dataset"
    # ),
    targets::tar_target(
      name = pgp_cleanup_state,
      command = pgp_preprocessing(
        data = covariates_state
      ),
      description = "Clean up state HAPS + covariate data for PGP fit"
    ),
      targets::tar_target(
         name = model_fit_state,
         command = pgp_fit(
           data = pgp_cleanup_state,
           dates = model_dates,
           vars = vars,
           logscale = TRUE
        )
       ),
    targets::tar_target(
      name = pgp_crossvalidation_state_random,
      command = pgp_cv(
        data = pgp_cleanup_state,
        dates = model_dates,
        vars = vars,
        logscale = FALSE,
        cv_method = "random",
        cv_splits = 5
      )
    ),
    targets::tar_target(
      name = pgp_crossvalidation_state_spatrandom,
      command = pgp_cv(
        data = pgp_cleanup_state,
        dates = model_dates,
        vars = vars,
        logscale = FALSE,
        cv_method = "spatialrandom",
        cv_splits = 5
      )
    ),
    targets::tar_target(
      name = pgp_crossvalidation_state_spatsnake,
      command = pgp_cv(
        data = pgp_cleanup_state,
        dates = model_dates,
        vars = vars,
        logscale = FALSE,
        cv_method = "spatialsnake",
        cv_splits = 5
      )
    ),
    targets::tar_target(
     name = prediction_state,
     command = pgp_pred(
     pred_dates = pred_dates,
     fullmodel = model_fit_state,
     pred_grid =  covariates_state_pred,
     vars = vars
     ),
      pattern = map(pred_dates),
      description = "Prediction on state grid"
    )
  )
  )
