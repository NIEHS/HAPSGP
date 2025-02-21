########################  model fit and predict   ##########################
target_fit <-
  list(   
    targets::tar_target(
        name = pred_grid_full,
       command = "output/AGU/gridmet_coarsegrid_2021.rds",
       description="Prediction grid"
    ),
    
   # targets::tar_target(
   #     name = model_fit,
   #    command = pgp_fit(data=covariates_imputed,
   #                     dates=model_dates, 
   #                     vars=vars)
   # ),
    targets::tar_target(
        name = model_fit_nc,
       command = pgp_fit(data=covs_pctri_nc,
                        dates=model_dates, 
                        vars=vars)
    ),
   targets::tar_target(
      name = pgp_crossvalidation_nc,
      command = pgp_cv(data=covs_pctri_nc,
                        dates=model_dates, 
                        vars=vars, 
                      cv_splits=5)
    )
)