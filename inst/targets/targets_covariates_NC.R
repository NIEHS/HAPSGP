########################  covariates?   ##########################
#nolint start
target_covariates_nc <-
  list(
    targets::tar_target(
      haps_locs_nc,
      command = select_states(locs=haps_locs,state_list=c("Texas")),
      description="Extract NC locations"
     )
     ,
    targets::tar_target(
        covariates_nc,
        command=covariates_full %>% filter(AMA_SITE_CODE %in% haps_locs_nc$AMA_SITE_CODE),
        description="Filter NC covariates"
    ),
    targets::tar_target(
        filter_covs_nc,
        command={
        # Identify columns where all values are 0 or NA
        cols_to_keep <- !sapply(covariates_nc, function(col) all(is.na(col) | col == 0))
        # Subset the data.table
        dt_filtered <- covariates_nc[, ..cols_to_keep]
        return(dt_filtered)
        }
    ),
    targets::tar_target(
      covariates_imputed_nc,
      command= 
       missRanger::missRanger(
        data = filter_covs_nc,
        maxiter = 10L,
        num.trees = 300L,
        num.threads = 32,
        mtry = 100L,
        sample.fraction = 0.1
      ),
      description = "Imputed covariates NC"
    ),
    targets::tar_target(
      covs_pctri_nc,
      command=
      post_calc_pca(
        data=covariates_imputed_nc,
        locs_id = "AMA_SITE_CODE",
        time_id = "time",
        yvar = "CONC_DAILY_STD",
        coords = c("lon", "lat"),
        threshold = 0.99,
        pattern = "FUGITIVE|STACK",
        groups = NULL,
        prefix = "TRI"
        ) 
    )
  
  )