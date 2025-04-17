########################  covariates?   ##########################
#nolint start
target_predgrid <-
  list(
    targets::tar_target(
      name = pred_grid_coarse,
      command = "output/AGU/gridmet_coarsegrid_2021.rds",
      description = "Coarse Prediction grid"
    ),
    targets::tar_target(
      pred_grid,
      command = make_grid_state(state_list = c("Texas"), grid_size <- 20000),
      description = "make prediction grid"
    ),
    targets::tar_target(
      gmet_process_pred,
      command = gridmet_process(
        data = pred_grid,
        dates_gridmet = model_dates,
        input_dir = "input/covariates/gridmet/",
        variables_gridmet = variables_gridmet,
        radiuses = buffer_radius,
        output = "covonly",
        haps_locs = pred_grid
      ),
      #pattern=cross(variables_gridmet,model_dates),
      description = "gridmet on prediction grid"
    ),
    ###########################      ECOREGIONS      ###########################

    ###########################      TRI/SEDC      ###########################
    targets::tar_target(
      list_feat_calc_tri_pred,
      command = {
        download_tri
        beethoven::inject_calculate(
          covariate = "tri",
          locs = pred_grid,
          # NOTE: locs are all AQS sites for computational efficiency
          injection = list(
            locs_id = "AMA_SITE_CODE",
            domain = df_feat_calc_tri_params$year,
            domain_name = "year",
            path = file.path(chr_input_dir, "tri"),
            variables = c(1, 13, 12, 14, 20, 34, 36, 47, 48, 49),
            radius = df_feat_calc_tri_params$radius,
            nthreads = 1,
            covariate = "tri"
          )
        )
      },
      iteration = "list",
      pattern = map(df_feat_calc_tri_params),
      description = "TRI features for pred"
    ),
    targets::tar_target(
      dt_feat_calc_tri_pred,
      command = {
        for (i in 1:(length(list_feat_calc_tri_pred) - 1)) {
          tribr2 = list_feat_calc_tri_pred[[i + 1]][[1]]
          print(i)
          if (i == 1) {
            tribr = list_feat_calc_tri_pred[[1]][[1]]
            merged = beethoven::reduce_merge(
              list(tribr, tribr2),
              by = NULL,
              all.y = T
            )
          } else {
            merged = beethoven::reduce_merge(
              list(merged, tribr2),
              by = NULL,
              all.y = T
            )
          }
        }
        return(merged)
      },
      #command = beethoven::reduce_merge(
      #  lapply(
      #    list_feat_calc_tri,
      #    function(x) data.table::data.table(beethoven::reduce_list(x)[[1]])
      #  ),
      #  by = NULL,
      #  all.y=TRUE
      #),
      description = "data.table TRI pred"
    ),
    ###########################         NLCD         ###########################
    targets::tar_target(
      list_feat_calc_nlcd_pred,
      command = {
        download_nlcd
        beethoven::inject_nlcd(
          locs = pred_grid,
          # NOTE: locs are all AQS sites for computational efficiency
          locs_id = "AMA_SITE_CODE",
          year = df_feat_calc_nlcd_params$year,
          radius = df_feat_calc_nlcd_params$radius,
          from = amadeus::process_nlcd(
            path = file.path(chr_input_dir, "nlcd", "data_files"),
            year = df_feat_calc_nlcd_params$year
          ),
          nthreads = 1,
          mode = "exact",
          max_cells = 3e7
        )
      },
      iteration = "list",
      pattern = map(df_feat_calc_nlcd_params),
      description = "Calculate NLCD for pred"
    ),
    targets::tar_target(
      name = dt_feat_calc_nlcd_pred,
      command = list_feat_calc_nlcd_pred %>%
        collapse::rowbind(fill = TRUE) %>%
        collapse::funique() %>%
        collapse::pivot(
          ids = c("AMA_SITE_CODE", "time"),
          values = names(.)[
            !names(.) %in%
              c(
                "AMA_SITE_CODE",
                "time"
              )
          ]
        ) %>%
        .[!is.na(.[["value"]]), ] %>%
        collapse::pivot(
          ids = c("AMA_SITE_CODE", "time"),
          values = c("value"),
          how = "wider"
        ),
      description = "NLCD pred list"
    ),
    #######################    Join and impute covariates    ###########################
    targets::tar_target(
      covariates_full_pred,
      command = join_covariates(
        as.data.table(gmet_process_pred),
        dt_feat_calc_tri_pred,
        dt_feat_calc_nlcd_pred
      ),
      description = "join prediction covariate data"
    ),
    targets::tar_target(
      filter_covs_pred,
      command = {
        # Identify columns where all values are 0 or NA
        cols_to_keep <- !sapply(
          covariates_full_pred,
          function(col) all(is.na(col) | col == 0)
        )
        # Subset the data.table
        dt_filtered <- covariates_full_pred[, ..cols_to_keep]
        return(dt_filtered)
      }
    ),
    #targets::tar_target(
    #    covariates_imputed_pred,
    #command = beethoven::impute_all(
    #  covariates_full,
    #  period = chr_daterange,
    #  nthreads_dt = 32,
    #  nthreads_collapse = 32,
    #  nthreads_imputation = 32
    #   command=
    #    missRanger::missRanger(
    #     data = filter_covs_pred,
    #     maxiter = 10L,
    #     num.trees = 300L,
    #     num.threads = 32,
    #     mtry = 50L,
    #     sample.fraction = 0.1
    #   ),
    #   description = "Imputed features + HAPS data"
    # ),
    targets::tar_target(
      covs_fill_na_pred,
      command = {
        pattern <- "FUGITIVE|STACK"
        data_tri <- filter_covs_pred[,
          grep(pattern, names(filter_covs_pred)),
          with = FALSE
        ]
        data_trim <- filter_covs_pred[,
          !grep(pattern, names(filter_covs_pred)),
          with = FALSE
        ]
        data_tri[,
          (names(data_tri)) := lapply(
            .SD,
            function(col) fifelse(is.na(col), 0, col)
          )
        ]
        data_return <- data.table::data.table(cbind(data_trim, data_tri))
        return(data_return)
      }
    ),
    targets::tar_target(
      covs_pctri_pred,
      command = post_calc_pca(
        data = covs_fill_na_pred,
        locs_id = "AMA_SITE_CODE",
        time_id = "time",
        coords = c("lon", "lat"),
        threshold = 0.99,
        pattern = "FUGITIVE|STACK",
        groups = NULL,
        prefix = "TRI"
      )
    ),
    targets::tar_target(
      name = prediction_nc,
      command = pgp_pred(
        pred_dates = pred_dates,
        fullmodel2 = "model_fit_nc_02202025.RDS",
        pred_grid = covs_pctri_pred,
        vars = vars
      ),
      pattern = map(pred_dates)
    )
  )
#nolint end
