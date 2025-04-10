########################  covariates   ##########################
#nolint start
target_covariates_predgrid <-
  list(
        targets::tar_target(
        pred_grid,
        command=make_grid_state(state_list=c("Texas"),
        grid_size <- 20000 ),
        description="make prediction grid"
    ),
###########################     GRIDMET      ###########################
     targets::tar_target(
      gmet_process_pred,
      command = gridmet_process(data=pred_grid,
      dates_gridmet=model_dates, input_dir="input/covariates/gridmet/",
      variables_gridmet= variables_gridmet, radiuses=buffer_radius, output="covonly",
      haps_locs=pred_grid),
      #pattern=cross(variables_gridmet,model_dates),
      description="gridmet on prediction grid"
     )
    ,  
    targets::tar_target(
      gmet_cleanup_pred,
      command=gridmet_cleanup(gmet_process_pred,vars),
        description="gridmet columns cleanup"
     ),
    ###########################      ECOREGIONS      ###########################
    targets::tar_target(
      dt_feat_calc_ecoregions_pred,
      command = {
        download_ecoregions
        data.table::data.table(
          amadeus::calculate_ecoregion(
            from = amadeus::process_ecoregion(
              path = file.path(
                chr_input_dir,
                "ecoregions",
                "data_files",
                "us_eco_l3_state_boundaries.shp"
              )
            ),
            radius=100,
            locs = pred_grid,
            locs_id = "AMA_SITE_CODE"
          )
        )
      },
      description = "data.table of Ecoregions features | predict"
    )
    ,
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
      description = "Calculate TRI features"
    )
    ,
    targets::tar_target(
      list_feat_reduce_tri_pred,
      command = {
        list_feat_calc_tri_unnest_pred <- lapply(
          list_feat_calc_tri_pred,
          function(x) x[[1]]
        )
        chr_tri_radii_index_pred <- sapply(
          list_feat_calc_tri_unnest_pred,
          function(x) {
            any(grepl(sprintf("_%05d", chr_iter_radii_tri), names(x)))
          }
        )
        beethoven::reduce_merge(
          list_feat_calc_tri_unnest_pred[chr_tri_radii_index_pred],
          by = NULL,
          all.x = TRUE,
          all.y = TRUE
        )
      },
      iteration = "list",
      pattern = map(chr_iter_radii_tri),
      description = "Reduce TRI features based on radii | predict"
    )
    ,
    targets::tar_target(
      dt_feat_calc_tri_pred,
      command = {
        dt_feat_merge_tri_pred <- beethoven::reduce_merge(
          list_feat_reduce_tri_pred[1:2],
          by = c("AMA_SITE_CODE", "time", "lon", "lat", "tri_year"),
          all.x = TRUE,
          all.y = TRUE
        )
        dt_feat_merge_tri_pred[is.na(dt_feat_merge_tri_pred)] <- 0
        dt_feat_pca_tri_pred <- beethoven::post_calc_pca(
          locs_id = "AMA_SITE_CODE",
          data = dt_feat_merge_tri_pred,
          yvar = NULL,
          num_comp = 5,
          pattern = "FUGITIVE|STACK",
          groups = sprintf("%05d", chr_iter_radii_tri),
          prefix = "TRI",
          kernel = TRUE
        )
      },
      description = "data.table of TRI PCA-reduced features | predict"
    )
    ,
      #command = beethoven::reduce_merge(
      #  lapply(
      #    list_feat_calc_tri,
      #    function(x) data.table::data.table(beethoven::reduce_list(x)[[1]])
      #  ),
      #  by = NULL,
      #  all.y=TRUE
      #),
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
      description = "Calculate NLCD features | predict"
    )
    ,
    targets::tar_target(
      name = dt_feat_calc_nlcd_pred,
      command = list_feat_calc_nlcd_pred %>%
        collapse::rowbind(fill = TRUE) %>%
        collapse::funique() %>%
        collapse::pivot(
          ids = c("AMA_SITE_CODE", "time"),
          values = names(.)[!names(.) %in% c(
            "AMA_SITE_CODE",
            "time"
          )]
        ) %>%
        .[!is.na(.[["value"]]),] %>%
        collapse::pivot(
          ids = c("AMA_SITE_CODE", "time"),
          values = c("value"),
          how = "wider"
        ),
      description = "NLCD feature list (all dt) | predict"
    ),
      ###########################      POPULATION      ###########################
    targets::tar_target(
      list_feat_calc_pop_pred,
      command = {
        download_population
        amadeus::calculate_population(
          from = amadeus::process_population(
            path = file.path(
              chr_input_dir,
              "population",
              "data_files",
              paste0(
                "gpw_v4_population_density_adjusted_to_",
                "2015_unwpp_country_totals_rev11_2020_30_sec.tif"
              )
            )
          ),
          locs = pred_grid,
          locs_id = "AMA_SITE_CODE",
          geom = FALSE,
          radius = chr_iter_radii
        )
      },
      pattern = map(chr_iter_radii),
      iteration = "list",
      description = "Calculate population features | predict"
    )
    ,
    targets::tar_target(
      dt_feat_calc_pop_pred,
      command = beethoven::reduce_merge(
        beethoven::reduce_list(list_feat_calc_pop_pred),by=NULL
      ),
      description = "data.table of population features | predict"
    ),
    ###########################        GROADS        ###########################
    targets::tar_target(
      list_feat_calc_groads_pred,
      command = {
        download_groads
        amadeus::calculate_groads(
          from = amadeus::process_groads(
            path = file.path(
              chr_input_dir,
              "groads",
              "data_files",
              "gROADS-v1-americas.gdb"
            )
          ),
          locs = pred_grid,
          locs_id = "AMA_SITE_CODE",
          radius = chr_iter_radii
        )
      },
      iteration = "list",
      pattern = map(chr_iter_radii),
      description = "Calculate gRoads features | predict"
    )
    ,
    targets::tar_target(
      dt_feat_calc_groads_pred,
      command = beethoven::reduce_merge(
        beethoven::reduce_list(list_feat_calc_groads_pred),
        by = c("AMA_SITE_CODE", "description")
      ),
      description = "data.table of gRoads features | predict"
    ) ,
    ###########################        KOPPEN        ###########################
    targets::tar_target(
    dt_feat_calc_koppen_pred,
    command = {
      download_koppen
      data.table::data.table(
        amadeus::calculate_koppen_geiger(
          from = amadeus::process_koppen_geiger(
            path = file.path(
              chr_input_dir,
              "koppen_geiger",
              "data_files",
              "Beck_KG_V1_present_0p0083.tif"
            )
          ),
          locs = pred_grid,
          # NOTE: locs are all AQS sites for computational efficiency
          locs_id = "AMA_SITE_CODE",
          geom = FALSE
        )
      )
    },
    description = "data.table of Koppen Geiger features | predict"
  ),
  ###########################         NEI          ###########################
    targets::tar_target(
    list_feat_calc_ne_pred,
    command = {
      download_nei
      beethoven::inject_calculate(
        covariate = "nei",
        locs = pred_grid,
        injection = list(
          domain = chr_iter_calc_nei,
          domain_name = "year",
          path = file.path(chr_input_dir, "nei", "data_files"),
          covariate = "nei"
        )
      )
    },
    iteration = "list",
    pattern = map(chr_iter_calc_nei),
    description = "Calculate NEI features | predict"
  )
  ,
  targets::tar_target(
    dt_feat_calc_nei_pred,
   # command = beethoven::reduce_merge(
   #     beethoven::reduce_list(list_feat_calc_nei),by=NULL
   #   ),
    command = beethoven::reduce_list(
      lapply(list_feat_calc_nei_pred,function(x) data.table::data.table(beethoven::reduce_list(x)[[1]])))[[1]],
    description = "data.table of NEI features | predict"
  ),
  ###########################         GMTED        ###########################
    targets::tar_target(
    list_feat_calc_gmted_pred,
    command = {
      download_gmted
      beethoven::calc_gmted_direct(
        variable = c(chr_iter_calc_gmted_vars, "7.5 arc-seconds"),
        path = file.path(chr_input_dir, "gmted", "data_files"),
        locs = pred_grid,
        locs_id = "AMA_SITE_CODE",
        radius = chr_iter_calc_gmted_radii
      )
    },
    iteration = "list",
    pattern = cross(
      chr_iter_calc_gmted_vars,
      chr_iter_calc_gmted_radii
    ),
    description = "Calculate GMTED features | predict"
  )
  ,
  targets::tar_target(
    dt_feat_calc_gmted_pred,
    command = beethoven::reduce_merge(
      beethoven::reduce_list(list_feat_calc_gmted_pred), by = "AMA_SITE_CODE"
    ),
    description = "data.table of GMTED features | predict"
  ),
  ###########################         HMS          ###########################
  targets::tar_target(
    list_feat_calc_hms_pred,
    command = {
      download_hms_buffer
      dt_iter_calc_hms <- beethoven::inject_calculate(
        covariate = "hms",
        locs = pred_grid,
        injection = list(
          locs_id = "AMA_SITE_CODE",
          path = file.path(chr_input_dir, "hms", "data_files"),
          date = beethoven::fl_dates(unlist(list_dates)),
          covariate = "hms"
        )
      )[[1]] |>
        dplyr::select(-dplyr::any_of(c("lon", "lat", "geometry", "hms_year")))
      beethoven::post_calc_cols(
        dt_iter_calc_hms,
        prefix = "HMS_",
        skip=c("AMA_SITE_CODE", "time")
      )
    },
    pattern = map(list_dates),
    iteration = "list",
    description = "Calculate HMS features | predict"     
  )
  ,
  targets::tar_target(
    dt_feat_calc_hms_pred,
    command = beethoven::reduce_list(list_feat_calc_hms_pred)[[1]],
    description = "data.table of HMS features | predict"
  ),
   ###########################         GEOS         ###########################
   targets::tar_target(
      list_feat_calc_geos_aqc_pred,
      command = {
        download_geos_buffer
        beethoven::calc_geos_strict(
          path = file.path(chr_input_dir, "geos", chr_iter_calc_geos[1]),
          date = beethoven::fl_dates(unlist(list_dates)),
          locs = pred_grid,
          locs_id = "AMA_SITE_CODE"
        )
      },
      pattern = map(list_dates),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100")
      ),
      iteration = "list",
      description = "Calculate GEOS-CF features | aqc | predict"
    )
    ,
    targets::tar_target(
      list_feat_calc_geos_chm_pred,
      command = {
        download_geos_buffer
        beethoven::calc_geos_strict(
          path = file.path(chr_input_dir, "geos", chr_iter_calc_geos[2]),
          date = beethoven::fl_dates(unlist(list_dates)),
          locs = pred_grid,
          locs_id = "AMA_SITE_CODE"
        )
      },
      pattern = map(list_dates),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100")
      ),
      iteration = "list",
      description = "Calculate GEOS-CF features | chm | predict"
    )
    ,
    targets::tar_target(
      dt_feat_calc_geos_pred,
      command = beethoven::reduce_merge(
        c(
          beethoven::reduce_list(list_feat_calc_geos_aqc_pred),
          beethoven::reduce_list(list_feat_calc_geos_chm_pred)
        ),
        by = c("AMA_SITE_CODE", "time", "CO", "NO2", "SO2")
      ),
      description = "data.table of GEOS-CF features | predict"
    ),
  ###########################         NARR         ###########################
    targets::tar_target(
      list_feat_calc_narr_pred,
      command = {
        download_narr_buffer
        dt_iter_calc_narr <- amadeus::calculate_narr(
          from = amadeus::process_narr(
            path = file.path(chr_input_dir, "narr", chr_iter_calc_narr),
            variable = chr_iter_calc_narr,
            date = beethoven::fl_dates(unlist(list_dates))
          ),
          locs = pred_grid,
          locs_id = "AMA_SITE_CODE",
          radius = 0,
          fun = "mean",
          geom = FALSE
        )
        if (length(grep("level", names(dt_iter_calc_narr))) == 1) {
          dt_iter_calc_narr <-
            dt_iter_calc_narr[dt_iter_calc_narr$level == 1000, ]
          dt_iter_calc_narr <-
            dt_iter_calc_narr[, -grep("level", names(dt_iter_calc_narr))]
        }
        beethoven::post_calc_cols(
          dt_iter_calc_narr,
          prefix = "NARR_0_",
          skip=c("AMA_SITE_CODE", "time")
        )
      },
      pattern = cross(list_dates, chr_iter_calc_narr),
      iteration = "list",
      description = "Calculate NARR features | nolag | predict"
    )
    ,
    targets::tar_target(
      dt_feat_calc_narr_nolag_pred,
      command = beethoven::reduce_merge(
        beethoven::reduce_list(list_feat_calc_narr_pred),
        by = c("AMA_SITE_CODE", "time")
      ),
      description = "data.table of NARR features | nolag | predict"
    )
    ,
    targets::tar_target(
      list_feat_calc_narr_lag_pred,
      command = {
        download_narr_buffer
        dt_lag_calc_narr <- amadeus::calculate_narr(
          from = amadeus::process_narr(
            path = file.path(chr_input_dir, "narr", chr_iter_calc_narr_lag),
            variable = chr_iter_calc_narr_lag,
            date = as.character(chr_dates[1] - 1)
          ),
          locs = pred_grid,
          locs_id = "AMA_SITE_CODE",
          radius = 0,
          fun = "mean",
          geom = FALSE
        )
        if (length(grep("level", names(dt_lag_calc_narr))) == 1) {
          dt_lag_calc_narr <-
            dt_lag_calc_narr[dt_lag_calc_narr$level == 1000, ]
          dt_lag_calc_narr <-
            dt_lag_calc_narr[, -grep("level", names(dt_lag_calc_narr))]
        }
        dt_lag_calc_narr <- beethoven::post_calc_cols(
          dt_lag_calc_narr,
          prefix = "NARR_0_",
          skip=c("AMA_SITE_CODE", "time")
        )
        int_narr_cols <- which(
          names(dt_feat_calc_narr_nolag_pred) %in% names(dt_lag_calc_narr)
        )
        dt_iter_calc_narr_bind <- rbind(
          dt_lag_calc_narr,
          dt_feat_calc_narr_nolag_pred[, int_narr_cols, with = FALSE]
        )
        amadeus::calculate_lagged(
          from = dt_iter_calc_narr_bind,
          date = chr_daterange,
          lag = 1,
          locs_id = "AMA_SITE_CODE"
        )
      },
      pattern = map(chr_iter_calc_narr_lag),
      iteration = "list",
      description = "Calculate NARR features | lag | predict"
    )
    ,
    targets::tar_target(
      dt_feat_calc_narr_pred,
      command = beethoven::reduce_merge(
        beethoven::reduce_list(
          c(list(dt_feat_calc_narr_nolag_pred), list_feat_calc_narr_lag_pred)
        ),
        by = c("AMA_SITE_CODE", "time")
      ),
      description = "data.table of NARR features | predict"
    ),
   ###########################       MODIS - MOD11       ######################
    targets::tar_target(
      list_feat_calc_mod11_pred,
      command = rlang::inject(
        do.call(amadeus::calculate_modis, 
        c(list(locs = pred_grid, 
        locs_id = "AMA_SITE_CODE"), 
        list_args_calc_mod11)
        )
        ),
      pattern = map(list_args_calc_mod11),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      ),
      description = "Calculate MODIS - MOD11 features | predict"
    ),
    ###########################       MODIS - MOD06       ######################
    targets::tar_target(
      list_feat_calc_mod06_pred,
    command = rlang::inject(
        do.call(amadeus::calculate_modis, 
        c(list(locs = pred_grid, 
        locs_id = "AMA_SITE_CODE"), 
        list_args_calc_mod06)
        )
        ),
      pattern = map(list_args_calc_mod06),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      ),
      description = "Calculate MODIS - MOD06 features | predict"
    )
    ,
    ###########################       MODIS - MOD13       ######################
    targets::tar_target(
      list_feat_calc_mod13_pred,
      command = rlang::inject(
        do.call(amadeus::calculate_modis, 
        c(list(locs = pred_grid, 
        locs_id = "AMA_SITE_CODE"), 
        list_args_calc_mod13)
        )
        ),
      pattern = map(list_args_calc_mod13),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100")
      ),
      description = "Calculate MODIS - MOD13 features | predict"
    )
    ,
    ###########################     MODIS - MCD19_1km     ######################
    targets::tar_target(
      list_feat_calc_mcd19_1km_pred,
     command = rlang::inject(
        do.call(amadeus::calculate_modis, 
        c(list(locs = pred_grid, 
        locs_id = "AMA_SITE_CODE"), 
        list_args_calc_mcd19_1km)
        )
        ),
      pattern = map(list_args_calc_mcd19_1km),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_250")
      ),
      description = "Calculate MODIS - MCD19_1km features | predict"
    )
    ,
    ###########################     MODIS - MCD19_5km     ######################
    targets::tar_target(
      list_feat_calc_mcd19_5km_pred,
     command = rlang::inject(
        do.call(amadeus::calculate_modis, 
        c(list(locs = pred_grid, 
        locs_id = "AMA_SITE_CODE"), 
        list_args_calc_mcd19_5km)
        )
        ),
      pattern = map(list_args_calc_mcd19_5km),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_250")
      ),
      description = "Calculate MODIS - MCD19_5km features | predict"
    )
    ,
    ###########################       MODIS - MOD09       ######################
    targets::tar_target(
      list_feat_calc_mod09_pred,
      command = rlang::inject(
        do.call(amadeus::calculate_modis, 
        c(list(locs = pred_grid, 
        locs_id = "AMA_SITE_CODE"), 
        list_args_calc_mod09)
        )
        ),
      pattern = map(list_args_calc_mod09),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_25")
      ),
      description = "Calculate MODIS - MOD09 features | predict"
    )
    ,
    ###########################       MODIS - VIIRS       ######################
    targets::tar_target(
      list_feat_calc_viirs_pred,
      command = rlang::inject(
        do.call(amadeus::calculate_modis, 
        c(list(locs = pred_grid, 
        locs_id = "AMA_SITE_CODE"), 
        list_args_calc_viirs)
        )
        ),
      pattern = map(list_args_calc_viirs),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100")
      ),
      description = "Calculate MODIS - VIIRS features | predict"
    )
    ,
    ###########################        MODIS/VIIRS        ######################
    targets::tar_target(
      dt_feat_calc_nasa_pred,
      command = beethoven::reduce_merge(
        lapply(
          list(
            list_feat_calc_mod11_pred,
            list_feat_calc_mod06_pred,
            list_feat_calc_mod13_pred,
            list_feat_calc_mcd19_1km_pred,
            list_feat_calc_mcd19_5km_pred,
            list_feat_calc_mod09_pred,
            list_feat_calc_viirs_pred
          ),
          function(x) data.table::data.table(beethoven::reduce_list(x)[[1]])
        ),
        by = NULL
      ),
      description = "data.table of MODIS/VIIRS features | predict"
    ),
  ########################       DATE FEATURES       #########################
    targets::tar_target(
      dt_feat_calc_date_pred,
      command = Reduce(
        post_calc_autojoin2,
        list(
         # gmet_cleanup_pred,
          dt_feat_calc_geos_pred,
          dt_feat_calc_narr_pred,
          dt_feat_calc_nasa_pred
        )
      ),
      description = "data.table of all features | predict"
    )
    ,
  ########################       BASE FEATURES       #########################
    targets::tar_target(
      list_feat_calc_base_flat_pred,
      command = lapply(
        list(
          list(dt_feat_calc_hms_pred),
          list(dt_feat_calc_tri_pred),
          list(dt_feat_calc_nei_pred),
          list(dt_feat_calc_ecoregions_pred),
          list(dt_feat_calc_koppen_pred),
          list(dt_feat_calc_pop_pred),
          list(dt_feat_calc_groads_pred)
        ),
        function(x) {
          if (length(x) == 1) {
            x[[1]]
          } else if (
            sum(grepl("light|medium|heavy", sapply(x, \(t) names(t)))) == 3
          ) {
            xr <- lapply(x, \(dt) {
              dta <- data.table::copy(dt)
              dta <- dta[, time := as.character(time)]
              return(dta)
            })
            xrr <- Reduce(
              function(x, y) {
                collapse::join(x, y, on = c("site_id", "time"), how = "full")
              },
              xr
            )
            return(xrr)
          } else {
            collapse::rowbind(x, use.names = TRUE, fill = TRUE)
          }
        }
      ),
      description = "Calculated base feature list (all dt) | predict"
    )
    ,
    targets::tar_target(
      dt_feat_calc_base_pred,
      command = Reduce(
        post_calc_autojoin2,
        c(
          list_feat_calc_base_flat_pred,
          list(dt_feat_calc_gmted_pred),
          list(data.table::data.table(dt_feat_calc_nlcd_pred))
        )
      ),
      description = "Base features with PM2.5 | predict"
   ),
#######################     CUMULATIVE FEATURES      #######################
    targets::tar_target(
      dt_feat_calc_design_pred,
      command = post_calc_autojoin2(
        dt_feat_calc_base_pred,
        dt_feat_calc_date_pred,
        year_start = as.integer(substr(chr_daterange[1], 1, 4)),
        year_end = as.integer(substr(chr_daterange[2], 1, 4))
      ),
      description = "data.table of all features with PM2.5 | predict"
    ),
    targets::tar_target(
      dt_feat_calc_imputed_pred,
      command = impute_all2(
        dt_feat_calc_design_pred,
        period = chr_daterange,
        nthreads_dt = 32,
        nthreads_collapse = 32,
        nthreads_imputation = 32
      ),
      description = "Imputed features + lags | predict"
    )
    ,
    targets::tar_target(
      name = dt_feat_calc_xyt_pred,
      command = data.table::data.table(
        beethoven::attach_xy(
          dt_feat_calc_imputed_pred,
          pred_grid,
          locs_id ="AMA_SITE_CODE"
        )
      ),
      description = "Imputed features + AQS sites (outcome and lat/lon) | predict"
    ),

#######################     FILTER TO STATE      #######################
    targets::tar_target(
      locs_nc_pred,
      command = select_states(locs=pred_grid,state_list=c("Texas")),
      description="Extract state locations | predict"
     )
     ,
    targets::tar_target(
        covariates_nc_pred,
        command=dt_feat_calc_xyt_pred %>% filter(AMA_SITE_CODE %in% locs_nc_pred$AMA_SITE_CODE),
        description="Filter NC covariates"
    )
  )
  #nolint end