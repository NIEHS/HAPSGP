########################  covariates   ##########################
#nolint start
target_covariates <-
  list(
###########################     GRIDMET      ###########################
     targets::tar_target(
      variables_gridmet,
      command = c("vs","th","sph","pr","tmmn","tmmx","srad"),
      description="gridmet variables"
     )
    ,
     targets::tar_target(
      buffer_radius,
      command = c(0),
      description="buffer radiuses"
     )
    ,
     targets::tar_target(
      gmet_process,
      command = gridmet_process(data=process,
      dates_gridmet=model_dates, input_dir="input/covariates/gridmet/",
      variables_gridmet= variables_gridmet, radiuses=chr_iter_radii, output="merged",
      haps_locs=haps_locs),
      #pattern=cross(variables_gridmet,model_dates),
      description="Process covariates"
     )
    ,
    targets::tar_target(
      gmet_cleanup,
      command=gridmet_cleanup(gmet_process,vars),
        description="gridmet columns cleanup"
     ),

    ###########################      ECOREGIONS      ###########################
    targets::tar_target(
      download_ecoregions,
      #command = qs2::qs_read("/ddn/gs1/group/set/Projects/beethoven/targets/objects/download_ecoregions"),
      command = qs2::qs_read("/set_targets/objects/download_ecoregions"),
      description = "Read ecoregions data target from Beethoven"
    )
    ,
    targets::tar_target(
      dt_feat_calc_ecoregions,
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
            locs = haps_locs,
            locs_id = "AMA_SITE_CODE"
          )
        )
      },
      description = "data.table of Ecoregions features | fit"
    )
    ,
    ###########################      TRI/SEDC      ###########################
    targets::tar_target(
      download_tri,
      #command= any(list.files("/ddn/gs1/group/set/Projects/beethoven/targets/objects", 
                #pattern = "^download_tri.*", full.names = TRUE)!= ""),
      command= any(list.files("/set_targets/objects/", 
                pattern = "^download_tri.*", full.names = TRUE)!= ""),
      #command = qs2::qs_read("/ddn/gs1/group/set/Projects/beethoven/targets/objects/download_tri"),
      #command = qs2::qs_read("/set_targets/objects/download_tri"),
      description = "Read TRI data target from Beethoven"
    ),
    targets::tar_target(
      chr_iter_radii_tri,
      command = c(1000, 10000),
      description = "Buffer radii for TRI"
    )
    ,
    targets::tar_target(
      df_feat_calc_tri_params,
      command = expand.grid(
        year = chr_years,
        radius = chr_iter_radii_tri
      ) %>%
        split(seq_len(nrow(.))),
      iteration = "list",
      description = "TRI features"
    )
    ,
    targets::tar_target(
      list_feat_calc_tri,
      command = {
        download_tri
        beethoven::inject_calculate(
          covariate = "tri",
          locs = haps_locs,
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
      list_feat_reduce_tri,
      command = {
        list_feat_calc_tri_unnest <- lapply(
          list_feat_calc_tri,
          function(x) x[[1]]
        )
        chr_tri_radii_index <- sapply(
          list_feat_calc_tri_unnest,
          function(x) {
            any(grepl(sprintf("_%05d", chr_iter_radii_tri), names(x)))
          }
        )
        beethoven::reduce_merge(
          list_feat_calc_tri_unnest[chr_tri_radii_index],
          by = NULL,
          all.x = TRUE,
          all.y = TRUE
        )
      },
      iteration = "list",
      pattern = map(chr_iter_radii_tri),
      description = "Reduce TRI features based on radii | fit"
    )
    ,
    targets::tar_target(
      dt_feat_calc_tri,
      command = {
        dt_feat_merge_tri <- beethoven::reduce_merge(
          list_feat_reduce_tri[1:2],
          by = c("AMA_SITE_CODE", "time", "lon", "lat", "tri_year"),
          all.x = TRUE,
          all.y = TRUE
        )
        dt_feat_merge_tri[is.na(dt_feat_merge_tri)] <- 0
        dt_feat_pca_tri <- beethoven::post_calc_pca(
          locs_id = "AMA_SITE_CODE",
          data = dt_feat_merge_tri,
          yvar = NULL,
          num_comp = 5,
          pattern = "FUGITIVE|STACK",
          groups = sprintf("%05d", chr_iter_radii_tri),
          prefix = "TRI",
          kernel = TRUE
        )
      },
      description = "data.table of TRI PCA-reduced features | fit"
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
      chr_iter_calc_nlcd,
      command = c(2019, 2021),
      description = "NLCD years | download"
    )
    ,
    targets::tar_target(
      download_nlcd,
      command= any(list.files("/set_targets/objects/", 
                pattern = "^download_nlcd*", full.names = TRUE)!= ""),
      description = "Download NLCD data | download"
    )
    ,
    targets::tar_target(
      df_feat_calc_nlcd_params,
      command = expand.grid(
        year = chr_iter_calc_nlcd,
        radius = chr_iter_radii
      ) %>%
        split(seq_len(nrow(.))),
      iteration = "list",
      description = "NLCD features"
    )
    ,
    targets::tar_target(
      list_feat_calc_nlcd,
      command = {
        download_nlcd
        beethoven::inject_nlcd(
          locs = haps_locs,
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
      description = "Calculate NLCD features | fit"
    )
    ,
    targets::tar_target(
      name = dt_feat_calc_nlcd,
      command = list_feat_calc_nlcd %>%
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
      description = "NLCD feature list (all dt) | fit"
    ),
      ###########################      POPULATION      ###########################
     targets::tar_target(
      download_population,
     # command= any(list.files("/ddn/gs1/group/set/Projects/beethoven/targets/objects/",
     #           pattern = "^download_population.*", full.names = TRUE)!= ""),
      command= any(list.files("/set_targets/objects/", 
                pattern = "^download_population.*", full.names = TRUE)!= ""),
      description = "check population data download"
    )
    ,
    targets::tar_target(
      list_feat_calc_pop,
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
          locs = haps_locs,
          locs_id = "AMA_SITE_CODE",
          geom = FALSE,
          radius = chr_iter_radii
        )
      },
      pattern = map(chr_iter_radii),
      iteration = "list",
      description = "Calculate population features | fit"
    )
    ,
    targets::tar_target(
      dt_feat_calc_pop,
      command = beethoven::reduce_merge(
        beethoven::reduce_list(list_feat_calc_pop),by=NULL
      ),
      description = "data.table of population features | fit"
    ),
    ###########################        GROADS        ###########################
     targets::tar_target(
      download_groads,
      # command= any(list.files("/ddn/gs1/group/set/Projects/beethoven/targets/objects/",
      #          pattern = "^download_groads.*", full.names = TRUE)!= ""),
      command= any(list.files("/set_targets/objects/", 
                pattern = "^download_groads.*", full.names = TRUE)!= ""),
      description = "check gRoads data download"
    ),
    targets::tar_target(
      list_feat_calc_groads,
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
          locs = haps_locs,
          locs_id = "AMA_SITE_CODE",
          radius = chr_iter_radii
        )
      },
      iteration = "list",
      pattern = map(chr_iter_radii),
      description = "Calculate gRoads features | fit"
    )
    ,
    targets::tar_target(
      dt_feat_calc_groads,
      command = beethoven::reduce_merge(
        beethoven::reduce_list(list_feat_calc_groads),
        by = c("AMA_SITE_CODE", "description")
      ),
      description = "data.table of gRoads features | fit"
    ) ,
    ###########################        KOPPEN        ###########################
    targets::tar_target(
      download_koppen,
      #command= any(list.files("/ddn/gs1/group/set/Projects/beethoven/targets/objects/",
      #          pattern = "^download_koppen.*", full.names = TRUE)!= ""),
      command= any(list.files("/set_targets/objects/", 
                pattern = "^download_koppen.*", full.names = TRUE)!= ""),
      description = "Check Koppen-Geiger data download"
    )
    ,
    targets::tar_target(
    dt_feat_calc_koppen,
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
          locs = haps_locs,
          # NOTE: locs are all AQS sites for computational efficiency
          locs_id = "AMA_SITE_CODE",
          geom = FALSE
        )
      )
    },
    description = "data.table of Koppen Geiger features | fit"
  ),
  ###########################         NEI          ###########################
    targets::tar_target(
      chr_iter_calc_nei,
      command = c(2017, 2020),
      description = "NEI features | download"
    )
    ,
    targets::tar_target(
      download_nei,
      #command= any(list.files("/ddn/gs1/group/set/Projects/beethoven/targets/objects/",
      #          pattern = "^download_nei.*", full.names = TRUE)!= ""),
      command= any(list.files("/set_targets/objects/", 
                pattern = "^download_nei.*", full.names = TRUE)!= ""),
      description = "Check NEI data download"
    )
    ,
    targets::tar_target(
    list_feat_calc_nei,
    command = {
      download_nei
      beethoven::inject_calculate(
        covariate = "nei",
        locs = haps_locs,
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
    description = "Calculate NEI features | fit"
  )
  ,
  targets::tar_target(
    dt_feat_calc_nei,
   # command = beethoven::reduce_merge(
   #     beethoven::reduce_list(list_feat_calc_nei),by=NULL
   #   ),
    command = beethoven::reduce_list(
      lapply(list_feat_calc_nei,function(x) data.table::data.table(beethoven::reduce_list(x)[[1]])))[[1]],
    description = "data.table of NEI features | fit"
  ),
  ###########################         GMTED        ###########################
  targets::tar_target(
    chr_iter_calc_gmted_vars,
    command = c(
      "Breakline Emphasis", "Systematic Subsample",
      "Median Statistic", "Minimum Statistic",
      "Mean Statistic", "Maximum Statistic",
      "Standard Deviation Statistic"
    ),
    description = "GMTED features | download"
  )
  ,
  targets::tar_target(
    download_gmted,
    #command= any(list.files("/ddn/gs1/group/set/Projects/beethoven/targets/objects/",
    #          pattern = "^download_gmted.*", full.names = TRUE)!= ""),
    command= any(list.files("/set_targets/objects/", 
              pattern = "^download_gmted.*", full.names = TRUE)!= ""),
    pattern = map(chr_iter_calc_gmted_vars),
    description = "Check GMTED data download"
  )
  ,
  targets::tar_target(
    chr_iter_calc_gmted_radii,
    command = c(0, 1e3, 1e4),
    description = "GMTED radii"
  )
  ,
  targets::tar_target(
    list_feat_calc_gmted,
    command = {
      download_gmted
      beethoven::calc_gmted_direct(
        variable = c(chr_iter_calc_gmted_vars, "7.5 arc-seconds"),
        path = file.path(chr_input_dir, "gmted", "data_files"),
        locs = haps_locs,
        locs_id = "AMA_SITE_CODE",
        radius = chr_iter_calc_gmted_radii
      )
    },
    iteration = "list",
    pattern = cross(
      chr_iter_calc_gmted_vars,
      chr_iter_calc_gmted_radii
    ),
    description = "Calculate GMTED features | fit"
  )
  ,
  targets::tar_target(
    dt_feat_calc_gmted,
    command = beethoven::reduce_merge(
      beethoven::reduce_list(list_feat_calc_gmted), by = "AMA_SITE_CODE"
    ),
    description = "data.table of GMTED features | fit"
  ),
  ###########################         HMS          ###########################
  targets::tar_target(
    download_hms,
     #command= any(list.files("/ddn/gs1/group/set/Projects/beethoven/targets/objects/",
    #          pattern = "^download_hms.*", full.names = TRUE)!= ""),
    command= any(list.files("/set_targets/objects/", 
              pattern = "^download_hms.*", full.names = TRUE)!= ""),
    description = "Check HMS data download"
  )
  ,
  targets::tar_target(
    download_hms_buffer,
    command = {
      download_hms
      TRUE
    },
    description = "Download HMS data | buffer | download"
  )
  ,
  targets::tar_target(
    list_feat_calc_hms,
    command = {
      download_hms_buffer
      dt_iter_calc_hms <- beethoven::inject_calculate(
        covariate = "hms",
        locs = haps_locs,
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
    description = "Calculate HMS features | fit"     
  )
  ,
  targets::tar_target(
    dt_feat_calc_hms,
    command = beethoven::reduce_list(list_feat_calc_hms)[[1]],
    description = "data.table of HMS features | fit"
  ),
   ###########################         GEOS         ###########################
   targets::tar_target(
      chr_iter_calc_geos,
      command = c("aqc_tavg_1hr_g1440x721_v1", "chm_tavg_1hr_g1440x721_v1"),
      description = "GEOS-CF features | download"
    )
    ,
    targets::tar_target(
    download_geos,
    #command= any(list.files("/ddn/gs1/group/set/Projects/beethoven/targets/objects/",
    #          pattern = "^download_geos.*", full.names = TRUE)!= ""),
    command= any(list.files("/set_targets/objects/", 
              pattern = "^download_geos.*", full.names = TRUE)!= ""),
      description = "Check GEOS-CF data download"
    )
    ,
    targets::tar_target(
      download_geos_buffer,
      command = {
        download_geos
        TRUE
      },
      description = "Download GEOS-CF data | buffer | download"
    )
    ,
    targets::tar_target(
      list_feat_calc_geos_aqc,
      command = {
        download_geos_buffer
        beethoven::calc_geos_strict(
          path = file.path(chr_input_dir, "geos", chr_iter_calc_geos[1]),
          date = beethoven::fl_dates(unlist(list_dates)),
          locs = haps_locs,
          locs_id = "AMA_SITE_CODE"
        )
      },
      pattern = map(list_dates),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100")
      ),
      iteration = "list",
      description = "Calculate GEOS-CF features | aqc | fit"
    )
    ,
    targets::tar_target(
      list_feat_calc_geos_chm,
      command = {
        download_geos_buffer
        beethoven::calc_geos_strict(
          path = file.path(chr_input_dir, "geos", chr_iter_calc_geos[2]),
          date = beethoven::fl_dates(unlist(list_dates)),
          locs = haps_locs,
          locs_id = "AMA_SITE_CODE"
        )
      },
      pattern = map(list_dates),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100")
      ),
      iteration = "list",
      description = "Calculate GEOS-CF features | chm | fit"
    )
    ,
    targets::tar_target(
      dt_feat_calc_geos,
      command = beethoven::reduce_merge(
        c(
          beethoven::reduce_list(list_feat_calc_geos_aqc),
          beethoven::reduce_list(list_feat_calc_geos_chm)
        ),
        by = c("AMA_SITE_CODE", "time", "CO", "NO2", "SO2")
      ),
      description = "data.table of GEOS-CF features | fit"
    ),
  ###########################         NARR         ###########################
    
    targets::tar_target(
      chr_iter_calc_narr,
      command = c(
        "air.sfc", "albedo", "apcp", "dswrf", "evap", "hcdc", "hpbl",
        "lcdc", "lhtfl", "mcdc", "pr_wtr", "prate", "pres.sfc",
        "shtfl", "snowc", "soilm", "tcdc", "ulwrf.sfc", "uwnd.10m",
        "vis", "vwnd.10m", "weasd", "omega", "shum"
      ),
      description = "NARR features"
    )
    ,
    targets::tar_target(
      chr_iter_calc_narr_lag,
      command = c(
        "air.sfc", "apcp", "pres.sfc", "shum", "uwnd.10m", "vwnd.10m"
      ),
      description = "NARR features | lag"
    )
    ,
    targets::tar_target(
      download_narr,
      #command= any(list.files("/ddn/gs1/group/set/Projects/beethoven/targets/objects/",
      #          pattern = "^download_narr.*", full.names = TRUE)!= ""),
      command= any(list.files("/set_targets/objects/", 
              pattern = "^download_narr.*", full.names = TRUE)!= ""),
      description = "Check NARR data download"
    )
    ,
    targets::tar_target(
      download_narr_lag,
      #command= any(list.files("/ddn/gs1/group/set/Projects/beethoven/targets/objects/",
      #          pattern = "^download_narr_lag.*", full.names = TRUE)!= ""),
      command= any(list.files("/set_targets/objects/", 
              pattern = "^download_narr_lag.*", full.names = TRUE)!= ""),
      description = "Download NARR data | lag | download"
    )
    ,
    targets::tar_target(
      download_narr_buffer,
      command = {
        download_narr
        download_narr_lag
        TRUE
      },
      description = "Download NARR data | buffer | download"
    )
    , 
    targets::tar_target(
      list_feat_calc_narr,
      command = {
        download_narr_buffer
        dt_iter_calc_narr <- amadeus::calculate_narr(
          from = amadeus::process_narr(
            path = file.path(chr_input_dir, "narr", chr_iter_calc_narr),
            variable = chr_iter_calc_narr,
            date = beethoven::fl_dates(unlist(list_dates))
          ),
          locs = haps_locs,
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
      description = "Calculate NARR features | nolag | fit"
    )
    ,
    targets::tar_target(
      dt_feat_calc_narr_nolag,
      command = beethoven::reduce_merge(
        beethoven::reduce_list(list_feat_calc_narr),
        by = c("AMA_SITE_CODE", "time")
      ),
      description = "data.table of NARR features | nolag | fit"
    )
    ,
    targets::tar_target(
      list_feat_calc_narr_lag,
      command = {
        download_narr_buffer
        dt_lag_calc_narr <- amadeus::calculate_narr(
          from = amadeus::process_narr(
            path = file.path(chr_input_dir, "narr", chr_iter_calc_narr_lag),
            variable = chr_iter_calc_narr_lag,
            date = as.character(chr_dates[1] - 1)
          ),
          locs = haps_locs,
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
          names(dt_feat_calc_narr_nolag) %in% names(dt_lag_calc_narr)
        )
        dt_iter_calc_narr_bind <- rbind(
          dt_lag_calc_narr,
          dt_feat_calc_narr_nolag[, int_narr_cols, with = FALSE]
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
      description = "Calculate NARR features | lag | fit"
    )
    ,
    targets::tar_target(
      dt_feat_calc_narr,
      command = beethoven::reduce_merge(
        beethoven::reduce_list(
          c(list(dt_feat_calc_narr_nolag), list_feat_calc_narr_lag)
        ),
        by = c("AMA_SITE_CODE", "time")
      ),
      description = "data.table of NARR features | fit"
    ),
   ###########################       MODIS - MOD11       ######################
    targets::tar_target(
      download_mod11,
      #command= any(list.files("/ddn/gs1/group/set/Projects/beethoven/targets/objects/",
      #          pattern = "^download_mod11.*", full.names = TRUE)!= ""),
      command= any(list.files("/set_targets/objects/", 
              pattern = "^download_mod11.*", full.names = TRUE)!= ""),
      description = "check MODIS - MOD11 data download"
    )
    ,
    targets::tar_target(
      chr_args_calc_mod11_files,
      command = {
        download_mod11
        list.files(
          file.path(chr_input_dir, "modis", "raw", "61", "MOD11A1"),
          full.names = TRUE,
          recursive = TRUE
        )
      },
      description = "MODIS - MOD11 files"
    )
    ,
    targets::tar_target(
      list_args_calc_mod11,
      command = list(
        from = grep(
          x = chr_args_calc_mod11_files,
          pattern = paste0(
            "MOD11A1.A", unlist(list_dates_julian), collapse = "|"
          ),
          value = TRUE
        ),
        name_covariates = c("MOD_SFCTD_0_", "MOD_SFCTN_0_"),
        subdataset = "^LST_",
        radius = chr_iter_radii
      ),
      pattern = map(list_dates_julian),
      iteration = "list",
      description = "MODIS - MOD11 arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_mod11,
      command = rlang::inject(
        do.call(amadeus::calculate_modis, 
        c(list(locs = haps_locs, 
        locs_id = "AMA_SITE_CODE"), 
        list_args_calc_mod11)
        )
        ),
      pattern = map(list_args_calc_mod11),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      ),
      description = "Calculate MODIS - MOD11 features | fit"
    ),
    ###########################       MODIS - MOD06       ######################
    targets::tar_target(
      download_mod06,
      #command= any(list.files("/ddn/gs1/group/set/Projects/beethoven/targets/objects/",
      #          pattern = "^download_mod06.*", full.names = TRUE)!= ""),
      command= any(list.files("/set_targets/objects/", 
              pattern = "^download_mod06.*", full.names = TRUE)!= ""),
      description = "Download MODIS - MOD06 data | download"
    )
    ,
    targets::tar_target(
      chr_args_calc_mod06_files,
      command = {
        download_mod06
        list.files(
          file.path(chr_input_dir, "modis", "raw", "61", "MOD06_L2"),
          full.names = TRUE,
          recursive = TRUE
        )
      },
      description = "MODIS - MOD06 files"
    )
    ,
    targets::tar_target(
      list_args_calc_mod06,
      command = list(
        from = grep(
          x = chr_args_calc_mod06_files,
          pattern = paste0(
            "MOD06_L2.A", unlist(list_dates_julian), collapse = "|"
          ),
          value = TRUE
        ),
        name_covariates = c("MOD_CLCVD_0_", "MOD_CLCVN_0_"),
        subdataset = c("Cloud_Fraction_Day", "Cloud_Fraction_Night"),
        preprocess = amadeus::process_modis_swath,
        radius = chr_iter_radii
      ),
      pattern = map(list_dates_julian),
      iteration = "list",
      description = "MODIS - MOD06 arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_mod06,
    command = rlang::inject(
        do.call(amadeus::calculate_modis, 
        c(list(locs = haps_locs, 
        locs_id = "AMA_SITE_CODE"), 
        list_args_calc_mod06)
        )
        ),
      pattern = map(list_args_calc_mod06),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      ),
      description = "Calculate MODIS - MOD06 features | fit"
    )
    ,
    ###########################       MODIS - MOD13       ######################
    targets::tar_target(
      download_mod13,
      #command= any(list.files("/ddn/gs1/group/set/Projects/beethoven/targets/objects/",
      #          pattern = "^download_mod13.*", full.names = TRUE)!= ""),
      command= any(list.files("/set_targets/objects/", 
              pattern = "^download_mod13.*", full.names = TRUE)!= ""),
      description = "Download MODIS - MOD13 data | download"
    )
    ,
    targets::tar_target(
      chr_args_calc_mod13_files,
      command = {
        download_mod13
        list.files(
          file.path(chr_input_dir, "modis", "raw", "61", "MOD13A2"),
          full.names = TRUE,
          recursive = TRUE
        )
      },
      description = "MODIS - MOD13 files"
    )
    ,
    targets::tar_target(
      list_args_calc_mod13,
      command = list(
        from = grep(
          x = chr_args_calc_mod13_files,
          pattern = paste0(
            "MOD13A2.A", unlist(list_dates_julian), collapse = "|"
          ),
          value = TRUE
        ),
        name_covariates = "MOD_NDVIV_0_",
        subdataset = "(NDVI)",
        radius = chr_iter_radii
      ),
      pattern = map(list_dates_julian),
      iteration = "list",
      description = "MODIS - MOD13 arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_mod13,
      command = rlang::inject(
        do.call(amadeus::calculate_modis, 
        c(list(locs = haps_locs, 
        locs_id = "AMA_SITE_CODE"), 
        list_args_calc_mod13)
        )
        ),
      pattern = map(list_args_calc_mod13),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100")
      ),
      description = "Calculate MODIS - MOD13 features | fit"
    )
    ,
    ###########################     MODIS - MCD19_1km     ######################
    targets::tar_target(
      download_mcd19,
      #command= any(list.files("/ddn/gs1/group/set/Projects/beethoven/targets/objects/",
      #          pattern = "^download_mcd19.*", full.names = TRUE)!= ""),
      command= any(list.files("/set_targets/objects/", 
              pattern = "^download_mcd19.*", full.names = TRUE)!= ""),
      description = "Download MODIS - MCD19 data | download"
    )
    ,
    targets::tar_target(
      chr_args_calc_mcd19_files,
      command = {
        download_mcd19
        list.files(
          file.path(chr_input_dir, "modis", "raw", "61", "MCD19A2"),
          full.names = TRUE,
          recursive = TRUE
        )
      },
      description = "MODIS - MCD19_*km files"
    )
    ,
    targets::tar_target(
      list_args_calc_mcd19_1km,
      command = list(
        from = grep(
          x = chr_args_calc_mcd19_files,
          pattern = paste0(
            "MCD19A2.A", unlist(list_dates_julian), collapse = "|"
          ),
          value = TRUE
        ),
        name_covariates = c("MOD_AD4TA_0_", "MOD_AD5TA_0_"),
        subdataset = "^Optical_Depth",
        radius = chr_iter_radii
      ),
      pattern = map(list_dates_julian),
      iteration = "list",
      description = "MODIS - MCD19_1km arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_mcd19_1km,
     command = rlang::inject(
        do.call(amadeus::calculate_modis, 
        c(list(locs = haps_locs, 
        locs_id = "AMA_SITE_CODE"), 
        list_args_calc_mcd19_1km)
        )
        ),
      pattern = map(list_args_calc_mcd19_1km),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_250")
      ),
      description = "Calculate MODIS - MCD19_1km features | fit"
    )
    ,
    ###########################     MODIS - MCD19_5km     ######################
    targets::tar_target(
      list_args_calc_mcd19_5km,
      command = list(
        from = grep(
          x = chr_args_calc_mcd19_files,
          pattern = paste0(
            "MCD19A2.A", unlist(list_dates_julian), collapse = "|"
          ),
          value = TRUE
        ),
        name_covariates = c(
          "MOD_CSZAN_0_", "MOD_CVZAN_0_", "MOD_RAZAN_0_",
          "MOD_SCTAN_0_", "MOD_GLNAN_0_"
        ),
        subdataset = "cos|RelAZ|Angle",
        radius = chr_iter_radii
      ),
      pattern = map(list_dates_julian),
      iteration = "list",
      description = "MODIS - MCD19_5km arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_mcd19_5km,
     command = rlang::inject(
        do.call(amadeus::calculate_modis, 
        c(list(locs = haps_locs, 
        locs_id = "AMA_SITE_CODE"), 
        list_args_calc_mcd19_5km)
        )
        ),
      pattern = map(list_args_calc_mcd19_5km),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_250")
      ),
      description = "Calculate MODIS - MCD19_5km features | fit"
    )
    ,
    ###########################       MODIS - MOD09       ######################
    targets::tar_target(
      download_mod09,
      #command= any(list.files("/ddn/gs1/group/set/Projects/beethoven/targets/objects/",
      #          pattern = "^download_mod09.*", full.names = TRUE)!= ""),
      command= any(list.files("/set_targets/objects/", 
              pattern = "^download_mod09.*", full.names = TRUE)!= ""),
      description = "Download MODIS - MOD09 data | download"
    )
    ,
    targets::tar_target(
      chr_args_calc_mod09_files,
      command = {
        download_mod09
        list.files(
          file.path(chr_input_dir, "modis", "raw", "61", "MOD09GA"),
          full.names = TRUE,
          recursive = TRUE
        )
      },
      description = "MODIS - MOD09 files"
    )
    ,
    targets::tar_target(
      list_args_calc_mod09,
      command = list(
        from = grep(
          x = chr_args_calc_mod09_files,
          pattern = paste0(
            "MOD09GA.A", unlist(list_dates_julian), collapse = "|"
          ),
          value = TRUE
        ),
        name_covariates = c(
          "MOD_SFCRF_1_", "MOD_SFCRF_2_", "MOD_SFCRF_3_", "MOD_SFCRF_4_",
          "MOD_SFCRF_5_", "MOD_SFCRF_6_", "MOD_SFCRF_7_"
        ),
        subdataset = "^sur_refl_",
        radius = chr_iter_radii
      ),
      pattern = map(list_dates_julian),
      iteration = "list",
      description = "MODIS - MOD09 arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_mod09,
      command = rlang::inject(
        do.call(amadeus::calculate_modis, 
        c(list(locs = haps_locs, 
        locs_id = "AMA_SITE_CODE"), 
        list_args_calc_mod09)
        )
        ),
      pattern = map(list_args_calc_mod09),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_25")
      ),
      description = "Calculate MODIS - MOD09 features | fit"
    )
    ,
    ###########################       MODIS - VIIRS       ######################
    targets::tar_target(
      download_viirs,
      #command= any(list.files("/ddn/gs1/group/set/Projects/beethoven/targets/objects/",
      #          pattern = "^download_viirs.*", full.names = TRUE)!= ""),
      command= any(list.files("/set_targets/objects/", 
              pattern = "^download_viirs.*", full.names = TRUE)!= ""),
      description = "Download MODIS - VIIRS data | download"
    )
    ,
    targets::tar_target(
      chr_args_calc_viirs_files,
      command = {
        download_viirs
        list.files(
          file.path(chr_input_dir, "modis", "raw", "5000", "VNP46A2"),
          full.names = TRUE,
          recursive = TRUE
        )
      },
      description = "MODIS - VIIRS files"
    )
    ,
    targets::tar_target(
      list_args_calc_viirs,
      command = list(
        from = grep(
          x = chr_args_calc_viirs_files,
          pattern = paste0(
            "VNP46A2.A", unlist(list_dates_julian), collapse = "|"
          ),
          value = TRUE
        ),
        name_covariates = "MOD_LGHTN_0_",
        subdataset = 3,
        preprocess = amadeus::process_blackmarble,
        radius = chr_iter_radii
      ),
      pattern = map(list_dates_julian),
      iteration = "list",
      description = "MODIS - VIIRS arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_viirs,
      command = rlang::inject(
        do.call(amadeus::calculate_modis, 
        c(list(locs = haps_locs, 
        locs_id = "AMA_SITE_CODE"), 
        list_args_calc_viirs)
        )
        ),
      pattern = map(list_args_calc_viirs),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100")
      ),
      description = "Calculate MODIS - VIIRS features | fit"
    )
    ,
    ###########################        MODIS/VIIRS        ######################
    targets::tar_target(
      dt_feat_calc_nasa,
      command = beethoven::reduce_merge(
        lapply(
          list(
            list_feat_calc_mod11,
            list_feat_calc_mod06,
            list_feat_calc_mod13,
            list_feat_calc_mcd19_1km,
            list_feat_calc_mcd19_5km,
            list_feat_calc_mod09,
            list_feat_calc_viirs
          ),
          function(x) data.table::data.table(beethoven::reduce_list(x)[[1]])
        ),
        by = NULL
      ),
      description = "data.table of MODIS/VIIRS features | fit"
    ),
  ########################       DATE FEATURES       #########################
    targets::tar_target(
      dt_feat_calc_date,
      command = Reduce(
        post_calc_autojoin2,
        list(
          #gmet_cleanup,
          dt_feat_calc_geos,
          dt_feat_calc_narr,
          dt_feat_calc_nasa
        )
      ),
      description = "data.table of all features | fit"
    )
    ,
  ########################       BASE FEATURES       #########################
    targets::tar_target(
      list_feat_calc_base_flat,
      command = lapply(
        list(
          list(dt_feat_calc_hms),
          list(dt_feat_calc_tri),
          list(dt_feat_calc_nei),
          list(dt_feat_calc_ecoregions),
          list(dt_feat_calc_koppen),
          list(dt_feat_calc_pop),
          list(dt_feat_calc_groads)
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
      description = "Calculated base feature list (all dt) | fit"
    )
    ,
    targets::tar_target(
      dt_feat_calc_base,
      command = Reduce(
        post_calc_autojoin2,
        c(
          list(process),
          list_feat_calc_base_flat,
          list(dt_feat_calc_gmted),
          list(data.table::data.table(dt_feat_calc_nlcd))
        )
      ),
      description = "Base features with PM2.5 | fit"
   ),
#######################     CUMULATIVE FEATURES      #######################
    targets::tar_target(
      dt_feat_calc_design,
      command = post_calc_autojoin2(
        dt_feat_calc_base,
        dt_feat_calc_date,
        year_start = as.integer(substr(chr_daterange[1], 1, 4)),
        year_end = as.integer(substr(chr_daterange[2], 1, 4))
      ),
      description = "data.table of all features with PM2.5 | fit"
    ),
  
    targets::tar_target(
      dt_feat_calc_imputed,
      command = impute_all2(
        dt_feat_calc_design,
        period = chr_daterange,
        nthreads_dt = 32,
        nthreads_collapse = 32,
        nthreads_imputation = 32
      ),
      description = "Imputed features + lags | fit"
    )
    ,
    targets::tar_target(
      name = dt_feat_calc_xyt,
      command = data.table::data.table(
        beethoven::attach_xy(
          dt_feat_calc_imputed,
          haps_locs,
          locs_id ="AMA_SITE_CODE"
        )
      ),
      description = "Imputed features + AQS sites (outcome and lat/lon) | fit"
    ),

#######################     FILTER TO STATE      #######################
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
        command=dt_feat_calc_xyt %>% filter(AMA_SITE_CODE %in% haps_locs_nc$AMA_SITE_CODE),
        description="Filter NC covariates"
    )
  )
  

  )
  #nolint end