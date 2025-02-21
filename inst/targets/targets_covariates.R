########################  covariates?   ##########################
#nolint start
target_covariates <-
  list(
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
      variables_gridmet= variables_gridmet, radiuses=buffer_radius, output="merged",
      haps_locs=haps_locs),
      #pattern=cross(variables_gridmet,model_dates),
      description="Process covariates"
     )
    ,
    ###########################      ECOREGIONS      ###########################

    ###########################      TRI/SEDC      ###########################
    targets::tar_target(
      download_tri,
      #command= any(list.files("/ddn/gs1/group/set/Projects/beethoven_targets/_targets/objects/", 
                #pattern = "^download_tri.*", full.names = TRUE)!= ""),
      command= any(list.files("/set_targets/objects/", 
                pattern = "^download_tri.*", full.names = TRUE)!= ""),
      #command = qs2::qs_read("/ddn/gs1/group/set/Projects/beethoven_targets/_targets/objects/download_tri"),
      #command = qs2::qs_read("/set_targets/objects/download_tri"),
      description = "Read TRI data target from Beethoven"
    ),
    targets::tar_target(
      df_feat_calc_tri_params,
      command = expand.grid(
        year = chr_years,
        radius = chr_iter_radii
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
      description = "Calculate TRI features | fit"
    )
    ,
    targets::tar_target(
      dt_feat_calc_tri,
      command= {
      for(i in 1:(length(list_feat_calc_tri)-1)){
      tribr2=list_feat_calc_tri[[i+1]][[1]]
      print(i)
      if(i==1){
      tribr=list_feat_calc_tri[[1]][[1]]   
      merged=beethoven::reduce_merge(list(tribr,tribr2), by=NULL, all.y=T)
      }else{
      merged=beethoven::reduce_merge(list(merged,tribr2), by=NULL, all.y=T)
     }
     }
     return(merged)
      }
      ,
      #command = beethoven::reduce_merge(
      #  lapply(
      #    list_feat_calc_tri,
      #    function(x) data.table::data.table(beethoven::reduce_list(x)[[1]])
      #  ),
      #  by = NULL,
      #  all.y=TRUE
      #),
      description = "data.table of TRI features | fit"
    ),
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
    )

  )
  #nolint end