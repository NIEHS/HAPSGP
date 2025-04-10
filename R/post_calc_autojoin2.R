# Function from Beethoven with slight edits to fit HAPSGP pipeline
post_calc_autojoin2 <-
  function(
    df_fine,
    df_coarse,
    field_sp = "AMA_SITE_CODE",
    field_t = "time",
    year_start = 2018L,
    year_end = 2022L
  ) {
    if (any(grepl("population", names(df_coarse)))) {
      df_coarse <- df_coarse[, -c("time"), with = FALSE]
    }
    common_field <- intersect(names(df_fine), names(df_coarse))
    df_fine <- data.table::as.data.table(df_fine)
    df_coarse <- data.table::as.data.table(df_coarse)
    df_fine <- beethoven::post_calc_drop_cols(df_fine)
    df_coarse <- beethoven::post_calc_drop_cols(df_coarse)
    # if (length(common_field) > 2) {
    #   message("The data frames have more than two common fields.")
    #   message("Trying to remove the redundant common fields...")
    #   common_field <- intersect(names(df_fine), names(df_coarse))
    #   print(common_field)
    #   common_field <-
    #     common_field[-which(!common_field %in% c(field_sp, field_t))]
    # }
    if (length(common_field) == 1) {
      print(common_field)
      if (common_field == field_sp) {
        joined <- data.table::merge.data.table(
          df_fine, df_coarse,
          by = field_sp,
          all.x = TRUE
        )
      }
    }
    if (length(common_field) == 2) {
      if (all(common_field %in% c(field_sp, field_t))) {
        # t_fine <- try(as.Date(df_fine[[field_t]][1]))
        df_fine[[field_t]] <- as.character(df_fine[[field_t]])
        df_coarse[[field_t]] <- as.character(df_coarse[[field_t]])
        t_coarse <- try(as.Date(df_coarse[[field_t]][1]))
        if (inherits(t_coarse, "try-error")) {
          message(
            "The time field includes years. Trying different join strategy."
          )
          # derive the available years from the coarsely resolved data
          coarse_years <- sort(unique(unlist(as.integer(df_coarse[[field_t]]))))
          df_coarse2 <- beethoven::post_calc_df_year_expand(
            df_coarse,
            locs_id=field_sp,
            time_start = year_start,
            time_end = year_end,
            time_available = coarse_years
          )
          joined <-
            beethoven:::post_calc_join_yeardate(df_coarse2, df_fine, field_t, field_t,spid=field_sp)
        } else {
          joined <- data.table::merge.data.table(
            df_fine, df_coarse,
            by = c(field_sp, field_t),
            all.x = TRUE
          )
        }
      }
    }
    return(joined)
  }
