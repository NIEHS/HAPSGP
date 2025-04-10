# Edited from beethoven to match HAPSGP pipeline
impute_all2 <-
  function(
    dt,
    period,
    nthreads_dt = 32L,
    nthreads_collapse = 32L,
    nthreads_imputation = 32L
  ) {
    data.table::setDTthreads(nthreads_dt)
    if (is.character(dt)) {
      if (!endsWith(dt, ".qs")) {
        stop(
          paste0(
            "If `dt` points to a file, provide full path to .qs file.\n"
          )
        )
      }
      dt <- qs::qread(file.path(dt))
    }
    dt$time <- as.POSIXct(dt$time)
    # remove unnecessary columns
    query <- "^(AMA_SITE_CODE|time)\\.[0-9]+"
    #dt <- dt[, !(grepl(query, names(dt)) | names(dt) %in% vars[2:length(vars)]), with = FALSE]
    dt <- dt[, !(grepl(query, names(dt))), with = FALSE]
    # name cleaning
    dt <- stats::setNames(
      dt,
      sub("light_00000", "OTH_HMSWL_0_00000", names(dt))
    )
    dt <- stats::setNames(
      dt,
      sub("medium_00000", "OTH_HMSWM_0_00000", names(dt))
    )
    dt <- stats::setNames(
      dt,
      sub("heavy_00000", "OTH_HMSWH_0_00000", names(dt))
    )
    dt <- stats::setNames(
      dt,
      sub("population_", "POP_SEDAC_0_", names(dt))
    )

    geoscn <-
      "ACET\tGEO_ACETO_0_00000
    ALD2\tGEO_ACETA_0_00000
    ALK4\tGEO_CALKA_0_00000
    BCPI\tGEO_HIBCA_0_00000
    BCPO\tGEO_HOBCA_0_00000
    BENZ\tGEO_BENZE_0_00000
    C2H6\tGEO_ETHTE_0_00000
    C3H8\tGEO_PROPA_0_00000
    CH4\tGEO_METHA_0_00000
    CO\tGEO_CMONO_0_00000
    DST1\tGEO_DUST1_0_00000
    DST2\tGEO_DUST2_0_00000
    DST3\tGEO_DUST3_0_00000
    DST4\tGEO_DUST4_0_00000
    EOH\tGEO_ETHOL_0_00000
    H2O2\tGEO_HYPER_0_00000
    HCHO\tGEO_FORMA_0_00000
    HNO3\tGEO_NITAC_0_00000
    HNO4\tGEO_PERAC_0_00000
    ISOP\tGEO_ISOPR_0_00000
    MACR\tGEO_METHC_0_00000
    MEK\tGEO_MEKET_0_00000
    MVK\tGEO_MVKET_0_00000
    N2O5\tGEO_DIPEN_0_00000
    NH3\tGEO_AMNIA_0_00000
    NH4\tGEO_AMNUM_0_00000
    NIT\tGEO_INNIT_0_00000
    NO\tGEO_NIOXI_0_00000
    NO2\tGEO_NIDIO_0_00000
    NOy\tGEO_NITRO_0_00000
    OCPI\tGEO_HIORG_0_00000
    OCPO\tGEO_HOORG_0_00000
    PAN\tGEO_PERNI_0_00000
    PM25_RH35_GCC\tGEO_PM25X_0_00000
    PM25_RH35_GOCART\tGEO_PM25R_0_00000
    PM25bc_RH35_GCC\tGEO_BLCPM_0_00000
    PM25du_RH35_GCC\tGEO_DUSPM_0_00000
    PM25ni_RH35_GCC\tGEO_NITPM_0_00000
    PM25oc_RH35_GCC\tGEO_ORCPM_0_00000
    PM25soa_RH35_GCC\tGEO_SORPM_0_00000
    PM25ss_RH35_GCC\tGEO_SEAPM_0_00000
    PM25su_RH35_GCC\tGEO_SULPM_0_00000
    PRPE\tGEO_CALKE_0_00000
    RCHO\tGEO_CALDH_0_00000
    SALA\tGEO_FSEAS_0_00000
    SALC\tGEO_CSEAS_0_00000
    SO2\tGEO_SULDI_0_00000
    SOAP\tGEO_SOAPR_0_00000
    SOAS\tGEO_SOASI_0_00000
    TOLU\tGEO_TOLUE_0_00000
    XYLE\tGEO_XYLEN_0_00000
    CO_y\tGEO_COVMR_0_00000
    NO2_y\tGEO_NOVMR_0_00000
    O3\tGEO_OZVMR_0_00000
    SO2_y\tGEO_SOVMR_0_00000"

    geoscn <- strsplit(geoscn, "\n")
    geoscn <- unlist(geoscn)
    geoscn <- strsplit(geoscn, "\t")
    geoscn <- do.call(rbind, geoscn)
    geoscndf <- as.data.frame(geoscn, stringsAsFactors = FALSE)
    colnames(geoscndf) <- c("variable", "code")
    geoscndf$variable <- trimws(geoscndf$variable)

    for (i in seq_len(nrow(geoscndf))) {
      dt <-
        setNames(
          dt,
          stringi::stri_replace_all_regex(
            names(dt), sprintf("%s$", geoscndf$variable[i]), geoscndf$code[i]
          )
        )
    }
    AMA_SITE_CODE <- NULL
    # NDVI 16-day
    # For each site_id, backward filling for 16-day NDVI
    # Last Observation Carried Forward is the method used;
    # it assumes that the rows are ordered by date
    dt <- dt[order(AMA_SITE_CODE, time), ]
    col_ndviv <- grep("MOD_NDVIV_", names(dt))
    dtndviv <-
      data.table::setnafill(
        dt, type = "nocb", nan = NA,
        cols = col_ndviv
      )

    collapse::set_collapse(mask = "manip", nthreads = nthreads_collapse)

    target_replace <- grep("^MOD_", names(dt), invert = TRUE)
    dt <- collapse::replace_inf(dtndviv, value = NA, replace.nan = TRUE)
    dt <- collapse::replace_na(dt, value = 0, cols = target_replace)

    # zero-variance exclusion
    dt_colvars <- collapse::fvar(dt[, 13:ncol(dt), with = FALSE])
    zero_var_fields <- names(dt_colvars[dt_colvars == 0])

    # Exclude fields with zero variance using data.table
    dt <- dt[, (zero_var_fields) := NULL]

    # Store the name of zero variance fields as an attribute of the input object
    attr(dt, "zero_var_fields") <- zero_var_fields


    # excluding columns with excessive "true zeros"
    # we should have a threshold for the zero rate
    # exc_zero <- collapse::fnth(dt[, 5:ncol(dt), with = FALSE], n = 0.9)
    # exc_zero <- unname(which(exc_zero == 0)) + 5L
    # dt <- dt[, (exc_zero) := NULL]

    # Q: Do we use all other features to impute? -- Yes.
    # 32-thread, 10% for tree building, 200 trees, 4 rounds: 11 hours
    imputed <-
      missRanger::missRanger(
        data = dt,
        maxiter = 10L,
        num.trees = 300L,
        num.threads = nthreads_imputation,
        mtry = 100L,
        sample.fraction = 0.1
      )

    imputed <- amadeus::calculate_temporal_dummies(imputed, "time")
    return(imputed)
    # lagged features: changing period (period[1] + 1 day)
    # period <- as.Date(period)
    # period[1] <- period[1] + as.difftime(1, units = "days")
    # period <- as.character(period)
    # index_lag <-
    #   sprintf("MET_%s", c("ATSFC", "ACPRC", "PRSFC", "SPHUM", "WNDSP"))
    # index_lag <- grep(paste(index_lag, collapse = "|"), names(dt))
    # target_lag <- imputed[, index_lag, with = FALSE]

    # output <- amadeus::calculate_lagged(target_lag, period, 1, "AMA_SITE_CODE")
    # return(output)
  }
