################################################################################
##############################      LIBPATHS       #############################
.libPaths(
  grep(
    paste0("biotools|", Sys.getenv("USER")),
    .libPaths(),
    value = TRUE,
    invert = TRUE
  )
)
cat("Active library paths:\n")
.libPaths()

############################      RUN PIPELINE      ############################
# targets::tar_make() # nolint
targets::tar_make(
  name = c(
    "mc_iter",
    "n_impute",
    "soil_mc",
    "soil_mc_censored",
    "soil_prestogp_model",
    "soil_prestogp_poe_model",
    "soil_relaxo_model",
    "soil_OK_model",
    "soil_prestogp_censored",
    "soil_prestogp_poe_censored",
    "soil_krig_ml_model",
    "summary_soil_results"
  ),
  reporter = "verbose"
) # nolint
