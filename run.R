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
    "soil_mc",
    "soil_prestogp_model",
    "soil_prestogp_poe_model",
    "soil_relaxo_model",
    "summary_soil_results"
  )
) # nolint
