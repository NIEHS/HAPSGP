# Run model with full dataset
# Run PrestoGP model on sh
# nolint start
# PrestoGP on HAPS + GRIDMET
# devtools::install_github("NIEHS/PrestoGP")
#library(PrestoGP)
#library(dplyr)
#dates=c("2021-01-01","2021-01-31")
#data="output/AGU/haps_gridmet_208-2021.rds"

#setwd("/ddn/gs1/home/kassienma/HAPSGP/")

pgp_fit = function(data, dates, vars, logscale) {
  # Read in data for model fit
  df = as.data.frame(data)

  # Filter to desired dates
  dates <- as.Date(dates)
  df2 = df %>%
    mutate(time = as.Date(time)) %>%
    filter(time %in% dates) %>%
    mutate(time = as.numeric(time))

  # Get covariate column indeces
  noncov_names = c(vars, "time", "lon", "lat", "year")
  cov_ind <- which(!names(df2) %in% noncov_names)
  # Get location column indeces
  loc_ind = c(
    which(colnames(df2) == "lon"),
    which(colnames(df2) == "lat"),
    which(colnames(df2) == "time")
  )

  # Transform to logscale if needed
  if (logscale == TRUE) {
    df2$CONC_DAILY_STD <- log(df2$CONC_DAILY_STD)
    df2$MDL_DAILY_STD_UG_M3 <- log(df2$MDL_DAILY_STD_UG_M3)
  }

  # remove rows with NA's in covariates
  df2 <- df2[
    !Reduce(`|`, lapply(df2[, cov_ind, drop = FALSE], is.na)),
  ]

  #Initialize lists for PrestoGP formatting
  ym <- list() # List for response variable data
  Xm <- list() # List for covariate data
  locsm <- list() # List for location data
  lodsm <- list() # List for MDL data

  #Loop through chemicals to populate lists
  chemlist = sort(unique(df2$AQS_PARAMETER_NAME)) # Generate list of unique chemicals

  for (i in 1:length(chemlist)) {
    dfchem = as.data.frame(df2 %>% filter(AQS_PARAMETER_NAME == chemlist[i]))

    y3 <- dfchem$CONC_DAILY_STD
    X3 <- as.matrix(dfchem[, cov_ind])[, 1:length(cov_ind)]
    colnames(X3) = paste0(colnames(X3), chemlist[i])

    # columns 1+2 are location coordinates; column 3 is time
    locs3 <- as.matrix(dfchem[, loc_ind])[, 1:3]
    lods3 <- dfchem$MDL_DAILY_STD_UG_M3

    ym[[i]] <- y3
    Xm[[i]] <- X3
    locsm[[i]] <- locs3
    lodsm[[i]] <- lods3
  }

  # Multivariate Vecchia model
  all.mvm <- new("MultivariateVecchiaModel", n_neighbors = 10)
  all.mvm2 <- prestogp_fit(
    all.mvm,
    ym,
    Xm,
    locsm,
    lod = lodsm,
    scaling = c(1, 1, 2),
    impute.y = TRUE,
    verbose = TRUE,
    penalty = "relaxed"
  )

  print("Model fit done. Saving results...")

  # Add output to a results list
  results = list()
  results$dates_vec = dates
  results$model = all.mvm2
  results$chemlist = chemlist
  results$logscale = logscale # Save to remember whether logscale was used in fit

  return(results)
}
#nolint end
