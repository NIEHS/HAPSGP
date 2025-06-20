# Presto GP cross-validation fit
#nolint start

pgp_cv = function(
  data,
  dates,
  vars,
  logscale,
  cv_splits,
  cv_method = "random"
) {
  set.seed(33) # Set random seed for reproducibility in CV splits

  dates <- as.Date(dates)

  df = df %>%
    mutate(time = as.Date(time)) %>%
    filter(time %in% dates) %>%
    mutate(time = as.numeric(time)) %>%
    mutate(idx = row_number())

  #Get covariate indeces
  noncov_names = c(vars, "time", "lon", "lat", "year")
  cov_ind <- which(!names(df) %in% noncov_names)
  loc_ind = c(
    which(colnames(df) == "lon"),
    which(colnames(df) == "lat"),
    which(colnames(df) == "time")
  )

  # remove rows with NA's in covariates
  df <- df[!Reduce(`|`, lapply(df[, cov_ind, drop = FALSE], is.na)), ]

  # Crossvalidation split
  # Convert to sf
  df_sf = st_as_sf(df, coords = c("lon", "lat"), crs = "EPSG:4326")
  # Separate into cross-validation sets
  rep = cv_splits
  if (cv_method == "spatial") {
    cvsplit <- spatialsample::spatial_block_cv(df_sf, v = rep, method = "snake")
  } else if (cv_method == "random") {
    cvsplit <- rsample::vfold_cv(df, v = rep)
  } else {
    stop("Please specify valid CV method (either 'spatial' or 'random')")
  }

  otr <- list()
  otst <- list()

  model.list <- list()
  pred.list <- list()
  ytest.list <- list()
  mse.list <- list()

  for (r in 1:rep) {
    print(paste("CV fold", r))

    #h=2
    cvsplit <- vfold_cv(df, v = rep)
    dftr <- spatialsample::analysis(cvsplit$splits[[r]])
    dftt <- spatialsample::assessment(cvsplit$splits[[r]])

    otr[[r]] <- dftr$idx
    otst[[r]] <- dftt$idx

    dftrain = df[otr[[r]], ]
    dftest = df[otst[[r]], ]

    #Initialize lists
    ym <- list()
    Xm <- list()
    locsm <- list()
    lodsm <- list()

    ytest <- list()
    Xtest <- list()
    locstest <- list()

    #Loop through chemicals to populate lists
    chemlist = sort(unique(df$AQS_PARAMETER_NAME))

    for (i in 1:length(chemlist)) {
      dfchem = as.data.frame(
        dftrain %>% filter(AQS_PARAMETER_NAME == chemlist[i])
      )

      y3 <- dfchem$CONC_DAILY_STD
      X3 <- as.matrix(dfchem[, cov_ind])[, 1:length(cov_ind)]
      colnames(X3) = paste0(colnames(X3), chemlist[i])

      # columns 1+2 are location coordinates; column 3 is time
      locs3 <- as.matrix(dfchem[, loc_ind])[, 1:3]
      lods3 = dfchem$MDL_DAILY_STD_UG_M3

      ym[[i]] <- y3
      Xm[[i]] <- X3
      locsm[[i]] <- locs3
      lodsm[[i]] <- lods3

      # set up test X and locs
      dfchem_test = dftest %>% filter(AQS_PARAMETER_NAME == chemlist[i])

      yt <- dfchem_test$CONC_DAILY_STD
      Xt <- as.matrix(dfchem_test[, cov_ind])
      colnames(Xt) = paste0(colnames(Xt), chemlist[i])

      locst <- as.matrix(dfchem_test[, loc_ind])

      ytest[[i]] <- yt
      Xtest[[i]] <- Xt
      locstest[[i]] <- locst
    }

    print("Fitting model on training set...")
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
      penalty = "SCAD"
    )

    print("Making prediction on testing set...")
    pred <- prestogp_predict(model = all.mvm2, X = Xtest, locs = locstest) #,return.values = "meanvar")

    model.mse = mse(exp(unlist(ytest)), exp(unlist(pred)))
    print(model.mse)

    # Add output to lists
    ytest.list[[r]] = ytest
    model.list[[r]] = all.mvm2
    pred.list[[r]] = unlist(pred)
    mse.list[[r]] = model.mse
  } #r

  #Compile results
  results = list()

  results$otr = otr
  results$otst = otst
  results$ytest.list = ytest.list
  results$model.list = model.list
  results$pred.list = pred.list
  results$mse.list = mse.list

  return(results)
  #saveRDS(results, paste0("output/AGU/pgpagu_spatcvfit_",dates[1],"_",dates[2],".RDS"))
}
# nolint end
