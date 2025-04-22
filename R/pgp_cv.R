#' Cross-validated PrestoGP Model Fit
#'
#' This function performs cross-validation (random or spatial) using the PrestoGP model
#' on air pollution data, supporting multiple chemical species and optional log-transformation.
#'
#' @param data A data frame or RDS file path containing pollution data with required columns.
#' @param dates A character vector of dates ("YYYY-MM-DD") to include in the model.
#' @param vars Character vector of column names to exclude from covariates.
#' @param logscale Logical; whether to log-transform concentration and MDL values.
#' @param cv_splits Integer number of cross-validation folds.
#' @param cv_method Cross-validation method: `"random"` (default), `"spatialsnake"`, or `"spatialrandom"`.
#'
#' @return A list containing:
#' \describe{
#'   \item{dates_vec}{Vector of dates used in the fit.}
#'   \item{chemlist}{Vector of unique chemical names (AQS_PARAMETER_NAME) in the dataset.}
#'   \item{logscale}{Logical value indicating whether log-transformation was applied.}
#'   \item{otr}{List of training set row indices for each cross-validation fold.}
#'   \item{otst}{List of test set row indices for each cross-validation fold.}
#'   \item{ytest.list}{List of test set response vectors (one per chemical) for each fold.}
#'   \item{model.list}{List of fitted PrestoGP models, one for each fold.}
#'   \item{pred.list}{List of predicted response values for each fold (unlisted across chemicals).}
#'   \item{mse.list}{List of mean squared error (MSE) values for each fold.}
#' }
#'
#' @importFrom dplyr mutate filter row_number
#' @importFrom sf st_as_sf
#' @importFrom rsample vfold_cv analysis assessment
#' @importFrom spatialsample spatial_block_cv
#' @export
pgp_cv <- function(
  data,
  dates,
  vars,
  logscale,
  cv_splits,
  cv_method = "random"
) {
  set.seed(33) # For reproducibility

  # ---- Validate inputs ----
  stopifnot(is.data.frame(data) || is.character(data))
  stopifnot(length(dates) >= 1)
  stopifnot(is.logical(logscale))
  stopifnot(cv_method %in% c("random", "spatialsnake", "spatialrandom"))

  # ---- Load and prepare data ----
  if (is.character(data)) {
    df <- readRDS(data)
  } else {
    df <- as.data.frame(data)
  }

  conc_col <- "CONC_DAILY_STD"
  mdl_col <- "MDL_DAILY_STD_UG_M3"
  chem_col <- "AQS_PARAMETER_NAME"

  dates <- as.Date(dates)

  df <- df %>%
    dplyr::mutate(time = as.Date(time)) %>%
    dplyr::filter(time %in% dates) %>%
    dplyr::mutate(
      time = as.numeric(time),
      idx = dplyr::row_number()
    )

  # ---- Covariates & Location Columns ----
  noncov_names <- c(vars, "time", "lon", "lat", "year")
  cov_names <- setdiff(names(df), noncov_names)
  cov_ind <- which(names(df) %in% cov_names)
  loc_ind <- c(
    which(colnames(df) == "lon"),
    which(colnames(df) == "lat"),
    which(colnames(df) == "time")
  )

  # ---- Optional log transformation ----
  if (logscale) {
    df[[conc_col]] <- log(df[[conc_col]])
    df[[mdl_col]] <- log(df[[mdl_col]])
  }

  # ---- Remove Rows with Incomplete Covariates ----
  # (Covariate Rows should be full through previous imputation, this is just a failsafe)
  df <- df[complete.cases(df[, cov_ind, drop = FALSE]), ]

  # ---- Cross-validation splitting ----
  if (startsWith(cv_method, "spatial")) {
    df_sf <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = "EPSG:4326")
    method_type <- if (cv_method == "spatialsnake") "snake" else "random"
    cvsplit <- spatialsample::spatial_block_cv(
      df_sf,
      v = cv_splits,
      method = method_type
    )
  } else {
    cvsplit <- rsample::vfold_cv(df, v = cv_splits)
  }

  # ---- Initialize Lists ----
  otr <- list()
  otst <- list()
  model.list <- list()
  pred.list <- list()
  ytest.list <- list()
  # mse.list <- list()

  # ---- Loop through folds ----
  for (r in seq_len(cv_splits)) {
    message("CV fold ", r)

    dftr <- spatialsample::analysis(cvsplit$splits[[r]])
    dftt <- spatialsample::assessment(cvsplit$splits[[r]])

    otr[[r]] <- dftr$idx
    otst[[r]] <- dftt$idx

    dftrain <- df[otr[[r]], ]
    dftest <- df[otst[[r]], ]

    ym <- list()
    Xm <- list()
    locsm <- list()
    lodsm <- list()

    ytest <- list()
    Xtest <- list()
    locstest <- list()

    chemlist <- sort(unique(df[[chem_col]]))

    for (i in seq_along(chemlist)) {
      chem <- chemlist[i]
      dfchem <- dftrain %>% dplyr::filter(.data[[chem_col]] == chem)

      y3 <- dfchem[[conc_col]]
      X3 <- as.matrix(dfchem[, cov_ind])
      colnames(X3) <- paste0(colnames(X3), "_", chem)

      locs3 <- as.matrix(dfchem[, loc_ind])
      lods3 <- dfchem[[mdl_col]]

      ym[[i]] <- y3
      Xm[[i]] <- X3
      locsm[[i]] <- locs3
      lodsm[[i]] <- lods3

      dfchem_test <- dftest %>% dplyr::filter(.data[[chem_col]] == chem)

      yt <- dfchem_test[[conc_col]]
      Xt <- as.matrix(dfchem_test[, cov_ind])
      colnames(Xt) <- paste0(colnames(Xt), "_", chem)

      locst <- as.matrix(dfchem_test[, loc_ind])

      ytest[[i]] <- yt
      Xtest[[i]] <- Xt
      locstest[[i]] <- locst
    }

    message("Fitting model on training set...")
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

    message("Making prediction on testing set...")
    pred <- prestogp_predict(model = all.mvm2, X = Xtest, locs = locstest) #,return.values = "meanvar")

    # model.mse <- mse(unlist(ytest), unlist(pred)) # Will check this again, may not be correct
    # model.mse <- mapply(Metrics::mse, ytest, pred)
    # message("MSE: ", round(model.mse, 4))

    ytest.list[[r]] <- ytest
    model.list[[r]] <- all.mvm2
    pred.list[[r]] <- pred
    # mse.list[[r]] <- model.mse
  }

  # ---- Return results ----
  results <- list(
    dates_vec = dates,
    chemlist = chemlist,
    logscale = logscale,
    otr = otr,
    otst = otst,
    ytest.list = ytest.list,
    model.list = model.list,
    pred.list = pred.list #,
    # mse.list = mse.list
  )

  return(results)
}
