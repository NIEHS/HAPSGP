#' Fit PrestoGP Model to Chemical Contaminant Data
#'
#' This function prepares and fits a PrestoGP model to a dataset of chemical pollution
#' measurements with optional log transformation. It supports multiple chemical species.
#'
#' @param data A data frame or path to an RDS file containing the input dataset.
#'   Must include columns: `CONC_DAILY_STD`, `MDL_DAILY_STD_UG_M3`, `AQS_PARAMETER_NAME`,
#'   `time`, `lon`, `lat`, `year`, and covariates listed in `vars`.
#' @param dates A character vector of dates in `"YYYY-MM-DD"` format
#'   to subset the data for model fitting.
#' @param vars A character vector of column names to be excluded from covariate matrix
#'   (e.g., known variables like `"CONC_DAILY_STD"`).
#' @param logscale Logical; if `TRUE`, the `CONC_DAILY_STD` and `MDL_DAILY_STD_UG_M3`
#'   columns will be log-transformed before model fitting.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{dates_vec}{The input vector of dates used for filtering.}
#'   \item{model}{The fitted PrestoGP multivariate Vecchia model object.}
#'   \item{chemlist}{A character vector of chemical names included in the fit.}
#'   \item{logscale}{Logical value indicating if log transformation was applied.}
#' }
#'
#' @importFrom dplyr mutate filter
#' @importFrom PrestoGP prestogp_fit MultivariateVecchiaModel
#' @export
pgp_fit <- function(data, dates, vars, logscale) {
  # ---- Validate Inputs ----
  stopifnot(is.data.frame(data) || is.character(data))
  stopifnot(is.logical(logscale))

  # ---- Load Data ----
  if (is.character(data)) {
    df <- readRDS(data)
  } else {
    df <- as.data.frame(data)
  }

  # ---- Column References ----
  conc_col <- "CONC_DAILY_STD"
  mdl_col <- "MDL_DAILY_STD_UG_M3"
  chem_col <- "AQS_PARAMETER_NAME"

  # ---- Filter by Date ----
  dates <- as.Date(dates)
  df_filtered <- df %>%
    dplyr::mutate(time = as.Date(time)) %>%
    dplyr::filter(time %in% dates) %>%
    dplyr::mutate(time = as.numeric(time))

  # ---- Identify Covariates ----
  noncov_names <- c(vars, "time", "lon", "lat", "year")
  cov_names <- setdiff(names(df_filtered), noncov_names)
  cov_ind <- which(names(df_filtered) %in% cov_names)

  # ---- Identify Location/Time Columns ----
  loc_ind <- c(
    which(colnames(df_filtered) == "lon"),
    which(colnames(df_filtered) == "lat"),
    which(colnames(df_filtered) == "time")
  )

  # ---- Optional Log Transformation ----
  if (logscale) {
    df_filtered[[conc_col]] <- log(df_filtered[[conc_col]])
    df_filtered[[mdl_col]] <- log(df_filtered[[mdl_col]])
  }

  # ---- Remove Rows with Incomplete Covariates ----
  # (Covariate Rows should be full through previous imputation, this is just a failsafe)
  df_filtered <- df_filtered[
    complete.cases(df_filtered[, cov_ind, drop = FALSE]),
  ]

  # ---- Initialize Lists ----
  ym <- list()
  Xm <- list()
  locsm <- list()
  lodsm <- list()

  # ---- Process Each Chemical ----
  chemlist <- sort(unique(df_filtered[[chem_col]]))

  for (i in seq_along(chemlist)) {
    chem <- chemlist[i]
    message("Processing chemical: ", chem)

    dfchem <- df_filtered %>%
      dplyr::filter(.data[[chem_col]] == chem)

    y3 <- dfchem[[conc_col]]
    X3 <- as.matrix(dfchem[, cov_ind])
    colnames(X3) <- paste0(colnames(X3), "_", chem)

    locs3 <- as.matrix(dfchem[, loc_ind])
    lods3 <- dfchem[[mdl_col]]

    ym[[i]] <- y3
    Xm[[i]] <- X3
    locsm[[i]] <- locs3
    lodsm[[i]] <- lods3
  }

  # ---- Fit Multivariate Vecchia Model ----
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

  message("Model fit done. Saving results...")

  # ---- Output Results ----
  results <- list(
    dates_vec = dates,
    model = all.mvm2,
    chemlist = chemlist,
    logscale = logscale
  )

  return(results)
}
