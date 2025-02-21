#' Perform Principal Component Analysis
#' @keywords internal
#' @param data data.frame or data.table
#' @param threshold numeric(1). A fraction of the total variance that should
#' be covered by the components.
#' @seealso [`recipes::step_pca()`]
#' @importFrom recipes recipe bake prep step_normalize step_pca
#' @return data.table with Principal Components sufficient to satisfy the
#' `threshold`.`
#' @export
reduce_pca <- function(
  data,
  threshold = 0.99
) {

  stopifnot("data.frame" %in% class(data))
  stopifnot(is.numeric(threshold))

  data_rec <- recipes::recipe(~., data = data)
  data_pca <- data_rec %>%
    recipes::step_normalize(recipes::all_numeric()) %>%
    recipes::step_pca(recipes::all_numeric(), threshold = threshold)

  data_prep <- recipes::prep(data_pca, data = data)
  data_pca <- recipes::bake(data_prep, new_data = data)

  return(data_pca)
}

#' Post-calculation Principal Component Analysis
#' @description This function performs PCA on the input data frame to reduce
#' number of predictors.
#' @param data data.frame or data.table
#' @param locs_id The column name in the spatial object that represents the
#'   location identifier.
#' @param time_id The column name in the data frame that represents the time
#'   identifier.
#' @param yvar The target variable.
#' @param coords The column names that represent the XY coordinates. Default
#' is `c("lon", "lat")`.
#' @param threshold numeric(1). A fraction of the total variance that should
#' be covered by the components.
#' @param pattern character(1). A regular expression pattern to match the
#' columns that should be included in the PCA.
#' @param groups character. A character vector of groups to perform PCA on.
#' Each character should be a regular expression pattern to match the columns
#' that should be included in the PCA. Default is `NULL`.
#' @param prefix character(1). A prefix to be added to the column names of the
#' Principal Components. Default is `NULL`.
#' @seealso [`reduce_pca()`] [`recipes::step_pca()`]
#' @importFrom data.table data.table
#' @return data.table with Principal Components sufficient to satisfy the
#' `threshold`, merged with `*_id` and `yvar` columns from original `data`.
post_calc_pca <- function(
  data,
  locs_id = "site_id",
  time_id = "time",
  yvar = "Arithmetic.Mean",
  coords = c("lon", "lat"),
  threshold = 0.99,
  pattern = "FUGITIVE|STACK",
  groups = NULL,
  prefix = "PCA"
) {

  data <- data.table::data.table(data)
  #chr_retaincols <- c(locs_id, time_id, yvar, coords)
  #data_trim <- data[, chr_retaincols, with = FALSE]
   data_trim <- data[
    , !grep(pattern, names(data)), with = FALSE
  ]

  data_pca <- data[
    , grep(pattern, names(data)), with = FALSE
  ]

  if (is.null(groups)) {
    return_pca <- reduce_pca(
      data = data_pca,
      threshold = threshold
    )
    names(return_pca) <- paste0(prefix, "_", names(return_pca))
  } else {
    list_pca <- list()
    for (g in seq_along(groups)) {
      data_group <- data_pca[
        , grep(groups[g], names(data_pca)), with = FALSE
      ]
      group_pca <- reduce_pca(
        data = data_group,
        threshold = threshold
      )
      names(group_pca) <- paste0(
        prefix, "_", names(group_pca), "_", groups[g]
      )
      list_pca <- c(list_pca, group_pca)
    }
    return_pca <- do.call(cbind, list_pca)
  }

  stopifnot(nrow(data_trim) == nrow(return_pca))
  data_return <- data.table::data.table(cbind(data_trim, return_pca))

  return(data_return)

}