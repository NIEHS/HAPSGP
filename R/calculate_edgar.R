#' Calculate EDGAR covariates
#' @description
#' Extract EDGAR yearly sector VOC values at point locations. Returns a
#' \code{data.frame} object containing \code{locs_id} and EDGAR voc variable.
#' variable column names reflect the VOC number and circular buffer radius.
#' @param from SpatRaster(1). Output from \code{process_edgar()}.
#' @param locs data.frame. character to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param fun character(1). Function used to summarize multiple raster cells
#' within sites location buffer (Default = `mean`).
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param ... Placeholders.
#' @author Mariana Alifa Kassien
#' @seealso [`process_edgar()`]
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @examples
#' ## NOTE: Current function only supports one VOC number in 'from' SpatRaster.
#' ##       User should process and calculate each VOC number separately.
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_edgar(
#'   from = edgar, # derived from process_gridmet() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean",
#'   geom = FALSE
#' )
#' }
#' @export
calculate_edgar <- function(
  from,
  locs,
  locs_id = NULL,
  radius = 0,
  fun = "mean",
  geom = FALSE,
  ...
) {
  #### prepare locations list
  sites_list <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]
  #### perform extraction
  sites_extracted <- amadeus::calc_worker(
    dataset = "edgar",
    from = from,
    locs_vector = sites_e,
    locs_df = sites_id,
    radius = radius,
    fun = fun,
    variable = 1,
    level = 2,
    time = 3,
    time_type = "year",
    ...
  )

  #### pivot to wide dataframe by sector
  voc_col <- names(sites_extracted)[ncol(sites_extracted)]
  species <- sub("_.*", "", voc_col)
  radius <- sub(".*_", "", voc_col)
  # Create the pivot key
  sites_extracted$variable <- paste0(
    species,
    "_",
    sites_extracted$level,
    "_",
    radius
  )
  # Rename the VOC value column to 'value' for use in reshape
  names(sites_extracted)[ncol(sites_extracted) - 1] <- "value"
  # Drop 'level' column
  sites_extracted$level <- NULL
  # reshape to wide format
  idvar <- names(sites_extracted)[
    names(sites_extracted) %in% c(locs_id, "time", "geometry")
  ]
  sites_wide <- reshape(
    sites_extracted,
    idvar = idvar,
    timevar = "variable",
    direction = "wide"
  )
  # clean up names
  names(sites_wide) <- sub("^value\\.", "", names(sites_wide))

  sites_return <- amadeus::calc_return_locs(
    covar = sites_wide,
    POSIXt = FALSE,
    geom = geom,
    crs = terra::crs(from)
  )

  #### return data.frame
  return(sites_return)
}
