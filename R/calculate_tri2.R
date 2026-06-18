#' Calculate toxic release covariates
#' @description
#' Calculate toxic release values for polygons or isotropic buffer point
#' locations. Returns a \code{data.frame} object containing \code{locs_id}
#' and variables for each chemical in \code{from}. Optional wind frequency binning.
#' @param from SpatVector(1). Output of \code{process_tri()}.
#' @param locs sf/SpatVector. Locations where TRI variables are calculated.
#' @param locs_id character(1). Unique site identifier column name.
#'  Default is `"site_id"`.
#' @param radius Circular buffer radius.
#' Default is \code{c(1000, 10000, 50000)} (meters)
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param ... Placeholders.
#' @author Insang Song, Mariana Kassien
#' @return a data.frame or SpatVector object
#' @note U.S. context.
#' @seealso [`sum_edc`], [`process_tri`]
#' @importFrom terra vect
#' @importFrom terra crs
#' @importFrom terra nearby
#' @importFrom methods is
#' @importFrom data.table .SD
#' @importFrom utils read.csv
#' @importFrom data.table rbindlist
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr ends_with
#' @importFrom dplyr all_of
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarize
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_tri(
#'   from = tri, # derived from process_tri() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = c(1e3L, 1e4L, 5e4L)
#' )
#' }
#' @export
calculate_tri2 <- function(
  from = NULL,
  locs,
  locs_id = "site_id",
  radius = c(1e3L, 1e4L, 5e4L),
  wf = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_geom(geom)
  if (!methods::is(locs, "SpatVector")) {
    if (methods::is(locs, "sf")) {
      locs <- terra::vect(locs)
    }
  }
  if (!is.numeric(radius)) {
    stop("radius should be numeric.\n")
  }
  locs_re <- terra::project(locs, terra::crs(from))

  # split by year: locs and tri locations
  tri_cols <- grep("_AIR", names(from), value = TRUE)
  # error fix: no whitespace
  tri_cols <- sub(" ", "_", tri_cols)

  # inner lapply
  list_radius <- split(radius, radius)
  list_locs_tri <-
    Map(
      function(x) {
        locs_tri_s <-
          sum_edc2(
            locs = locs_re,
            from = from,
            locs_id = locs_id,
            sedc_bandwidth = x,
            target_fields = tri_cols,
            wf = wf,
            geom = FALSE
          )
        return(locs_tri_s)
      },
      list_radius
    )

  # bind element data.frames into one
  df_tri <- Reduce(function(x, y) dplyr::full_join(x, y), list_locs_tri)
  if (nrow(df_tri) != nrow(locs)) {
    df_tri <- dplyr::left_join(as.data.frame(locs), df_tri)
  }

  df_tri_return <- amadeus::calc_return_locs(
    covar = df_tri,
    POSIXt = FALSE,
    geom = geom,
    crs = terra::crs(from)
  )

  # read attr
  df_tri_return$time <- as.integer(attr(from, "tri_year"))

  return(df_tri_return)
}
