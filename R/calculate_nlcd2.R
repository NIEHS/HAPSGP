#' Calculate land cover covariates
#' @description
#' Compute ratio of land cover class in circle buffers around points. Returns
#' a \code{data.frame} object containing \code{locs_id}, longitude, latitude,
#' time (year), and computed ratio for each land cover class.
#' @param from SpatRaster(1). Output of \code{process_nlcd()}.
#' @param locs terra::SpatVector of points geometry
#' @param locs_id character(1). Unique identifier of locations
#' @param mode character(1). One of `"exact"`
#'   (using [`exactextractr::exact_extract()`])
#'   or `"terra"` (using [`terra::freq()`]). Ignored if `locs` are points.
#' @param radius numeric (non-negative) giving the
#' radius of buffer around points.
#' @param max_cells integer(1). Maximum number of cells to be read at once.
#' Higher values may expedite processing, but will increase memory usage.
#' Maximum possible value is `2^31 - 1`. Only valid when
#' `mode = "exact"`.
#' See [`exactextractr::exact_extract`] for details.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param ... Placeholders.
#' @note NLCD is available in U.S. only. Users should be aware of
#' the spatial extent of the data. The results are different depending
#' on `mode` argument. The `"terra"` mode is less memory intensive
#' but less accurate because it counts the number of cells
#' intersecting with the buffer. The `"exact"` may be more accurate
#' but uses more memory as it will account for the partial overlap
#' with the buffer.
#' @seealso [`process_nlcd`]
#' @return a data.frame or SpatVector object
#' @importFrom utils read.csv
#' @importFrom methods is
#' @importFrom terra rast project vect crs set.crs buffer
#' @importFrom sf st_union st_geometry
#' @importFrom terra intersect metags
#' @importFrom exactextractr exact_extract
#' @importFrom collapse rowbind
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_nlcd(
#'   from = nlcd, # derived from process_nlcd() example
#'   locs = loc,
#'   locs_id = "id",
#'   mode = "exact",
#'   geom = FALSE
#' )
#' }
#' @export
calculate_nlcd2 <- function(
  from,
  locs,
  locs_id = "site_id",
  mode = c("exact", "terra"),
  radius = 1000,
  max_cells = 5e7,
  geom = FALSE,
  ...
) {
  # check inputs
  mode <- match.arg(mode)
  if (!is.numeric(radius)) {
    stop("radius is not a numeric.")
  }
  if (radius < 0) {
    stop("radius has not a likely value.")
  }
  if (!methods::is(from, "SpatRaster")) {
    stop("from is not a SpatRaster.")
  }

  # currently only handles 1 year
  if (terra::nlyr(from) > 1) {
    stop(
      paste0(
        "`from` contains more than one data layer. Current version ",
        "only processes one year worth of NLCD data."
      )
    )
  }

  # prepare locations
  locs_prepared <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  locs_vector <- locs_prepared[[1]]
  locs_df <- locs_prepared[[2]]

  # detect new or deprecated file path stucture
  if (names(from) == "NLCD Land Cover Class") {
    message(
      paste0(
        "Deprecated data format detected. Data still analyzed, but ",
        "see https://www.mrlc.gov/data/project/annual-nlcd for updated ",
        "NLCD documentation and availability."
      )
    )
  }
  year <- as.integer(terra::metags(from)) #[2, 2])
  stopifnot(year %in% 1985:2023L)

  # select points within mainland US and reproject on nlcd crs if necessary
  data_vect_b <-
    terra::project(locs_vector, y = terra::crs(from))
  cfpath <- system.file("extdata", "nlcd_classes.csv", package = "amadeus")
  nlcd_classes <- utils::read.csv(cfpath)

  if (radius <= 0 && terra::geomtype(locs) == "points") {
    message(
      paste0(
        "Calculating NLCD Land Cover Class covariates for ",
        year,
        "..."
      )
    )
    new_data_vect <- suppressMessages(
      amadeus::calc_worker(
        dataset = "nlcd",
        from = from,
        locs_vector = data_vect_b,
        locs_df = locs_df,
        fun = "mean",
        variable = 1,
        time = 4,
        time_type = "year",
        radius = 0,
        level = NULL
      )
    )
    new_data_vect$time <- year
    names(new_data_vect)[grep("Annual", names(new_data_vect))] <- sprintf(
      "LDU_0_%05d",
      radius
    )
  } else {
    # create circle buffers with buf_radius
    bufs_pol <- terra::buffer(data_vect_b, width = radius)
    if (mode == "terra") {
      # terra mode
      class_query <- "names"
      # extract land cover class in each buffer
      nlcd_at_bufs <- Map(
        function(i) {
          terra::freq(
            from,
            zones = bufs_pol[i, ],
            wide = TRUE
          )
        },
        seq_len(nrow(bufs_pol))
      )
      nlcd_at_bufs_fill <- amadeus::collapse_nlcd(
        data = nlcd_at_bufs,
        mode = mode,
        locs_id = locs_id
      )
      nlcd_at_bufs_fill <- nlcd_at_bufs_fill[, -seq(1, 2)]
      nlcd_cellcnt <- nlcd_at_bufs_fill[, seq(1, ncol(nlcd_at_bufs_fill), 1)]
      nlcd_cellcnt <- nlcd_cellcnt / rowSums(nlcd_cellcnt, na.rm = TRUE)
      nlcd_at_bufs_fill[, seq(1, ncol(nlcd_at_bufs_fill), 1)] <- nlcd_cellcnt
    } else {
      class_query <- "value"
      # ratio of each nlcd class per buffer
      bufs_polx <- bufs_pol[terra::ext(from), ] |>
        sf::st_as_sf()

      nlcd_at_bufs <- Map(
        function(i) {
          exactextractr::exact_extract(
            from,
            bufs_polx[i, ],
            fun = "frac",
            force_df = TRUE,
            progress = FALSE,
            append_cols = locs_id,
            max_cells_in_memory = max_cells
          )
        },
        seq_len(nrow(bufs_polx))
      )
      nlcd_at_bufs_fill <- amadeus::collapse_nlcd(
        data = nlcd_at_bufs,
        mode = mode,
        locs = bufs_pol,
        locs_id = locs_id
      )
      # select only the columns of interest
      nlcd_at_buf_names <- names(nlcd_at_bufs_fill)
      nlcd_val_cols <-
        grep("^frac_", nlcd_at_buf_names)
      nlcd_at_bufs_fill <- nlcd_at_bufs_fill[, nlcd_val_cols]
    }
    # fill NAs
    nlcd_at_bufs_fill[is.na(nlcd_at_bufs_fill)] <- 0
    # change column names
    nlcd_names <- names(nlcd_at_bufs_fill)
    nlcd_names <- sub(pattern = "frac_", replacement = "", x = nlcd_names)
    nlcd_names <-
      switch(
        mode,
        exact = as.numeric(nlcd_names),
        terra = nlcd_names
      )
    nlcd_names <-
      nlcd_classes$class[match(nlcd_names, nlcd_classes[[class_query]])]
    new_names <- sprintf("LDU_%s_0_%05d", nlcd_names, radius)
    names(nlcd_at_bufs_fill) <- new_names

    # merge locs_df with nlcd class fractions
    new_data_vect <- cbind(locs_df, as.integer(year), nlcd_at_bufs_fill)
  }

  if (geom %in% c("sf", "terra")) {
    names(new_data_vect)[1:3] <- c(locs_id, "geometry", "time")
  } else {
    names(new_data_vect)[1:2] <- c(locs_id, "time")
  }
  new_data_return <- amadeus::calc_return_locs(
    covar = new_data_vect,
    POSIXt = FALSE,
    geom = geom,
    crs = terra::crs(from)
  )
  return(new_data_return)
}
#' Inject arguments into NLCD calculation function for branching
#' @keywords Calculation
#' @param year An integer specifying the year to calculate NLCD data for.
#' @param radius An integer specifying the radius for the NLCD calculation.
#' @param ... Additional arguments to be passed to the NLCD calculation
#'  function.
#' @return data.frame object.
#' @export
inject_nlcd2 <-
  function(
    year = 2019,
    radius = 1000,
    ...
  ) {
    args_ext <- list(...)
    args_ext <- c(args_ext, list(year = year, radius = radius))
    inject_match(calculate_nlcd2, args_ext)
  }
