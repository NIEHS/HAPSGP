# nolint start
#' Calculate isotropic Sum of Exponentially Decaying Contributions (SEDC) covariates
#' with optional wind frequency binning
#' @param from `SpatVector`(1). Point locations which contain point-source
#' covariate data.
#' @param locs sf/SpatVector(1). Locations where the sum of exponentially
#' decaying contributions are calculated.
#' @param locs_id character(1). Name of the unique id field in `point_to`.
#' @param sedc_bandwidth numeric(1).
#' Distance at which the source concentration is reduced to
#'  `exp(-3)` (approximately -95 %)
#' @param target_fields character(varying). Field names in characters.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @return a data.frame (tibble) or SpatVector object with input field names with
#'  a suffix \code{"_sedc"} where the sums of EDC are stored.
#'  Additional attributes are attached for the EDC information.
#'    - `attr(result, "sedc_bandwidth")``: the bandwidth where
#'  concentration reduces to approximately five percent
#'    - `attr(result, "sedc_threshold")``: the threshold distance
#'  at which emission source points are excluded beyond that
#' @note The function is originally from
#' [chopin](https://github.com/ropensci/chopin)
#' Distance calculation is done with terra functions internally.
#'  Thus, the function internally converts sf objects in
#'  \code{point_*} arguments to terra.
#'  The threshold should be carefully chosen by users.
#' @author Insang Song
#' @references
#' \insertRef{messier2012integrating}{amadeus}
#'
#' \insertRef{web_sedctutorial_package}{amadeus}
#' @examples
#' set.seed(101)
#' ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
#' nc <- terra::vect(ncpath)
#' nc <- terra::project(nc, "EPSG:5070")
#' pnt_locs <- terra::centroids(nc, inside = TRUE)
#' pnt_locs <- pnt_locs[, "NAME"]
#' pnt_from <- terra::spatSample(nc, 10L)
#' pnt_from$pid <- seq(1, 10)
#' pnt_from <- pnt_from[, "pid"]
#' pnt_from$val1 <- rgamma(10L, 1, 0.05)
#' pnt_from$val2 <- rgamma(10L, 2, 1)
#'
#' vals <- c("val1", "val2")
#' sum_edc(pnt_locs, pnt_from, "NAME", 1e4, vals)
#' @importFrom dplyr as_tibble
#' @importFrom dplyr left_join
#' @importFrom dplyr summarize
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr all_of
#' @importFrom dplyr across
#' @importFrom dplyr ungroup
#' @importFrom terra nearby
#' @importFrom terra distance
#' @importFrom terra buffer
#' @importFrom rlang sym
#' @export
# nolint end
sum_edc2 <-
  function(
    from = NULL,
    locs = NULL,
    locs_id = NULL,
    sedc_bandwidth = NULL,
    target_fields = NULL,
    wf = NULL,
    geom = FALSE
  ) {
    amadeus::check_geom(geom)
    if (!methods::is(locs, "SpatVector")) {
      locs <- try(terra::vect(locs))
    }
    if (!methods::is(from, "SpatVector")) {
      from <- try(terra::vect(from))
    }

    cn_overlap <- intersect(names(locs), names(from))
    if (length(cn_overlap) > 0) {
      warning(
        sprintf(
          "There are %d fields with the same name.
The result may not be accurate.\n",
          length(cn_overlap)
        )
      )
    }
    len_point_locs <- seq_len(nrow(locs))

    locs$from_id <- len_point_locs
    locs_buf <-
      terra::buffer(
        locs,
        width = sedc_bandwidth * 2,
        quadsegs = 90
      )

    from_in <- from[locs_buf, ]
    len_point_from <- seq_len(nrow(from_in))

    # len point from? len point to?
    from_in$to_id <- len_point_from
    dist <- NULL

    # near features with distance argument: only returns integer indices
    # threshold is set to the twice of sedc_bandwidth
    res_nearby <-
      terra::nearby(locs, from_in, distance = sedc_bandwidth * 2)
    # attaching actual distance
    dist_nearby <- terra::distance(locs, from_in)
    dist_nearby_df <- as.vector(dist_nearby)
    # adding integer indices
    dist_nearby_tdf <-
      expand.grid(
        from_id = len_point_locs,
        to_id = len_point_from
      )
    dist_nearby_df <- cbind(dist_nearby_tdf, dist = dist_nearby_df)

    # summary
    res_sedc <- res_nearby |>
      dplyr::as_tibble() |>
      dplyr::left_join(data.frame(locs)) |>
      dplyr::left_join(data.frame(from_in)) |>
      dplyr::left_join(dist_nearby_df)

    res_sedc$bearing <- mapply(
      get_bearing,
      res_sedc$lon,
      res_sedc$lat,
      res_sedc$LONGITUDE,
      res_sedc$LATITUDE
    )
    bearings_df = as.data.frame(cbind(res_sedc[[locs_id]], res_sedc$bearing))
    colnames(bearings_df) = c(locs_id, "bearings")

    res_sedc$freq <-
      match_wind_bin(
        bearings = bearings_df,
        wd_intervals = extract_intervals(wf$wd_bin),
        freq_data = wf,
        locs_id = locs_id
      )
    # per the definition in
    # https://mserre.sph.unc.edu/BMElab_web/SEDCtutorial/index.html
    # exp(-3) is about 0.05 * (value at origin)
    res_sedc <- res_sedc |>
      dplyr::mutate(w_sedc = freq * exp((-3 * dist) / sedc_bandwidth)) |>
      dplyr::group_by(!!rlang::sym(locs_id)) |>
      dplyr::summarize(
        dplyr::across(
          dplyr::all_of(target_fields),
          ~ sum(w_sedc * ., na.rm = TRUE)
        )
      ) |>
      dplyr::ungroup()
    idx_air <- grep("_AIR_", names(res_sedc))
    names(res_sedc)[idx_air] <-
      sprintf("%s_%05d", names(res_sedc)[idx_air], sedc_bandwidth)

    if (geom %in% c("sf", "terra")) {
      res_sedc <- merge(
        terra::as.data.frame(locs, geom = "WKT")[, c("site_id", "geometry")],
        res_sedc,
        "site_id"
      )
    }

    res_sedc_return <- amadeus::calc_return_locs(
      covar = res_sedc,
      POSIXt = TRUE,
      geom = geom,
      crs = terra::crs(from)
    )

    attr(res_sedc_return, "sedc_bandwidth") <- sedc_bandwidth
    attr(res_sedc_return, "sedc_threshold") <- sedc_bandwidth * 2

    return(res_sedc_return)
  }
