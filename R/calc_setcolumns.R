#' Set column names
#' @description
#' Apply standard column names to calculated covariates consistent with
#' the requirements of the \code{beethoven} package. Column names follow fixed
#' format of 3 character data genre, 2 - 15 character variable code, 1 digit
#' temporal lag, and 5 digit buffer radius (in meters). Variable code character
#' range is required to retain interpretable column names across datasets.
#' @note
#' \code{beethoven} utilizes point, 1km, and 10km radius buffer distance for
#' covariate calculation, and therefore the buffer radius column is
#' padded to 5 digits. If provided a buffer radius greater than 5 digits,
#'  \code{calc_setcolumns()} will expand to the number of digits. (ie. buffer
#' radius of 100km = CCC_CCCCC_I_100000).
#' @param from data.frame(1) or SpatVector(1). Calculated covariates as
#' returned from \code{calc_covariates()} or a source specific covariate
#' function.
#' @param lag integer(1). Temporal lag.
#' @param dataset character(1). Covariate parent dataset.
#' @param locs_id character(1). Column containing identifier for each unique
#' coordinate location.
#' @keywords internal auxiliary
#' @importFrom stringi stri_pad
#' @return a data.frame or SpatVector object (depending on `from`)
#' @export
calc_setcolumns <- function(
  from,
  lag,
  dataset,
  locs_id
) {
  #### check from is data.frame
  stopifnot(class(from) %in% c("data.frame", "SpatVector"))
  #### original names
  names_from <- colnames(from)
  #### copy
  names_return <- names_from
  #### identifier
  id_index <- which(names_from == locs_id)
  names_return[id_index] <- locs_id
  #### time
  time_index <- which(
    names_from %in% c("time", "year", "date", "hour")
  )
  stopifnot(length(time_index) <= 1)
  names_return[time_index] <- "time"
  #### description (for time period coverage)
  description_index <- which(names_from == "description")
  stopifnot(length(description_index) <= 1)
  names_return[description_index] <- "description"
  #### geometry
  geometry_index <- which(names_from == "geometry")
  stopifnot(length(geometry_index) <= 1)
  names_return[geometry_index] <- "geometry"
  #### latitude and longitude
  lat_index <- which(
    tolower(names_from) %in% c("lat", "latitude")
  )
  stopifnot(length(lat_index) <= 1)
  names_return[lat_index] <- "lat"
  lon_index <- which(
    tolower(names_from) %in% c("lon", "longitude")
  )
  stopifnot(length(lon_index) <= 1)
  names_return[lon_index] <- "lon"
  #### vertical pressure level
  level_index <- which(names_from == "level")
  stopifnot(length(level_index) <= 1)
  names_return[level_index] <- "level"
  #### dataset and genre
  datasets <- c(
    "aqs",
    "ecoregions",
    "geos",
    "gmted",
    "koppen geiger",
    "merra2",
    "modis",
    "narr",
    "nlcd",
    "hms",
    "groads",
    "pop",
    "tri",
    "nei",
    "gridmet",
    "terraclimate",
    "edgar"
  )
  stopifnot(dataset %in% datasets)
  genre <- substr(dataset, 1, 3)
  #### covariates
  cov_index <- which(
    !(c(
      names_from %in%
        c(
          locs_id,
          "geometry",
          "time",
          "lat",
          "lon",
          "level",
          "description"
        )
    ))
  )
  for (c in seq_along(cov_index)) {
    name_covariate <- names_return[cov_index[c]]
    name_split <- strsplit(name_covariate, "_")[[1]]
    name_variable <- tolower(name_split[1])
    name_variable <- gsub(
      "\\.",
      "",
      gsub(
        "-",
        "",
        gsub(
          " ",
          "",
          name_variable
        )
      )
    )
    nchar_radius <- nchar(as.character(name_split[2]))
    width_radius <- ifelse(nchar_radius > 5, nchar_radius, 5)
    name_new <- paste0(
      genre,
      "_",
      strsplit(
        name_variable,
        1,
        15
      ),
      "_",
      lag,
      "_",
      stringi::stri_pad(
        name_split[2],
        width = width_radius,
        pad = 0,
        side = "left"
      )
    )
    names_return[cov_index[c]] <- toupper(name_new)
  }
  #### check for unique names
  stopifnot(length(names_return) == length(unique(names_return)))
  return(from)
}
