#' Calculate Initial Bearing Between Two Geographic Coordinates
#' Computes the initial bearing (azimuth) in degrees between two geographic points specified by their longitude and latitude.
#' @param lon1 Numeric. Longitude of the starting point (in degrees).
#' @param lat1 Numeric. Latitude of the starting point (in degrees).
#' @param lon2 Numeric. Longitude of the destination point (in degrees).
#' @param lat2 Numeric. Latitude of the destination point (in degrees).
#' @return Numeric. The initial bearing in degrees, ranging from 0 to 360.
#' @details
#' The bearing is calculated using the spherical law of cosines and the `atan2()` function to ensure correct quadrant handling. The result is normalized to fall within the range [0, 360) degrees.
#' This implementation assumes a spherical Earth and may differ slightly from more precise ellipsoidal methods (e.g., Vincenty's formula).
#' @examples
#' # Bearing from Los Angeles to Paris
#' get_bearing(-118.25, 34.05, 2.35, 48.85)
#' # Bearing from New York to Madrid
#' get_bearing(-74.01, 40.71, -3.68, 40.42)
#' @export
# nolint start
get_bearing <- function(lon1, lat1, lon2, lat2) {
  # Convert degrees to radians
  lon1 <- lon1 * pi / 180
  lat1 <- lat1 * pi / 180
  lon2 <- lon2 * pi / 180
  lat2 <- lat2 * pi / 180

  # Calculate differences
  delta_lon <- lon2 - lon1

  # Compute bearing using atan2
  y <- sin(delta_lon) * cos(lat2)
  x <- cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(delta_lon)
  bearing_rad <- atan2(y, x)

  # Convert bearing from radians to degrees and normalize to [0, 360)
  bearing_deg <- (bearing_rad * 180 / pi + 360) %% 360

  return(bearing_deg)
}

#' Auxiliary function
#' Match Bearings to Wind Direction Frequency Bins
#'
#' @param bearings A numeric vector of wind direction angles (0–360) or a data.frame with columns "bearings" and site ID.
#' @param wd_intervals A matrix with two columns representing bin start and end angles (from extract_intervals()).
#' @param freq_data A data.frame with columns "wd_bin", "freq", and optionally a site ID column.
#' @param locs_id Optional. Name of the column in `freq_data` and `bearings` for site ID.
#'
#' @return A numeric vector of matched frequencies (same length as `bearings`).
match_wind_bin <- function(bearings, wd_intervals, freq_data, locs_id = NULL) {
  if (class(bearings) == "data.frame") {
    n <- nrow(bearings)
    site_id = bearings[[locs_id]]
  } else {
    n <- length(bearings)
  }
  result <- rep(NA_real_, n)

  for (i in seq_len(n)) {
    b <- as.numeric(bearings$bearings[i])

    # Subset frequency data if site matching is used
    if (!is.null(locs_id)) {
      site_val <- site_id[i]
      # Skip if no data for site
      freq_subset <- freq_data[freq_data[[locs_id]] == site_val, ]
      if (nrow(freq_subset) == 0) {
        next
      }
      wd_intervals_sub <- wd_intervals[freq_data[[locs_id]] == site_val, ]

      # Match bearing to interval
      for (j in seq_len(nrow(wd_intervals_sub))) {
        low <- wd_intervals_sub[j, 1]
        high <- wd_intervals_sub[j, 2]
        if (
          (low <= high && b >= low && b < high) ||
            (low > high && (b >= low || b < high))
        ) {
          result[i] <- freq_subset$freq[j]
          break
        }
      }
    } else {
      freq_subset <- freq_data
      # Skip if no data for site
      if (nrow(freq_subset) == 0) {
        next
      }
      # Match bearing to interval
      for (j in seq_len(nrow(wd_intervals))) {
        low <- wd_intervals[j, 1]
        high <- wd_intervals[j, 2]
        if (
          (low <= high && b >= low && b < high) ||
            (low > high && (b >= low || b < high))
        ) {
          result[i] <- freq_subset$freq[j]
          break
        }
      }
    }
  }
  return(result)
}

#nolint end
