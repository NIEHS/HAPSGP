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

# Auxiliary function
# Match bearing to the appropriate wind direction bin
match_bin <- function(bearing, wd_intervals, freq_values) {
  match_index <- which(
    (bearing >= wd_intervals[, 1] & bearing < wd_intervals[, 2]) |
    (wd_intervals[, 1] > wd_intervals[, 2] & 
     (bearing >= wd_intervals[, 1] | bearing < wd_intervals[, 2]))
  )
  if (length(match_index) == 0) return(NA)  # No match found
  return(freq_values[match_index])
}

#nolint end