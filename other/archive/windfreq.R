#' Compute Wind Frequency Table
#' @description
#' The \code{windfreq()} function bins wind speed and wind direction into
#' specified intervals, computes the frequency or fraction of observations
#' in each bin, and returns the results in either table or data frame format.
#' @param mydata A data frame containing wind speed and wind direction data.
#' @param ws_col Character. Name of the column in `mydata` that contains
#' wind speed values.
#' @param wd_col Character. Name of the column in `mydata` that contains
#' wind direction values.
#' @param ws.int Numeric. Interval width for wind speed bins.
#' @param wd.int Integer. Number of bins for wind direction (must be a divisor
#' of 360; otherwise, it will be adjusted to the next larger divisor).
#' @param calm.thres Numeric. Threshold wind speed for "calm" conditions
#' (for default of 0, no "calm" bin will be defined.)
#' @param statistic Character. Specifies whether to return
#' absolute counts (`"count"`) or relative frequencies (`"fraction"`).
#' Default is `"count"`.
#' @param format Character. Output format: `"data.frame"` (default) or `"table"`.
#' @returns A frequency table of wind speed and direction bins, either as
#' a data frame or a table.
#' @examples
#' # Example dataset
#' set.seed(123)
#' mydata <- data.frame(wind_speed = runif(100, 0, 20), wind_dir = runif(100, 0, 360))
#'
#' # Compute wind frequency table
#' windfreq(mydata, ws_col = "wind_speed", wd_col = "wind_dir", ws.int = 5, wd.int = 8, 
#'          calm.thres = 0.5, statistic = "count", format = "data.frame")
#' @export
# nolint start
windfreq <- function(mydata, ws_col, wd_col, ws.int, wd.int, calm.thres = 0, 
                     statistic = c("count", "fraction"), format = c("data.frame", "table")) {
  
  # Ensure valid statistic and format arguments
  statistic <- match.arg(statistic)
  format <- match.arg(format)
  
  # Ensure specified columns exist in mydata
  if (!(ws_col %in% colnames(mydata))) stop("Error: Column '", ws_col, "' not found in mydata.")
  if (!(wd_col %in% colnames(mydata))) stop("Error: Column '", wd_col, "' not found in mydata.")
  
  # Extract wind speed and direction
  ws <- mydata[[ws_col]]
  wd <- mydata[[wd_col]]
  
  # 1. Define wind speed bins dynamically based on ws.int
  max_ws <- max(ws, na.rm = TRUE)  # Get the maximum wind speed value
  
  # Calculate wind speed bin edges
  ws_bins <- seq(0, max_ws, by = ws.int)
  if (ws_bins[length(ws_bins)] < max_ws) {
    ws_bins <- c(ws_bins, max_ws)  # Append max_ws if not included
  }
  
  # Add a calm bin if applicable
  ws_bins <- c(0, calm.thres[calm.thres > 0], ws_bins[ws_bins > calm.thres])
  
  # Create wind speed labels with interval notation
  ws_labels <- paste0("[", head(ws_bins, -1), "-", tail(ws_bins, -1), ")")
  
  # 2. Define wind direction bins based on wd.int

  # Ensure wd.int is a divisor of 360
  if (360 %% wd.int != 0) {
    original_wd_int <- wd.int
    
    # Find the next largest divisor of 360
    possible_divisors <- seq(wd.int, 360, by = 1)
    wd.int <- min(possible_divisors[360 %% possible_divisors == 0])
    
    # Print a warning if adjustment was made
    message("Warning: wd.int = ", original_wd_int, 
            " is not a divisor of 360. Adjusting to wd.int = ", wd.int, " for even binning.")
  }

  # Calculate the bin width
  wd_bin_width <- 360 / wd.int

  # Shift bins to center on cardinal directions (N, E, S, W)
  # Example: For 4 bins, we want centers at 0, 90, 180, 270
  wd_centers <- seq(0, 360 - wd_bin_width, length.out = wd.int)

  # Shift the bin edges to center on these points
  # Each bin spans half the bin width on either side of the center
  wd_bins <- (wd_centers - wd_bin_width / 2) %% 360

  # Ensure the bins wrap correctly
  wd_bins <- sort(c(wd_bins, 360))

  # Create wind direction labels with interval notation
  wd_labels <- paste0("[", head(wd_bins, -1), "-", tail(wd_bins, -1), ")")
  # 3. Bin the wind speed and wind direction
  mydata$ws_binned <- cut(ws,
                          breaks = ws_bins, 
                          right = FALSE, 
                          include.lowest = TRUE,
                          labels = ws_labels)
  
  mydata$wd_binned <- cut(wd, 
                          breaks = wd_bins, 
                          right = FALSE, 
                          include.lowest = TRUE,
                          labels = wd_labels)
  
  # 4. Compute frequency of (ws_binned, wd_binned) combinations
  freq_table <- table(mydata$ws_binned, mydata$wd_binned)
  
  if (statistic == "fraction") {
    freq_table <- (freq_table / sum(complete.cases(ws, wd))) 
  }
  
  if (format == "data.frame") {
    freq_table <- as.data.frame(as.table(freq_table))
    colnames(freq_table) <- c("ws_bin", "wd_bin", "freq")
  }
  
  return(freq_table)
}

# Auxiliary function: extract intervals from wind labels
extract_intervals <- function(bin_labels) {
  # Extract numeric values from labels like "[315-45)"
  bins <- gsub("[^0-9.\\-]", "", bin_labels) # Remove non-numeric characters
  bins <- strsplit(bins, "-") # Split at the "-"
  bins <- lapply(bins, as.numeric) # Convert to numeric
  bins <- do.call(rbind, bins) # Create matrix of start and end
  return(bins)
}

# nolint end