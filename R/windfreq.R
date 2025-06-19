#' Compute Wind Frequency Table
#' @description
#' The \code{windfreq()} function bins wind speed and wind direction into
#' specified intervals, computes the frequency or fraction of observations
#' in each bin and returns the results in either table or data frame format,
#' and optionally computes results by group (e.g., site ID).
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
#' @param locs_id Optional character. Name of column in `mydata` for group ID (e.g., site).
#'
#' @param format Character. Output format: `"data.frame"` (default) or `"table"`.
#' @returns A frequency table of wind speed and direction bins, either as
#' a data frame or a table, with optional grouping.
#' @examples
#'
#' mydata <- data.frame(
#'   site = rep(c("A", "B"), each = 100),
#'   ws = runif(200, 0, 15),
#'   wd = runif(200, 0, 360)
#' )
#' windfreq(mydata, ws_col = "ws", wd_col = "wd", ws.int = 5, wd.int = 8,
#' calm.thres = 0.5, statistic = "count", format = "data.frame", locs_id = "site")
#' @export
windfreq <- function(
  mydata,
  ws_col,
  wd_col,
  ws.int,
  wd.int,
  calm.thres = 0,
  statistic = c("count", "fraction"),
  format = c("data.frame", "table"),
  locs_id = NULL
) {
  statistic <- match.arg(statistic)
  format <- match.arg(format)

  if (!(ws_col %in% names(mydata))) {
    stop("Column ", ws_col, " not found in mydata.")
  }
  if (!(wd_col %in% names(mydata))) {
    stop("Column ", wd_col, " not found in mydata.")
  }
  if (!is.null(locs_id) && !(locs_id %in% names(mydata))) {
    stop("Column ", locs_id, " not found in mydata.")
  }

  # Helper function to process one group (or entire dataset if locs_id is NULL)

  process_one <- function(subset_data) {
    ws <- subset_data[[ws_col]]
    wd <- subset_data[[wd_col]]

    # Return NA row if wind speed or direction is missing entirely
    if (all(is.na(ws)) || all(is.na(wd))) {
      freq_table <- data.frame(ws_bin = NA, wd_bin = NA, freq = NA)
      if (!is.null(locs_id)) {
        freq_table[[locs_id]] <- unique(subset_data[[locs_id]])
        freq_table <- freq_table[, c(locs_id, "ws_bin", "wd_bin", "freq")]
      }
    } else {
      max_ws <- max(ws, na.rm = TRUE)
      ws_bins <- seq(0, max_ws, by = ws.int)
      if (ws_bins[length(ws_bins)] < max_ws) {
        ws_bins <- c(ws_bins, max_ws)
      }
      if (calm.thres == 0 && !0 %in% ws_bins) {
        ws_bins <- sort(unique(c(0, ws_bins)))
      }
      if (calm.thres > 0) {
        ws_bins <- sort(unique(c(0, calm.thres, ws_bins[ws_bins > calm.thres])))
      }
      ws_labels <- paste0("[", head(ws_bins, -1), "-", tail(ws_bins, -1), ")")

      if (360 %% wd.int != 0) {
        original_wd_int <- wd.int
        wd.int <- min(seq(wd.int, 360, by = 1)[
          360 %% seq(wd.int, 360, by = 1) == 0
        ])
        message(
          "Warning: wd.int = ",
          original_wd_int,
          " is not a divisor of 360. Adjusting to wd.int = ",
          wd.int
        )
      }

      wd_width <- 360 / wd.int
      wd_centers <- seq(0, 360 - wd_width, length.out = wd.int)
      wd_starts <- (wd_centers - wd_width / 2) %% 360
      wd_ends <- (wd_centers + wd_width / 2) %% 360
      wd_intervals <- cbind(wd_starts, wd_ends)
      wd_labels <- paste0("[", wd_starts, "-", wd_ends, ")")
      assign_wd_bin <- function(angle) {
        for (i in seq_len(nrow(wd_intervals))) {
          start <- wd_intervals[i, 1]
          end <- wd_intervals[i, 2]
          if (
            (start < end && angle >= start && angle < end) ||
              (start > end && (angle >= start || angle < end))
          ) {
            return(wd_labels[i])
          }
        }
        return(NA_character_) # If no match found
      }

      subset_data$wd_binned <- vapply(wd, assign_wd_bin, character(1))

      subset_data$ws_binned <- cut(
        ws,
        breaks = ws_bins,
        right = FALSE,
        include.lowest = TRUE,
        labels = ws_labels
      )
      freq_table <- table(subset_data$ws_binned, subset_data$wd_binned)

      if (statistic == "fraction") {
        freq_table <- freq_table / sum(complete.cases(ws, wd))
      }

      if (format == "data.frame") {
        freq_table <- as.data.frame(as.table(freq_table))
        names(freq_table) <- c("ws_bin", "wd_bin", "freq")
        if (!is.null(locs_id)) {
          freq_table[[locs_id]] <- unique(subset_data[[locs_id]])
        }
      }
    }
    return(freq_table)
  }

  # Process grouped or full data
  if (!is.null(locs_id)) {
    split_data <- split(mydata, mydata[[locs_id]])
    result_list <- lapply(split_data, process_one)
    result <- do.call(rbind, result_list)
    rownames(result) <- NULL
  } else {
    result <- process_one(mydata)
  }

  return(result)
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
