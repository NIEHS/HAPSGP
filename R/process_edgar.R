# nolint start
#' Process EDGAR data
#' @description
#' The \code{process_edgar()} function imports and cleans EDGAR inventory
#' yearly VOC speciation data, returning a single `SpatRaster` object.
#' @param year numeric(1 or 2). Year (1) or start and end Years (2).
#' @param path character(1). Directory with downloaded netCDF (.nc) files.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param voc Integer vector. Used for VOC speciation in version
#'  "8.1_voc". Accepts integers from 1 to 25. See:
#'  https://edgar.jrc.ec.europa.eu/dataset_ap81_VOC_spec#p3  for reference on
#'  speciation groups and VOC numbers.
#' @param sector_voc Character vector. Emission sectors for VOC speciation
#'  data. Supported values: "AGRICULTURE", "BUILDINGS", "FUEL_EXPLOITATION",
#' "IND_COMBUSTION", "IND_PROCESSES", "POWER_INDUSTRY", "TRANSPORT", "WASTE".
#' @param ... Placeholders.
#' @note
#' Layer names of the returned `SpatRaster` object contain the variable acronym,
#' and date.
#' @author Mariana Kassien
#' @return a `SpatRaster` object
#' @importFrom terra rast
#' @importFrom terra time
#' @importFrom terra subset
#' @examples
#' ## NOTE: current version only supports processing of VOC speciation data
#' ##       in .nc format
#' \dontrun{
#' edgar_voc <- process_edgar(
#'   path = "./data/edgar/voc/data_files/"
#'   year = c(2018, 2021),
#'   voc = 1:3,
#'   sector_voc = c(
#'     "AGRICULTURE",
#'     "BUILDINGS")
#' )
#' }
#' @export
# nolint end
process_edgar <- function(
  year = c(2018, 2021),
  path = NULL,
  extent = NULL,
  voc = 1:25,
  sector_voc = c(
    "AGRICULTURE",
    "BUILDINGS",
    "FUEL_EXPLOITATION",
    "IND_COMBUSTION",
    "IND_PROCESSES",
    "POWER_INDUSTRY",
    "TRANSPORT",
    "WASTE"
  ),
  ...
) {
  #### directory setup
  path <- download_sanitize_path(path)
  #### check years
  stopifnot(length(year) == 1 | length(year) == 2)
  if (length(year) == 2) {
    year <- seq(year[1], year[2])
  }
  #### check for voc
  check_for_null_parameters(mget(ls()))

  #### identify file paths
  voi <- paste0("_voc", voc, "_")
  data_paths <- list.files(
    path,
    pattern = paste(voi, collapse = "|"),
    full.names = TRUE
  )
  data_paths <- data_paths[grep("\\.nc$", data_paths)]

  #### Filter by year
  year_patterns <- paste0("_", year, "_")
  data_paths <- grep(
    paste(year_patterns, collapse = "|"),
    data_paths,
    value = TRUE
  )

  #### Filter by sector_voc
  sector_patterns <- paste0("_", sector_voc, collapse = "|")
  data_paths <- grep(sector_patterns, data_paths, value = TRUE)

  if (length(data_paths) == 0) {
    stop(
      "No matching files found for the specified voc, year, and sector."
    )
  }

  #### Loop over each file and process
  data_full <- terra::rast()
  for (p in seq_along(data_paths)) {
    file_path <- data_paths[p]
    file_name <- basename(file_path)

    # Extract voc, year and sector_voc from filename
    voc_number <- stringr::str_extract(file_name, "voc\\d+_")
    year_matches <- stringr::str_extract_all(file_name, "\\d{4}")[[1]]
    if (length(year_matches) < 2) {
      stop(paste("Couldn't find second year in filename:", file_name))
    }
    year_extracted <- year_matches[2]
    sector_matched <- sector_voc[which.max(sapply(sector_voc, function(s) {
      grepl(s, file_name)
    }))]

    # Load raster (apply extent if provided)
    if (!is.null(extent)) {
      data_year <- terra::rast(file_path, win = extent)
    } else {
      data_year <- terra::rast(file_path)
    }

    # Construct standardized layer name
    layer_name <- paste0(
      voc_number,
      gsub("_", "", sector_matched),
      "_",
      year_extracted
    )
    names(data_year) <- layer_name

    # Add metadata
    terra::varnames(data_year) <- paste0(
      voc_number,
      gsub("_", "", sector_matched)
    )
    terra::longnames(data_year) <- paste(
      gsub("_", " ", layer_name)
    )

    # Combine
    data_full <- c(data_full, data_year, warn = FALSE)
  }

  # Return stacked SpatRaster with layers named according to voc, sector, year
  return(data_full)
}
