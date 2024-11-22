#' Process HAPS data
#' @description
#' The \code{process_haps()} function imports and cleans HAPs data
#' @param path character giving data path
#' @param year numeric giving the year of HAPs data used
#' @param sites_file character(1) path to metadata file for sites' datum and lat-lon
#' @param mode character(1). One of "date-location" (all dates * all locations)
#' or "available-data" (date-location pairs with available data) or
#' "location" (unique locations).
#' @param data_field character. Vector with columns of interest to extract.
#' @param return_format character(1). `"terra"` or `"sf"` or `"data.table"`.
#' @param ... Placeholders.
#' @description Reads HAPs file of selected `year`.
#' @returns a `SpatVector` or sf object depending on the `return_format`
#' @author Mariana Kassien, Eva Marques, Insang Song
#' @importFrom terra rast
#' @importFrom terra metags
#' @importFrom terra vect
#' @importFrom terra project
#' @importFrom sf st_as_sf
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @export
process_haps <-
  function(
    path = NULL,
    date = c("2018-01-01", "2021-12-31"),
    sites_file="AMA_SITE_INFORMATION.Rda",
    mode = c("date-location", "available-data", "location"),
    data_field = NULL,
    return_format = c("terra", "sf", "data.table"),
    ...
  ) {
    # check inputs
    if (!is.character(path) || is.null(path)) {
      stop("path is not a character.")
    }
    if (!dir.exists(path)) {
      stop("path does not exist.")
    }
    if (!is.null(date)) {
      date <- try(as.Date(date))
      if (inherits(date, "try-error")) {
        stop("date has invalid format(s). Please check the values.")
      }
      if (length(date) != 2) {
        stop("date should be a character vector of length 2.")
      }
    } else {
      stop("date should be defined.")
    }

    # open HAPs files corresponding to the years
    date_start <- as.Date(date[1]) 
    date_end <- as.Date(date[2])
    date_sequence <- seq(date_start, date_end, "day")
    date_sequence <- as.character(date_sequence)
    year_vec=unique(format.Date(date_sequence,"%Y"))
    
    filenames <- paste0('AMA2021_daily_',year_vec,'.Rda')
    haps_file <-
      list.files(path, full.names=T)[list.files(path) %in% filenames]
    if (length(haps_file) == 0) {
      stop("HAPs data not available for this year.")
    }
    
    # Load data
    if(length(haps_file)>1){
      # Loop to bind multiple years together if dates selected match 
      # multiple years. Maybe there is a more efficient way to do this?
      for(n in 1:length(haps_file)){
      load(haps_file[n])
      AMA=daily #dirty fix for naming differences
      if(n==1){
        AMA_f=AMA
      }else{
        AMA_f=rbind(AMA_f,AMA)
      }
        AMA=AMA_f
        
      }
      rm(AMA_f)
    }else{
      # Load data
      load(haps_file)
      AMA=daily #dirty fix for naming differences
    }

    # Import sites metadata file to find Datum information
    #meta_file <- list.files(path, full.names=T)[list.files(path) %in% sites_file]
    load(sites_file)

    # Append latitude and longitude
    AMA <- dplyr::left_join(AMA, AMA_SITE_INFORMATION |> dplyr::select(AMA_SITE_CODE, MONITOR_LONGITUDE, MONITOR_LATITUDE), by = "AMA_SITE_CODE")
    
    # Fix dates to standard format
    AMA$SAMPLE_DATE <- as.Date(AMA$SAMPLE_DATE, format = "%m/%d/%Y")
    AMA$SAMPLE_DATE <- format(AMA$SAMPLE_DATE, "%Y-%m-%d")
    
    # select relevant fields only 
    AMA <- AMA |>
      dplyr::as_tibble() |>
      dplyr::filter(as.character(SAMPLE_DATE) %in% date_sequence)
    
      col_sel <- c("STATE_ABBR","AMA_SITE_CODE", "MONITOR_LONGITUDE", "MONITOR_LATITUDE")
    
    if (mode != "available-data") {
      AMA_v <- unique(AMA[, col_sel])
      names(AMA_v)[3:4] <- c("lon", "lat")
    } else {
      col_sel <- append(col_sel, "SAMPLE_DATE")
      col_sel <- append(col_sel, data_field)
      
      AMA_v <- AMA |>
        dplyr::select(dplyr::all_of(col_sel)) |>
        dplyr::distinct()
      
      names(AMA_v)[3:5] <- c("lon", "lat","time")
    }
    
    # subset mainland
    AMA_v <- data.table::as.data.table(AMA_v)
    AMA_v <- AMA_v[!grepl("^(AK|HI|PR|VI)", STATE_ABBR), ] 
    AMA_v <- AMA_v[,-1] # remove "STATE_ABBR" variable

    # Append datum to sites
    # Perform the join
    AMA_v <- dplyr::left_join(AMA_v, AMA_SITE_INFORMATION |> dplyr::select(AMA_SITE_CODE, DATUM), by = "AMA_SITE_CODE")
    
    # Assume locations with no datum are WGS84
    AMA_v$DATUM[is.na(AMA_v$DATUM) | AMA_v$DATUM=="UNKNOWN" ]="WGS84"
    
    # NAD83 to WGS84
    AMA_v_nad83 <-
      AMA_v[DATUM == "NAD83"]
    if(nrow(AMA_v_nad83)>0){
      AMA_v_nad83 <-
        terra::vect(
          AMA_v_nad83,
          keepgeom = TRUE,
          crs = "EPSG:4269"
        )
      AMA_v_nad83 <- terra::project(AMA_v_nad83, "EPSG:4326")
      
      # postprocessing: combine WGS84 and new WGS84 records
      AMA_v_nad83 <- as.data.frame(AMA_v_nad83)
      AMA_v_wgs <- AMA_v[DATUM == "WGS84"]
      final_sites <- rbind(AMA_v_wgs, AMA_v_nad83)
      }
    
    # NAD27 to WGS84
    AMA_v_nad27 <-
      AMA_v[DATUM == "NAD27"]
    if(nrow(AMA_v_nad27)>0){
      AMA_v_nad27 <-
        terra::vect(
          AMA_v_nad27,
          keepgeom = TRUE,
          crs = "EPSG:4267"
        )
      AMA_v_nad27 <- terra::project(AMA_v_nad27, "EPSG:4326")
      
      # postprocessing: combine WGS84 and new WGS84 records
      AMA_v_nad27 <- AMA_v_nad27[, seq(1, 3)]
      AMA_v_nad27 <- as.data.frame(AMA_v_nad27)
      final_sites <- rbind(final_sites, AMA_v_nad83)
      }
     final_sites <- final_sites[, .SD, .SDcols = -ncol(final_sites)] #Remove DATUM column
    
    if (mode == "date-location") {
      final_sites <-
        split(date_sequence, date_sequence) |>
        lapply(function(x) {
          fs_time <- final_sites
          fs_time$time <- x
          return(fs_time)
        })
      final_sites <- data.table::rbindlist(final_sites, fill = TRUE)
    }
    if (mode == "location") {
      final_sites <- unique(final_sites)
    }
    
    final_sites <-
      switch(
        return_format,
        terra =
          terra::vect(
            final_sites,
            keepgeom = TRUE,
            crs = "EPSG:4326"
          ),
        sf =
          sf::st_as_sf(
            final_sites,
            remove = FALSE,
            dim = "XY",
            coords = c("lon", "lat"),
            crs = "EPSG:4326"
          ),
        data.table = final_sites
      )
    
    return(final_sites)
  }
