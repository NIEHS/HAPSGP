# nolint start
#' Download Hazardous Air Pollutants data
#' @description
#' The \code{download_haps()} function accesses and downloads Hazardous 
#' Air Pollutants (HAPS) data from the 
#' [ Ambient Monitoring Archive (AMA) for the  Hazardous Air Pollutants (HAPs) ]
#' (https://www.epa.gov/amtic/amtic-ambient-monitoring-archive-haps).
#' @param url_aqs_download character(1).
#'  URL to the AQS pre-generated datasets.
#' @param directory_to_download character(1).
#'  Directory to download zip files from AQS data mart.
#' @param directory_to_save character(1).
#'  Directory to decompress zip files.
#' @param download logical(1). \code{FALSE} will generate a *.txt file
#' containing all download commands. By setting \code{TRUE} the function
#' will download all of the requested data files.
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param remove_command logical(1).
#' Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands. Default is FALSE.
#' @param unzip logical(1). Unzip zip files. Default \code{TRUE}.
#' @param remove_zip logical(1). Remove zip file from directory_to_download.
#' Default \code{FALSE}.
#' @param ... Placeholders.
#' @author Mariana Kassien, Insang Song, Mitchell Manware
#' @returns NULL; Separate comma-separated value (CSV) files of
#'  monitors and the daily representative values
#'  will be stored in \code{directory_to_save}.
#' @export
download_haps <-
  function(
    url_aqs_download = "https://www.epa.gov/system/files/other-files/2023-10/ama_haps_2021_r_files.zip",
    directory_to_download = "~/Downloads",
    directory_to_save = "~",
    download = FALSE,
    acknowledgement = TRUE,
    remove_command = TRUE,
    unzip = TRUE,
    remove_zip = TRUE,
    ...
  ) {
    #### 1. check for data download acknowledgement (this is an auxiliary function)
    download_permit(acknowledgement = acknowledgement)
    #### 2. check for null parameters (this is an auxiliary function)
    check_for_null_parameters(mget(ls()))
    #### 3. check for valid URL
    if (!(check_url_status(url_aqs_download))) {
      stop(paste(
        "Invalid URL, check EPA HAPS website for correct link:
        https://www.epa.gov/amtic/amtic-ambient-monitoring-archive-haps"
      ))
    }
    #### 4. directory setup
    directory_to_download <- download_sanitize_path(directory_to_download)
    directory_to_save <- download_sanitize_path(directory_to_save)
    download_setup_dir(directory_to_download)
    download_setup_dir(directory_to_save)
    #### 5. build download file name
    download_names <- paste(directory_to_download,
            "haps.zip",
            sep = "")
    #### 6. build download command
    download_commands <- paste0(
      "curl ",
      url_aqs_download,
      " --output ",
      download_names,
      "\n"
    )
    
    #### 7. initiate "..._curl_commands.txt"
    commands_txt <- paste0(
      directory_to_download,
      "haps_curl_commands.txt"
    )
    download_sink(commands_txt)
    
    #### 8. concatenate and print download commands to "..._curl_commands.txt"
    writeLines(download_commands)
    #### 9. finish "..._curl_commands.txt" file
    sink()
    #### 10. build system command
    system_command <- paste0(
      ". ",
      commands_txt,
      "\n"
    )
    #### 11. download data
    download_run(
      download = download,
      system_command = system_command
    )
    #### 12. unzip data
    download_unzip(
      file_name = download_names,
      directory_to_unzip = directory_to_save,
      unzip = unzip
    )
    download_remove_zips(
      remove = remove_zip,
      download_name = download_names
    )
    #### 13. remove command file
    download_remove_command(
      commands_txt = commands_txt,
      remove = remove_command
    )
  }
