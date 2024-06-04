# nolint start
#' Download Hazardous Air Pollutants data
#' @description
#' The \code{download_haps_data()} function accesses and downloads Hazardous 
#' Air Pollutants (HAPS) data from the 
#' [ Ambient Monitoring Archive (AMA) for the  Hazardous Air Pollutants (HAPs) ]
#' (https://www.epa.gov/amtic/amtic-ambient-monitoring-archive-haps).
#' @param parameter_code integer(1). length of 5.
#'  EPA pollutant parameter code. For details, please refer to
#'  [AQS parameter codes](https://aqs.epa.gov/aqsweb/documents/codetables/parameters.html)
# nolint end
#' @param resolution_temporal character(1).
#'  Name of column containing POC values.
#'  Currently, no value other than `"daily"` works.
#' @param url_aqs_download character(1).
#'  URL to the AQS pre-generated datasets.
#' @param year_start integer(1). length of 4.
#'  Start year for downloading data.
#' @param year_end integer(1). length of 4.
#'  End year for downloading data.
#' @param directory_to_download character(1).
#'  Directory to download zip files from AQS data mart.
#' @param directory_to_save character(1).
#'  Directory to decompress zip files.
#' @param download logical(1). \code{FALSE} will generate a *.txt file
#' containing all download commands. By setting \code{TRUE} the function
#' will download all of the requested data files.
#' @param remove_command logical(1).
#' Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands. Default is FALSE.
#' @param unzip logical(1). Unzip zip files. Default \code{TRUE}.
#' @param remove_zip logical(1). Remove zip file from directory_to_download.
#' Default \code{FALSE}.
#' @param remove_metadata logical(1). Work in progress. Remove metadata files 
#' from directory_to_save.
#' Default \code{FALSE}.
#' @author Mariana Kassien, Insang Song, Mitchell Manware
#' @returns NULL; Separate comma-separated value (CSV) files of
#'  monitors and the daily representative values
#'  will be stored in \code{directory_to_save}.
#' @export
download_haps_data <-
  function(
    #parameter_code = 88101,
    #resolution_temporal = "daily",
    year_start = 2018,
    year_end = 2021,
    url_aqs_download = "https://www.epa.gov/system/files/other-files/2023-10/ama_haps_2021_r_files.zip",
    directory_to_download = "~/Downloads",
    directory_to_save = "~",
    download = TRUE,
    remove_command = TRUE,
    unzip = TRUE,
    remove_zip = TRUE,
   # remove_metadata = TRUE
  ) {

    #### 1. check for null parameters
    check_for_null_parameters(mget(ls()))
    
    #### 2. check for valid URL
    if (!(check_url_status(url_aqs_download))) {
      stop(paste(
        "Invalid URL, check EPA HAPS website for correct link:
        https://www.epa.gov/amtic/amtic-ambient-monitoring-archive-haps"
      ))
    }
    
    #### 3. directory setup
    directory_to_download <- download_sanitize_path(directory_to_download)
    directory_to_save <- download_sanitize_path(directory_to_save)
    download_setup_dir(directory_to_download)
    download_setup_dir(directory_to_save)
    
    #### 4. define year sequence
    year_sequence <- seq(year_start, year_end, 1)
    
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
    
    #### 14. remove command file
    download_remove_command(
      commands_txt = commands_txt,
      remove = remove_command
    )
  }
