################################################################################
##### unit and integration tests for EDGAR functions
# nolint start
################################################################################
##### download_edgar
testthat::test_that("download_edgar (no errors, yearly with sectors)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  download_edgar(
    species = "CO",
    temp_res = "yearly",
    sector_yearly = "ENE",
    year_range = c(2021, 2022),
    directory_to_save = directory_to_save,
    acknowledgement = TRUE,
    download = FALSE
  )
  commands_path <- paste0(directory_to_save, "/edgar_yearly_curl_commands.txt")
  commands <- read_commands(commands_path = commands_path)
  urls <- extract_urls(commands = commands, position = 6)
  url_status <- check_urls(urls = urls, size = length(urls), method = "HEAD")
  test_download_functions(
    directory_to_save = directory_to_save,
    commands_path = commands_path,
    url_status = url_status
  )
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (monthly, no sector)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  download_edgar(
    species = "SO2",
    temp_res = "monthly",
    directory_to_save = directory_to_save,
    acknowledgement = TRUE,
    download = FALSE
  )
  commands_path <- paste0(directory_to_save, "/edgar_monthly_curl_commands.txt")
  commands <- read_commands(commands_path = commands_path)
  urls <- extract_urls(commands = commands, position = 6)
  url_status <- check_urls(urls = urls, size = length(urls), method = "HEAD")
  test_download_functions(
    directory_to_save = directory_to_save,
    commands_path = commands_path,
    url_status = url_status
  )
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (single year)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  download_edgar(
    species = "NOx",
    temp_res = "yearly",
    sector_yearly = "IND",
    year_range = c(2022, 2022),
    directory_to_save = directory_to_save,
    acknowledgement = TRUE,
    download = FALSE
  )
  commands_path <- paste0(directory_to_save, "/edgar_yearly_curl_commands.txt")
  commands <- read_commands(commands_path = commands_path)
  urls <- extract_urls(commands = commands, position = 6)
  url_status <- check_urls(urls = urls, size = length(urls), method = "HEAD")
  test_download_functions(
    directory_to_save = directory_to_save,
    commands_path = commands_path,
    url_status = url_status
  )
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (invalid species)", {
  expect_error(
    download_edgar(
      species = "XYZ",
      temp_res = "yearly",
      sector_yearly = "ENE",
      year_range = c(2021, 2022),
      acknowledgement = TRUE,
      directory_to_save = paste0(tempdir(), "/e/")
    )
  )
})

testthat::test_that("download_edgar (incompatible output-format)", {
  expect_error(
    download_edgar(
      species = "CO",
      temp_res = "monthly",
      output = "flx",
      format = "txt",
      acknowledgement = TRUE,
      directory_to_save = paste0(tempdir(), "/e/")
    )
  )
})

testthat::test_that("download_edgar (VOC with sector_voc)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  download_edgar(
    species = "VOC",
    temp_res = "yearly",
    sector_voc = "solvents",
    year_range = c(2018, 2019),
    acknowledgement = TRUE,
    download = FALSE,
    directory_to_save = directory_to_save
  )
  commands_path <- paste0(directory_to_save, "/edgar_yearly_curl_commands.txt")
  commands <- read_commands(commands_path = commands_path)
  urls <- extract_urls(commands = commands, position = 6)
  url_status <- check_urls(urls = urls, size = length(urls), method = "HEAD")
  test_download_functions(
    directory_to_save = directory_to_save,
    commands_path = commands_path,
    url_status = url_status
  )
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (VOC without sector_voc should error)", {
  expect_error(
    download_edgar(
      species = "VOC",
      temp_res = "yearly",
      year_range = c(2020, 2021),
      acknowledgement = TRUE,
      directory_to_save = paste0(tempdir(), "/e/")
    )
  )
})

testthat::test_that("download_edgar (default year_range)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  download_edgar(
    species = "SO2",
    temp_res = "yearly",
    sector_yearly = "TRA",
    acknowledgement = TRUE,
    download = FALSE,
    directory_to_save = directory_to_save
  )
  commands_path <- paste0(directory_to_save, "/edgar_yearly_curl_commands.txt")
  commands <- read_commands(commands_path = commands_path)
  urls <- extract_urls(commands = commands, position = 6)
  url_status <- check_urls(urls = urls, size = length(urls), method = "HEAD")
  test_download_functions(
    directory_to_save = directory_to_save,
    commands_path = commands_path,
    url_status = url_status
  )
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (missing acknowledgement triggers error)", {
  expect_error(
    download_edgar(
      species = "CO",
      temp_res = "monthly",
      directory_to_save = paste0(tempdir(), "/e/")
    )
  )
})
################################################################################
##### process_edgar
testthat::test_that("process_edgar basic functionality", {
  withr::local_package("terra")

  edgar <- process_edgar(
    year = c(2018, 2019),
    voc = 1:2,
    sector_voc = c("BUILDINGS", "FUEL_EXPLOITATION"),
    path = testthat::test_path("..", "testdata", "edgar", "voc")
  )

  # expect function exists
  testthat::expect_true(is.function(process_edgar))

  # expect output is SpatRaster
  testthat::expect_s4_class(edgar, "SpatRaster")

  # expect raster has values
  testthat::expect_true(terra::hasValues(edgar))

  # expect CRS is not null
  testthat::expect_false(is.null(terra::crs(edgar)))

  # expect non-trivial spatial extent
  testthat::expect_false(any(c(0, 1) %in% dim(edgar)[1:2]))

  # expect layer names match expected format
  testthat::expect_true(all(grepl("^\\d+_[A-Z_]+_\\d{4}$", names(edgar))))

  # expect number of layers matches number of matching files
  testthat::expect_gt(dim(edgar)[3], 0)
})

testthat::test_that("process_edgar with extent cropping", {
  withr::local_package("terra")

  full <- process_edgar(
    year = 2018,
    voc = 1,
    sector_voc = "BUILDINGS",
    path = testthat::test_path("..", "testdata", "edgar", "voc")
  )

  cropped <- process_edgar(
    year = 2018,
    voc = 1,
    sector_voc = "BUILDINGS",
    path = testthat::test_path("..", "testdata", "edgar", "voc"),
    extent = terra::ext(-10, 10, -10, 10)
  )

  # expect output is smaller than full raster
  testthat::expect_lt(prod(dim(cropped)[1:2]), prod(dim(full)[1:2]))

  # expect output is still a SpatRaster
  testthat::expect_s4_class(cropped, "SpatRaster")
})

testthat::test_that("expect error for no matches", {
  testthat::expect_error(
    process_edgar(
      year = 2020,
      voc = 99, # assuming no such file
      sector_voc = "NOT_A_SECTOR",
      path = testthat::test_path("..", "testdata", "edgar", "voc")
    ),
    "No matching files found for the specified voc, year, and sector."
  )
})

testthat::test_that("expect error for invalid year format", {
  testthat::expect_error(
    process_edgar(
      year = c(2018, 2019, 2020),
      voc = 1,
      sector_voc = "BUILDINGS",
      path = testthat::test_path("..", "testdata", "edgar", "voc")
    ),
    "length\\(year\\) == 1 | length\\(year\\) == 2 is not TRUE"
  )
})

testthat::test_that("process_edgar works with a single year", {
  edgar <- process_edgar(
    year = 2018,
    voc = 2,
    sector_voc = "TRANSPORT",
    path = testthat::test_path("..", "testdata", "edgar", "voc")
  )

  testthat::expect_s4_class(edgar, "SpatRaster")
  testthat::expect_true(all(grepl("^2_TRANSPORT_2018$", names(edgar))))
})
################################################################################
##### calculate_edgar
testthat::test_that("calculate_edgar", {
  withr::local_package("terra")
  withr::local_package("data.table")
  radii <- c(0, 1000)
  site <- data.frame(lon = -96.8601, lat = 32.8201, AMA_SITE_CODE = "481130069")
  # expect function
  testthat::expect_true(
    is.function(calculate_edgar)
  )
  for (r in seq_along(radii)) {
    edgar <-
      process_edgar(
        year = c(2018, 2019),
        voc = 1,
        sector_voc = c("BUILDINGS", "FUEL_EXPLOITATION"),
        path = testthat::test_path("..", "testdata", "edgar", "voc")
      )
    edgar_covariate <-
      calculate_edgar(
        from = edgar,
        locs = data.table::data.table(site),
        locs_id = "AMA_SITE_CODE",
        radius = radii[r]
      )
    # set column names
    edgar_covariate <- calc_setcolumns(
      from = edgar_covariate,
      lag = 0,
      dataset = "edgar",
      locs_id = "AMA_SITE_CODE"
    )
    # expect output is data.frame
    testthat::expect_true(
      class(edgar_covariate) == "data.frame"
    )
    # expect 4 columns
    testthat::expect_true(
      ncol(edgar_covariate) == 4
    )
    # expect numeric value
    testthat::expect_true(
      class(edgar_covariate[, 4]) == "numeric"
    )
  }
  # with included geometry terra
  testthat::expect_no_error(
    edgar_covariate_terra <- calculate_edgar(
      from = edgar,
      locs = site,
      locs_id = "AMA_SITE_CODE",
      radius = 0,
      fun = "mean",
      geom = "terra"
    )
  )
  testthat::expect_equal(
    ncol(edgar_covariate_terra),
    4
  )
  testthat::expect_true(
    "SpatVector" %in% class(edgar_covariate_terra)
  )

  # with included geometry sf
  testthat::expect_no_error(
    edgar_covariate_sf <- calculate_edgar(
      from = edgar,
      locs = site,
      locs_id = "AMA_SITE_CODE",
      radius = 0,
      fun = "mean",
      geom = "sf"
    )
  )
  testthat::expect_equal(
    ncol(edgar_covariate_sf),
    5
  )
  testthat::expect_true(
    "sf" %in% class(edgar_covariate_sf)
  )

  testthat::expect_error(
    calculate_edgar(
      from = edgar,
      locs = site,
      locs_id = "AMA_SITE_CODE",
      radius = 0,
      fun = "mean",
      geom = TRUE
    )
  )
})
