################################################################################
##### Define critical targets
# nolint start
target_critical <-
  list(
    ############################################################################
    ############################################################################
    ###########################     CRITICAL TARGETS      ######################
    ##### 1. chr_daterange controls all time-related targets for the entire
    #####    pipeline. This is the only target that needs to be changed to
    #####    update the pipeline with a new temopral range. Month and year
    #####    specific arguments are derived from the time range defined by
    #####    chr_daterange.
    targets::tar_target(
      chr_daterange,
      command = c("2021-01-01", "2021-12-31"),
      description = "Date range | critical"
    )
    ,
    ##### 2. chr_nasa_token sets the file path to the user's NASA Earthdata
    #####    account credentials. We can create a group credential file,
    #####    but this target is still critical since the CREDENTIALS
    #####    EXPIRE AT ~90 DAY INTERVALS. Regardless of the user or group
    #####    credential file, the token must be updated every 90 days.
    targets::tar_target(
      chr_nasa_token,
      command = readLines("/inst/extdata/nasa_token.txt"),
      description = "NASA Earthdata token | critical"
    )
    ,
    ##### 3. chr_mod06_links is the file path to the MOD06 links file. These
    #####    links must be manually downloaded per the `amadeus::download_modis`
    #####    function. The links are then stored in a CSV file that is read
    #####    by the function. The new file with links must be updated to match
    #####    the new date range.
    targets::tar_target(
      chr_mod06_links,
      #command = "/ddn/gs1/home/kassienma/beethoven/inst/extdata/mod06_links_2018_2022.csv",
      command = "/beethoven/inst/extdata/mod06_links_2018_2022.csv",
     description = "File of MOD06 links | critical"
    )
    ,
    ##### 4. chr_input_dir is the file path to the input directory. This target
    #####    controls where the raw data files are downloaded to and imported
    #####    from. This file path **MUST** be mounted to the container at run
    #####    time in the `run.sh` script.
    targets::tar_target(
      chr_input_dir,
      #command = "/ddn/gs1/group/set/Projects/NRT-AP-Model/input",
      command = "/set_input/",
      description = "Data directory | critical"
    )
    ,
    ##### 5. chr_dates_split controls the size of temporal splits. Splitting the
    #####    temporal range into smaller chunks allows for parallel processing
    #####    across multiple workers. It also allows for dispatching new dynamic
    #####    branches when the temporal range is updated.
    targets::tar_target(
      num_dates_split,
      command = 122,
      description = "Number of days in each temporal split | critical"
    ),

    targets::tar_target(
      model_dates,
      command = seq(as.Date(chr_daterange[1]), as.Date(chr_daterange[2]), "day"), 
      description = "Model date sequence"
    )
    ,
     targets::tar_target(
      pred_dates,
       command = seq(as.Date(chr_daterange[1]), as.Date(chr_daterange[2]), "day"), 
        description = "prediction dates"
    )
    ,
    targets::tar_target(
      data.dir,
      #command = "/ddn/gs1/home/kassienma/HAPSGP/input/",
      command = "/input/",
      description = "Input directory"
    )
    ,
    targets::tar_target(
      amayr,
      command = 2021,
      description = "AMA year"
    )
    ,
    targets::tar_target(
      allyears,
      command = unique(format.Date(model_dates,"%Y")),
      description = "Model years"
    )
    ,
    targets::tar_target(
      chemlist,
      command = c("Benzene","Hexane"),
      description = "Chemical list"
    )
    ,
    targets::tar_target(
      vars,
      command = c("AMA_SITE_CODE","AQS_PARAMETER_NAME",
      "AQS_PARAMETER_CODE", "DURATION_DESC","CONC_DAILY_UG_M3",
      "CONC_DAILY_STD","MDL_DAILY_STD_UG_M3","ALTERNATE_MDL_DAILY",
      "SAMPLING_FREQUENCY_DAILY","POC_COUNT","ZERO_COUNT"),
      description = "vars of interest"
    )
    ############################################################################
    ############################################################################
    ############################################################################
  )
#nolint end