# nolint start
target_run <-
  list(
    ############################################################################
    ############################################################################
    ###########################     CRITICAL TARGETS      ######################
    targets::tar_target(
      model_dates,
      command = c("2021-01-01", "2021-01-08"),
      description = "Model dates"
    )
    ,
     targets::tar_target(
      pred_dates,
      command = c("2021-01-01", "2021-01-08"),
      description = "prediction dates"
    )
    ,
    targets::tar_target(
      data.dir,
      command = "/ddn/gs1/home/kassienma/HAPSGP/input/",
      description = "Input directory"
    )
    ,
    targets::tar_target(
      results.dir,
      command = "/ddn/gs1/home/kassienma/HAPSGP/output/targets_test/",
      description = "Output directory"
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
      command = 2018:2021,
      description = "Model years"
    )
    ,
    targets::tar_target(
      chemlist,
      command = c("Benzene","Hexane","Chloroform"),
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
    ,
    ############################################################################
    ############################################################################
    ########################  AMA preprocessing   ##############################
    targets::tar_target(
      select,
      command = AMA_select (data.dir=data.dir,results.dir=results.dir,allyears=allyears,chemlist=chemlist),
      description="select chemicals"
     )
     ,
    targets::tar_target(
      LC2STD,
      command = AMA_LC2STDRatio(filelist=select,results.dir=results.dir,amayr=amayr,allyears=allyears), 
      description="Local to STD ratio"
     )
     ,
     targets::tar_target(
      LC2STD_site,
      command = AMA_LC2STDRatio_Site(filelist=LC2STD,results.dir=results.dir,amayr=amayr,allyears=allyears), 
      description="L2STD sites"
     )
     ,
    targets::tar_target(
      preprocess,
      command = AMA_preprocessing(filelist_ama=select,filelist_lcstd=LC2STD,file_lcstdsite=LC2STD_site,results.dir=results.dir,amayr=amayr,allyears=allyears),
      description="Preprocessing"
     )
     ,
    targets::tar_target(
      dailyavg,
      command = AMA_duration2daily(filelist=preprocess,data.dir=data.dir,results.dir=results.dir,amayr=amayr,allyears=allyears),
      description="Daily average"
     )
   #  ,

    ############################################################################
    ############################################################################
    ########################  model fit and predict   ##########################
  #  targets::tar_target(
  #      name = model_fit,
  #      command = pgp_fit(data="output/AGU/haps_gridmet_208-2021.rds",dates=pred_dates)
  #      # format = "qs" # Efficient storage for general data objects.
  #  ),
  #  targets::tar_target(
  #      name = prediction,
  #      command = pgp_pred(pred_dates=pred_dates,model=model_fit,
  #      pred_grid="output/AGU/gridmet_coarsegrid_2021.rds")
  #  )

)
  #nolint end