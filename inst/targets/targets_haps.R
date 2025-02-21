# nolint start
target_haps <-
  list(
    ############################################################################
    ############################################################################
    ########################  AMA preprocessing   ##############################
    targets::tar_target(
      select,
      command = AMA_select (data.dir=data.dir,allyears=allyears,chemlist=chemlist),
      description="select chemicals"
     )
     ,
    targets::tar_target(
      LC2STD,
      command = AMA_LC2STDRatio(filelist=select), 
      description="Local to STD ratio"
     )
     ,
     targets::tar_target(
      LC2STD_site,
      command = AMA_LC2STDRatio_Site(filelist=LC2STD), 
      description="L2STD sites"
     )
     ,
    targets::tar_target(
      preprocess,
      command = AMA_preprocessing(filelist_ama=select,filelist_lcstd=LC2STD,AMA_RatioSite=LC2STD_site),
      description="Preprocessing"
     )
     ,
    targets::tar_target(
      dailyavg,
      command = AMA_duration2daily(filelist=preprocess,data.dir=data.dir,amayr=amayr),
      description="Daily average"
     )
     ,
    targets::tar_target(
      haps_locs,
      command = process_haps(data=dailyavg,date = model_dates,sites_file="input/AMA_SITE_INFORMATION.Rda",mode = "location",
                return_format = "sf"),
      description="Extract site locations"
     )
    ,
    targets::tar_target(
      process,
      command = process_haps(data=dailyavg,date = model_dates,sites_file="input/AMA_SITE_INFORMATION.Rda",mode = "available-data",
                data_field=vars,return_format = "data.table"),
      description="Process haps data"
     )
  )

  #nolint end