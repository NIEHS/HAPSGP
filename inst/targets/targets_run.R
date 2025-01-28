# nolint start
target_run <-
  list(
    ############################################################################
    ############################################################################
    ###########################     CRITICAL TARGETS      ######################
    targets::tar_target(
      model_dates,
      command = seq(as.Date("2018-01-01"), as.Date("2021-12-31"), "day"), 
      description = "Model date sequence"
    )
    ,
     targets::tar_target(
      pred_dates,
       command = seq(as.Date("2021-01-01"), as.Date("2021-12-31"), "day"),
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
      command = c("Benzene","Chloroform"),
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
                return_format = "data.table"),
      description="Extract site locations"
     )
    ,
    targets::tar_target(
      process,
      command = process_haps(data=dailyavg,date = model_dates,sites_file="input/AMA_SITE_INFORMATION.Rda",mode = "available-data",
                data_field=vars,return_format = "data.table"),
      description="Process haps data"
     )
    ,
    ############################################################################
    ############################################################################
    ########################  covariates?   ##########################
     targets::tar_target(
      variables_gridmet,
      command = c("vs","th","sph","pr","tmmn","tmmx","srad"),
      description="gridmet variables"
     )
    ,
     targets::tar_target(
      buffer_radius,
      command = c(0),
      description="buffer radiuses"
     )
    ,
     targets::tar_target(
      gmet_process,
      command = gridmet_process(data=process, 
      dates_gridmet=model_dates, input_dir="input/covariates/gridmet/",
      variables_gridmet= variables_gridmet, radiuses=buffer_radius, output="merged",
      haps_locs=haps_locs),
      #pattern=cross(variables_gridmet,model_dates),
      description="Process covariates"
     )
    ,



    ############################################################################
    ############################################################################
    ########################  model fit and predict   ##########################
    targets::tar_target(
        name = pred_grid,
       command = "output/AGU/gridmet_coarsegrid_2021.rds",
       description="Prediction grid"
    ),
    
    targets::tar_target(
        name = model_fit,
       command = pgp_fit(data=gmet_process,
                        dates=model_dates, 
                        radiuses=buffer_radius)
    ),
    targets::tar_target(
        name = prediction,
        command = pgp_pred(pred_dates=pred_dates,fullmodel=model_fit,
        pred_grid=pred_grid, covnames=variables_gridmet,
        radiuses=buffer_radius),
        pattern=map(pred_dates)
    )

)
  #nolint end