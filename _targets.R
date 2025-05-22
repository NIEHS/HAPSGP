# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(plyr)
library(dplyr)
library(data.table)
library(tidyr)
library(openxlsx)
library(stringr)
library(amadeus)
library(PrestoGP)
library(crew)
library(qs2)
library(beethoven)
library(rsample)
library(spatialsample)
library(sf)
library(Metrics)
library(tigris)
library(fields)
library(gtools)

################################################################################
#############################      CONTROLLER      #############################
#############################      CONTROLLER      #############################
##### `controller_250` uses full allocation of workers (~4.0 Gb per worker).
controller_250 <- crew::crew_controller_local(
  name = "controller_250",
  workers = 250
)
##### `controller_100` uses 100 workers (~10.0 Gb per worker).
controller_100 <- crew::crew_controller_local(
  name = "controller_100",
  workers = 100
)
##### `controller_50` uses 50 workers (~20.0 Gb per worker).
controller_50 <- crew::crew_controller_local(
  name = "controller_50",
  workers = 50
)
##### `controller_25` uses 25 workers (~40.0 Gb per worker).
controller_25 <- crew::crew_controller_local(
  name = "controller_25",
  workers = 25
)
##### `controller_10` uses 10 workers (~90.0 Gb per worker).
controller_10 <- crew::crew_controller_local(
  name = "controller_10",
  workers = 10
)
##### `controller_1` uses 1 worker for sequential {lightGBM} models.
controller_1 <- crew::crew_controller_local(
  name = "controller_1",
  workers = 1
)


# Set target options:
tar_option_set(
  packages = c(
    "tibble",
    "targets",
    "plyr",
    "dplyr",
    "data.table",
    "tidyr",
    "openxlsx",
    "stringr",
    "amadeus",
    "PrestoGP",
    "crew",
    "qs2",
    "beethoven",
    "rsample",
    "spatialsample",
    "sf",
    "Metrics",
    "tigris",
    "fields",
    "gtools"
  ),
  #format = "qs",
  controller = crew::crew_controller_group(
    controller_250,
    controller_100,
    controller_50,
    controller_25,
    controller_10,
    controller_1
  ),
  resources = targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "controller_50")
  ),
  garbage_collection = 100,
  storage = "worker",
  retrieval = "worker",
  error = "abridge"
)

# Run the R scripts in the R/ folder with your custom functions:
#tar_source()
tar_source("R/AMA_select.R")
tar_source('R/AMA_LC2STDRatio.R')
tar_source('R/AMA_preprocessing.R')
tar_source('R/AMA_duration2daily.R')
tar_source('R/process_haps.R')
tar_source('R/gridmet_process.R')
tar_source("R/pgp_fit.R")
tar_source("R/pgp_pred.R")
tar_source("R/pred_test.R")
tar_source("R/pgp_cv.R")
tar_source("R/join_covariates.R")
tar_source("R/select_states.R")
tar_source("R/reduce_calc_pca.R")
tar_source("R/make_grid_state.R")
tar_source("R/post_calc_autojoin2.R")
tar_source("R/impute_all2.R")
tar_source("R/gridmet_cleanup.R")
tar_source("R/pgp_preprocessing.R")
tar_source("R/kriging_funs.R")

###########################      SOURCE TARGETS      ###########################
targets::tar_source("inst/targets/targets_critical.R")
targets::tar_source("inst/targets/targets_initiate.R")
targets::tar_source("inst/targets/targets_haps.R")
targets::tar_source("inst/targets/targets_covariates.R")
targets::tar_source("inst/targets/targets_covariates_predgrid.R")
targets::tar_source("inst/targets/targets_fit.R")
targets::tar_source("inst/targets/targets_soil_example.R")

##############################      PIPELINE      ##############################
list(
  # target_critical,
  # target_initiate,
  # target_haps,
  # target_covariates,
  # #target_covariates_predgrid,
  # target_fit,
  target_soil_example
)
