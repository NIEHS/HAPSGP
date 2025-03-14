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

# library(tarchetypes) # Load other packages as needed.

################################################################################
#############################      CONTROLLER      #############################
# Get the value of SLURM_CPUS_PER_TASK environment variable
cpus_per_task <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "30"))

# Define the controller with dynamic workers based on cpus-per-task
default_controller <- crew::crew_controller_local(
  name = "default_controller",
  workers = cpus_per_task,
  seconds_idle = 30
)

# Set target options:
tar_option_set(
  packages = c("tibble","targets","plyr","dplyr","data.table","tidyr","openxlsx",
  "stringr","amadeus","PrestoGP","crew","qs2","beethoven","rsample","spatialsample",
  "sf","Metrics","tigris"),
  #format = "qs",
  controller = crew_controller_group(default_controller),
  resources = tar_resources(
    crew = tar_resources_crew(
      controller = "default_controller"
   )
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

###########################      SOURCE TARGETS      ###########################
targets::tar_source("inst/targets/targets_critical.R")
targets::tar_source("inst/targets/targets_initiate.R")
targets::tar_source("inst/targets/targets_haps.R")
targets::tar_source("inst/targets/targets_covariates.R")
targets::tar_source("inst/targets/targets_covariates_NC.R")
targets::tar_source("inst/targets/targets_covjoin.R")
targets::tar_source("inst/targets/targets_predgrid.R")
targets::tar_source("inst/targets/targets_fit.R")

##############################      PIPELINE      ##############################
list(
  target_critical,
  target_initiate,
  target_haps,
  target_covariates,
  target_covariates_nc,
  target_covjoin,
  target_predgrid,
  target_fit
)
