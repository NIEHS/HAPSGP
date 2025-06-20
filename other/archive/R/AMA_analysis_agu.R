# AMA_analysis_agu.R
# Analysis of HAPS subset for AGU
# nolint start
library(plyr)
library(dplyr)
library(data.table)
library(tidyr)
library(openxlsx)
library(stringr)


# functions to source
source("R/AMA_select.R")
source('R/AMA_LC2STDRatio.R')
source('R/AMA_preprocessing.R')
#source('R/AMA_duration2daily_mod.R')
source('R/AMA_duration2daily.R')
source('R/process_haps.R')

allyears = 2018:2021
chemlist= c("Benzene", "Carbon tetrachloride", "Chloroform", "Hexane")

#0. Subset HAPS of interest into a new folder for AGU analysis
AMA_select (data.dir="input/",results.dir="input/AGU/",allyears,chemlist)

#Restate input/output folders for the rest of the analysis
amayr = 2021 # version year for the AMA
data.dir = "input/AGU/"
results.dir = "output/AGU/"


#1. calculate ratios between local and standard conditions
AMA_LC2STDRatio(data.dir=data.dir,results.dir=results.dir,amayr=amayr,allyears=allyears) 

#2. removing select data and derive local conditions from LC/STD ratios
AMA_preprocessing(data.dir=data.dir,results.dir=results.dir,amayr=amayr,allyears=allyears) 

# calculate daily averages by pollutant/site/sampling duration
#Stopped here
#AMA_duration2daily_mod(results.dir=results.dir,amayr=amayr,allyears=allyears)
AMA_duration2daily(results.dir=results.dir,amayr=amayr,allyears=allyears)

# Add lat-lon data and select only relevant fields, rename parameters for use with covariates
vars=c("AMA_SITE_CODE","AQS_PARAMETER_NAME","AQS_PARAMETER_CODE", "DURATION_DESC","CONC_DAILY_UG_M3",
"CONC_DAILY_STD","MDL_DAILY_STD_UG_M3","ALTERNATE_MDL_DAILY","SAMPLING_FREQUENCY_DAILY","POC_COUNT","ZERO_COUNT")

haps_df=process_haps(
  path = "output/AGU/",
  date = c("2018-01-01", "2021-12-31"),
  sites_file="input/AGU/AMA_SITE_INFORMATION.Rda",
  mode = "available-data",
  data_field = vars,
  return_format = "data.table")

save(haps_df,file="output/AGU/haps_df.Rda")

#load(paste0(results.dir,"AMA2021_daily_2021.Rda"))
# nolint end