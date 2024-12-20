###### CALCULATE ANNUAL AVERAGES FOR THE AMBIENT MONITORING ARCHIVE ######

##### INITIALZE R #####

#
# R operational notes:
# (1) to install, use the following command: install.packages("LIBRARY NAME")
# (2) to update all packages, use the following command: update.packages(checkBuilt = TRUE)
# (3) to install an updated version of R, use the following command: updateR()
#
# sessionInfo():
# R version 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)
# other attached packages:
# dplyr_1.0.7, openxlsx_4.2.4, readr_2.0.1, NADA_1.6-1.1, survival_3.2-7, 
# reshape_0.8.8, data.table_1.14.0, plyr_1.8.6, stringr_1.4.0   
#

# set working directory
setwd('SET WORKING DIRECTORY')

# load all libraries
library(installr)
library(plyr)
library(data.table)
library(reshape)
library(NADA)
library(readr)
library(openxlsx)
library(dplyr)
library(stringr) 
library(tidyr)

# functions to source
source('AMA_LC2STDRatio.R')
source('AMA_preprocessing.R')
source('AMA_duration2daily.R')
source('AMA_daily2annual.R')
source('AMA_file.R')

##### CREATE DIRECTORIES AND GLOBAL VARIABLES #####

## directory folder names and variables

amayr = 2021 # version year for the AMA
allyears = 1990:2021 # years of AMA data
data.dir = paste0('../RData',amayr,'/') # directory for all AMA files
results.dir = paste0('../RDataCreated',amayr,'/') # directory for all results

## create directory folders if they do not already exist

if(!dir.exists(data.dir)){dir.create(data.dir)}
if(!dir.exists(results.dir)){dir.create(results.dir)}

##### CALCULATE ANNUAL AVERAGES #####

# calculate ratios between local and standard conditions
AMA_LC2STDRatio(data.dir=data.dir,results.dir=results.dir,amayr=amayr,allyears=allyears) 

# removing select data and derive local conditions from LC/STD ratios
AMA_preprocessing(data.dir=data.dir,results.dir=results.dir,amayr=amayr,allyears=allyears) 

# calculate daily averages by pollutant/site/sampling duration
AMA_duration2daily(results.dir=results.dir,amayr=amayr,allyears=allyears)

# calculate annual averages from daily data
AMA_daily2annual(results.dir=results.dir,amayr=amayr,allyears=allyears) 

# create the final excel file for the Archive
AMA_file(data.dir=data.dir,results.dir=results.dir,amayr=amayr,allyears=allyears)