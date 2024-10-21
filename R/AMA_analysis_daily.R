###### CALCULATE DAILY AVERAGES FOR THE AMBIENT MONITORING ARCHIVE ######
#### EDITED FROM ORIGINAL FUNCTION BY EPA NAMED AMA_analysis.R
##### INITIALZE R #####

# sessionInfo():
# R version 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)
# other attached packages:
# dplyr_1.0.7, openxlsx_4.2.4, readr_2.0.1, NADA_1.6-1.1, survival_3.2-7, 
# reshape_0.8.8, data.table_1.14.0, plyr_1.8.6, stringr_1.4.0   
#

# set working directory
#setwd("/Users/kassienma/HAPS-GP/")

# load all libraries
#library(installr)
#library(plyr)
library(data.table)
#library(reshape)
#library(NADA)
library(readr)
library(openxlsx)
library(dplyr)
library(stringr) 
library(tidyr)

# functions to source
source('R/AMA_LC2STDRatio.R')
source('R/AMA_preprocessing.R')
source('R/AMA_duration2daily.R')


##### CREATE DIRECTORIES AND GLOBAL VARIABLES #####

## directory folder names and variables

amayr = 2021 # version year for the AMA
allyears = (2018:2021)# years of AMA data
data.dir = "input/" # directory for all AMA files
results.dir = "output/" # directory for all results

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