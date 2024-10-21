# HAPS_pollutantstats.r
# Author: Mariana Kassien
# This script produces a data frame of unique pollutants in the HAPS database and statistics on
# spatio-temporal data availablilty, detection, sampling frequencies and durations, and match to the Tox21 database.

library(openxlsx)
library(dplyr)
#library(plyr)
library(tidyr)
library(data.table)

#source("R/download_haps.R")
#source('R/AMA_LC2STDRatio.R')
#source('R/AMA_preprocessing.R')

# Establish years and directories
amayr = 2021 # version year for the AMA
allyears = (2018:2021)# years of AMA data
data.dir = "input/" # directory for all AMA files
results.dir = "output/" # directory for all results

if(!dir.exists(data.dir)){dir.create(data.dir)}
if(!dir.exists(results.dir)){dir.create(results.dir)}

### Mode function for strings
mode_STR <- function(x) {
  ux <- unique(x);
  ux[which.max(tabulate(match(x, ux)))] } # mode string in vector

### reading in pollutant CAS numbers
load(paste0(data.dir,"AMA_POLLUTANT_CODES_DICTIONARY.Rda")) #pollutant codes metadata

### reading in tox21 data to filter chemicals with tox data only
tox21=read.xlsx(paste0(data.dir,"Tox21.xlsx"))%>%
  mutate(CAS_nodash = gsub('-','',CASRN))
tox21_cas=unique(tox21$CAS_nodash)
rm(tox21) # remove tox21 data to save space


### Download data
#download_haps(directory_to_download = data.dir,
#                   directory_to_save = data.dir,
#                   download = TRUE,
#                   acknowledgement = TRUE,
#                   remove_command = TRUE,
#                   unzip = TRUE,
#                   remove_zip = TRUE)

### calculate ratios between local and standard conditions
  #AMA_LC2STDRatio(data.dir=data.dir,results.dir=results.dir,amayr=amayr,allyears=allyears)
### removing select data and derive local conditions from LC/STD ratios
  #AMA_preprocessing(data.dir=data.dir,results.dir=results.dir,amayr=amayr,allyears=allyears)

### Read in HAPS data and concatenate years
# Load data
filenames <- paste0('AMA',amayr,'_preprocessing_',allyears,'.Rda')
haps_file <- list.files(results.dir, full.names=F)[list.files(results.dir) %in% filenames]

if(length(haps_file)>1){
  # Loop to bind multiple years together if dates selected match
  # multiple years. Maybe there is a more efficient way to do this?
  for(n in 1:length(haps_file)){
  load(paste0(results.dir,haps_file[n]))
  if(n==1){
    AMA_f=AMA
  }else{
    AMA_f=rbind(AMA_f,AMA)
  }
    AMA=AMA_f
  }
  rm(AMA_f)
}else{
  # Load data
  load(haps_file)
    }

# Append pollutant CASNUM from data dictionary
AMA = AMA %>%
join(AMA_POLLUTANT_CODES_DICTIONARY[,c('AQS_PARAMETER_CODE','POLLUTANT_CASNUM')])

### Statistics of dates available per location
AMA_date_stats <- AMA %>%
  dplyr::group_by(AQS_PARAMETER_CODE, MONITOR_LATITUDE, MONITOR_LONGITUDE) %>%
  dplyr::summarise(distinct_dates = n_distinct(SAMPLE_DATE)) %>%
  dplyr::group_by(AQS_PARAMETER_CODE) %>%
  dplyr::summarise(
    max_DatesPerSite = max(distinct_dates),
    min_DatesPerSite = min(distinct_dates),
    mean_DatesPerSite = mean(distinct_dates)
  )

### All other statistics, join dates statistics at the end
AMA_stats <- AMA %>%
dplyr::group_by(AQS_PARAMETER_NAME, AQS_PARAMETER_CODE) %>%
dplyr::mutate(SAMPLING_FREQUENCY_MODE = mode_STR(as.numeric(SAMPLING_FREQUENCY_CODE)),
       SAMPLING_DURATION_MODE = mode_STR(DURATION_DESC)) %>%
    ungroup() %>%
  dplyr::group_by(AQS_PARAMETER_NAME, AQS_PARAMETER_CODE,SAMPLING_FREQUENCY_MODE,SAMPLING_DURATION_MODE) %>%
  dplyr::summarise(Distinct_Locations = n_distinct(paste(MONITOR_LATITUDE, MONITOR_LONGITUDE)),
        NonDetects = 100*sum(SAMPLE_VALUE_FLAG=="ND")/sum(!is.na(SAMPLE_VALUE_STD)),
        BelowMDL = 100*sum(BELOW_MDL_FLAG=="Y")/sum(!is.na(SAMPLE_VALUE_STD)),
        BelowALTMDL =100*sum(SAMPLE_VALUE_STD > ALTERNATE_MDL)/sum(!is.na(SAMPLE_VALUE_STD)),
        AboveMDL = 100*sum(SAMPLE_VALUE_FLAG=="" & BELOW_MDL_FLAG=="" & !is.na(SAMPLE_VALUE_STD) )/sum(!is.na(SAMPLE_VALUE_STD))) %>%
ungroup() %>%
join(AMA_POLLUTANT_CODES_DICTIONARY[,c('AQS_PARAMETER_CODE','POLLUTANT_CASNUM')]) %>%
mutate(T21_flag= (as.character(POLLUTANT_CASNUM) %in% tox21_cas))%>%
left_join(AMA_date_stats, by = "AQS_PARAMETER_CODE")


#saveRDS(AMA_stats, "input/HAPS_exploratory.RDS")
