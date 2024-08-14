# HAPS exploratory analysis
#setwd("/ddn/gs1/home/kassienma/HAPS-GP/R")
source("download_haps.R")
source("process_haps.R")
library(amadeus)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(gridExtra)


vars=c("AMA_SITE_CODE", "AQS_POC" , "SAMPLE_DATE", "DURATION_DESC",
       "SAMPLE_VALUE_REPORTED","AQS_UNIT_CODE","UNIT_DESC",
       "SAMPLING_FREQUENCY_CODE","SAMPLE_VALUE_STD",
       "SAMPLE_VALUE_STD_FINAL_UG_M3","SAMPLE_VALUE_STD_FINAL_TYPE",
       "AQS_PARAMETER_CODE_FINAL","AQS_PARAMETER_NAME_FINAL","ALTERNATE_MDL",
       "MDL_STD_UG_M3", "MDL_TYPE","AQS_NULL_DATA_CODE" ,"AQS_METHOD_CODE",
       "SAMPLE_COLLECTION_DESC","SAMPLE_ANALYSIS_DESC", "SAMPLE_VALUE_FLAG",
       "BELOW_MDL_FLAG","CENSUS_TRACT_ID_2010","CENSUS_TRACT_ID_2020",
       "MONITOR_LATITUDE" ,"MONITOR_LONGITUDE")


#download_haps(directory_to_download = "~/HAPS-GP/input",
#                   directory_to_save = "~/HAPS-GP/input",
#                   download = FALSE,
#                   acknowledgement = TRUE,
#                   remove_command = TRUE,
#                   unzip = TRUE,
#                   remove_zip = TRUE)

#dates=c("2018-01-01", "2021-12-31")

#data <- process_haps(
#    path = "/ddn/gs1/home/kassienma/HAPS-GP/input",
#    date = dates,
#    sites_file="AMA_SITE_INFORMATION.Rda",
#    mode = "available-data",
#    data_field = vars,
#    return_format = "data.table")

# Save data.table so I don't have to keep loading it every time
#saveRDS(data, "/ddn/gs1/home/kassienma/HAPS-GP/input/HAPS_data_exploratory.rds")

#data <- readRDS("/ddn/gs1/home/kassienma/HAPS-GP/input/HAPS_data_exploratory.rds")
data <- readRDS("input/HAPS_data_exploratory.rds") #relative path

#Number of chemicals
length(unique(data$AQS_PARAMETER_NAME_FINAL))
#132 chemicals between 2018 and 2021

#Spatial extent and missing-ness ####
  #How many distinct sites are there?
  distinct_locations <- data %>%
  group_by(AQS_PARAMETER_NAME_FINAL) %>%
  summarise(Distinct_Locations = n_distinct(paste(lat, lon))) %>%
  arrange(desc(Distinct_Locations))

  #How do these change per year?
  # Parse year from the time column
  data <- data %>%
  mutate(year = year(ymd(time)))

# Count distinct locations (lat-lon pairs) per AQS_PARAMETER_CODE_FINAL and per year
  distinct_locations_per_year <- data %>%
  group_by(AQS_PARAMETER_NAME_FINAL, year) %>%
  summarise(Distinct_Locations = n_distinct(paste(lat, lon))) %>%
  arrange(AQS_PARAMETER_NAME_FINAL, year)
# Make wide table
  distinct_locations_wide <- distinct_locations_per_year %>%
  pivot_wider(names_from = year, values_from = Distinct_Locations, names_prefix = "Year_")

# Join the total distinct locations with the wide table
  distinct_locations <- distinct_locations %>%
  left_join(distinct_locations_wide, by = "AQS_PARAMETER_NAME_FINAL",)

# Plot Histograms to get an idea where to cut off
par(mfrow=c(1,1))
y=hist(distinct_locations$Distinct_Locations, main="Distinct locations 2018-2021",
     xlab="Number of locations", ylab="Number of chemicals")
par(mfrow=c(2,2))
hist(distinct_locations$Year_2018, main="Distinct locations 2018",
     xlab="Number of locations", ylab="Number of chemicals")
hist(distinct_locations$Year_2019, main="Distinct locations 2019",
     xlab="Number of locations", ylab="Number of chemicals")
hist(distinct_locations$Year_2020, main="Distinct locations 2020",
     xlab="Number of locations", ylab="Number of chemicals")
hist(distinct_locations$Year_2021, main="Distinct locations 2021",
     xlab="Number of locations", ylab="Number of chemicals")

# Separate Distinct_locations with data for all years
distinct_locations_df=as.data.frame(distinct_locations)
distinct_locations_complete=na.omit(distinct_locations)
!is.finite(distinct_locations$AQS_PARAMETER_NAME_FINAL)

#Different sparsity throughout years
plot(distinct_locations_df$AQS_PARAMETER_NAME_FINAL,distinct_locations_df$Distinct_Locations)
sum(is.na(distinct_locations$Year_2018))

# Could consider paring down locations based on measurement frequency and number of total measurements available.
# Sub-monthly? Or sub weekly/biwekly? Check number of chemicals that have this measurement frequency and decide then
# Detected at least 20% of the time


#####
# Filter based on spatial missingness
#####

chem_50p=distinct_locations$AQS_PARAMETER_NAME_FINAL[distinct_locations$Distinct_Locations>=50]
# This leaves 76 chemicals
data2= data %>% filter(AQS_PARAMETER_NAME_FINAL %in% chem_50p)

#Chemical classes - How to classify?
#####
#Sampling frequency - what % of data are every how many days?
######

sfreq <- table(data2$SAMPLING_FREQUENCY_CODE) |>
  as.data.frame() |>
  dplyr::arrange(desc(Freq)) |>
  dplyr::mutate(Percent = Freq / sum(Freq) * 100)
View(sfreq)

# Summarize the frequency of SAMPLING_FREQUENCY_NAME per AQS_PARAMETER_CODE_FINAL
frequency_table <- data2 |>
  group_by(AQS_PARAMETER_NAME_FINAL, SAMPLING_FREQUENCY_CODE) |>
  summarise(Frequency = n()) |>
  group_by(AQS_PARAMETER_NAME_FINAL) |>
  mutate(Percent = Frequency / sum(Frequency) * 100) |>
  arrange(AQS_PARAMETER_NAME_FINAL, desc(Frequency))

# Calculate the most frequent chemical per sampling frequency
most_frequent <- data2 %>%
  group_by(AQS_PARAMETER_NAME_FINAL, SAMPLING_FREQUENCY_CODE) %>%
  summarise(Frequency = n()) %>%
  arrange(AQS_PARAMETER_NAME_FINAL, desc(Frequency)) %>%
  slice(1) %>%
  group_by(SAMPLING_FREQUENCY_CODE) %>%
  summarise(Most_Frequent_Frequency = n()) %>%
  arrange(desc(Most_Frequent_Frequency))
######
#Does frequency change have to do with change in sample kind?
# Redo previous charts for sampling type
######

#Sampling kind - what % of data are have each sampling type?
skind <- table(data2$AQS_METHOD_CODE) |>
  as.data.frame() |>
  dplyr::arrange(desc(Freq)) |>
  dplyr::mutate(Percent = Freq / sum(Freq) * 100)
View(skind)

skind <- table(data2$DURATION_DESC) |>
  as.data.frame() |>
  dplyr::arrange(desc(Freq)) |>
  dplyr::mutate(Percent = Freq / sum(Freq) * 100)
View(skind)



# Summarize the frequency of SAMPLE_COLLECTION_DESC per AQS_PARAMETER_CODE_FINAL
sample_table <- data2 |>
  group_by(AQS_PARAMETER_NAME_FINAL, SAMPLE_COLLECTION_DESC) |>
  summarise(Frequency = n()) |>
  group_by(AQS_PARAMETER_NAME_FINAL) |>
  mutate(Percent = Frequency / sum(Frequency) * 100) |>
  arrange(AQS_PARAMETER_NAME_FINAL, desc(Frequency))

# Calculate the most frequent chemical per sampling kind
most_freq_sample <- data2 %>%
  group_by(AQS_PARAMETER_NAME_FINAL, SAMPLE_COLLECTION_DESC) %>%
  summarise(Frequency = n()) %>%
  arrange(AQS_PARAMETER_NAME_FINAL, desc(Frequency)) %>%
  slice(1) %>%
  group_by(SAMPLE_COLLECTION_DESC) %>%
  summarise(Most_Frequent_Frequency = n()) %>%
  arrange(desc(Most_Frequent_Frequency))


#####
#Frequency of non-detects?
#####
unique_days=length(unique(data2$time))
distinct_dates <- data2 %>%
  group_by(AQS_PARAMETER_NAME_FINAL) %>%
  summarise(Unique_Dates = n_distinct(time)) %>%
  mutate(Percent = (Unique_Dates / unique_days) * 100) %>%
  arrange(desc(Unique_Dates))


# What chemicals have the most frequent sampling codes? Is it separated by chemical type?

# Does sample frequency represent a cumulative measurement?
# Include sample duration back? Read tech report to figure this out
