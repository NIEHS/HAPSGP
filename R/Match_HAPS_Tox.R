# Match_HAPS_Tox.R
# Author: Mariana Kassien
# Match HAPS data to Tox21 database based on CASRN number
# Part of exploratory funcitons; will probably incorporate into a bigger processing function later on. 

library(openxlsx)
library(dplyr)

# Read HAPS data and CAS data
data <- readRDS("input/HAPS_data_exploratory.rds") 
load("input/AMA_POLLUTANT_CODES_DICTIONARY.Rda") #pollutant codes metadata

# Append CAS to sites
# Perform the join
data_cas <- data|> 
dplyr::mutate(AQS_PARAMETER_CODE = AQS_PARAMETER_CODE_FINAL) 

data_cas=dplyr::left_join(data_cas, AMA_POLLUTANT_CODES_DICTIONARY |> dplyr::select(AQS_PARAMETER_CODE, POLLUTANT_CASNUM), by = "AQS_PARAMETER_CODE")

# Download Tox21 reference data
url="https://ice.ntp.niehs.nih.gov/downloads/casreferencelists/Tox21.xlsx"
path="input/Tox21.xlsx"
download.file(url, path)

tox21=read.xlsx(path)
tox21=tox21 %>%
    mutate(CAS_nodash = gsub('-','',CASRN))

tox_cas=unique(tox21$CAS_nodash)

data_tox=data_cas %>% filter(as.character(POLLUTANT_CASNUM) %in% tox_cas)
length(unique(data_cas$AQS_PARAMETER_CODE)) # 132
length(unique(data_tox$AQS_PARAMETER_CODE)) # 72

# List of chemicals plus unique spatial locations
  distinct_locations <- data_tox %>%
  group_by(AQS_PARAMETER_NAME_FINAL) %>%
  summarise(Distinct_Locations = n_distinct(paste(lat, lon))) %>%
  arrange(desc(Distinct_Locations))

# There are 72 unique HAPs that match the Tox21 database,
# Of these, 38 HAPs have data for more than 50 locations.
# 54 HAPs have data for more than 40 locations.