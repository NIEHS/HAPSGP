# Process and calculate covariatess
#pak::pak("NIEHS/amadeus@1.1.4")

library(amadeus)
library(dplyr)

setwd("/ddn/gs1/home/kassienma/HAPSGP/")

#################
#### HAPs sites data
source("R/process_haps.R")

# Produce locations file for covariate calculation
haps_locs=process_haps(path = "output/AGU/",
  date = c("2018-01-01", "2021-12-31"),
  sites_file="input/AGU/AMA_SITE_INFORMATION.Rda",
  mode = "location",
  #data_field = vars,
  return_format = "data.table")

# Load full haps file (produced via AMA_analysis_agu.R )
load("output/AGU/haps_df.Rda")

#################
#### GRIDMET ####
#
#years_gridmet=c(2018,2021)
directory_to_save_gridmet="input/covariates/gridmet/"

#Process
dates_gridmet=c("2018-01-01", "2021-12-31")

variables_gridmet= c("vs","th","sph","pr","tmmn","tmmx","srad")
radiuses=c(0, 10000)
#variable_gridmet= c("vs")

#Initialize haps dataframe
dates_haps=seq(as.Date(dates_gridmet[1]), as.Date(dates_gridmet[2]), "days")
df_full=haps_df %>% filter(time %in% dates_haps)

for (i in 1:length(variables_gridmet)){
  for (j in 1:length(radiuses)){
    print(paste(variables_gridmet[i], "radius=", radiuses[j]))
    proc=process_gridmet(
      date = dates_gridmet,
      variable = variables_gridmet[i],
      path = paste0(directory_to_save_gridmet,variables_gridmet[i]),
      extent = NULL
    )

    #Calculate
    calc=calculate_gridmet(
      from=proc,
      locs=haps_locs,
      locs_id = "AMA_SITE_CODE",
      radius = radiuses[j],
      fun = "mean",
      geom = FALSE
    )

    calc$time=as.character(calc$time)

    if(i==1&j==1){
    merged_data <- merge(df_full, calc, by = c("AMA_SITE_CODE", "time"), all.x = TRUE)
    } else{
      merged_data <- merge(merged_data, calc, by = c("AMA_SITE_CODE", "time"), all.x = TRUE)
    }
  } # j radiuses
} # i covariates

fname="output/AGU/haps_gridmet_208-2021.rds"
saveRDS(merged_data,fname)

#################
#### PRISM 
#times=format(seq(as.Date("2018-01-01"), as.Date("2021-12-31"), "days"),"%Y%m%d")
#times=format(seq(as.Date("2018-01-01"), as.Date("2018-01-02"), "days"),"%Y%m%d")
#directory_to_save_prism="input/covariates/prism/"
#data_type_prism="ts"
#format_prism="nc"
#prism_elements=c("ppt","tmean","tdmean")

#################
#### NARR
#variables_narr= c("uwnd.10m", "vwnd.10m")

#years_narr=c(2018,2021)
#directory_to_save_narr="input/covariates/narr/"
