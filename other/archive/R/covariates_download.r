# covariates_download.R
# Downloading covariates in AMADEUS

library(amadeus)

#################
#### PRISM 
times=format(seq(as.Date("2018-01-01"), as.Date("2021-12-31"), "days"),"%Y%m%d")
#times=format(seq(as.Date("2018-01-01"), as.Date("2018-01-02"), "days"),"%Y%m%d")
directory_to_save_prism="input/covariates/prism/"
data_type_prism="ts"
format_prism="nc"
prism_elements=c("ppt","tmean","tdmean")

# Using mapply to call the function for each combination of time and element
mapply(function(time, element) {
  download_prism(
    time = time,
    element = element,
    data_type = data_type_prism,
    format = format_prism,
    directory_to_save = directory_to_save_prism,
    acknowledgement = TRUE,
    download = TRUE,
    remove_command = TRUE,
    hash = FALSE
  )
}, time = times, element = prism_elements)

#################
#### GRIDMET ####

# Download
variables_gridmet= c("vs","th","sph","pr","tmmn","tmmx","srad")
years_gridmet=c(2018,2021)
directory_to_save_gridmet="input/covariates/gridmet/"

download_gridmet(
  variables = variables_gridmet,
  year = years_gridmet,
  directory_to_save = directory_to_save_gridmet,
  acknowledgement = TRUE,
  download = TRUE,
  remove_command = TRUE,
  hash = FALSE
)


#################
#### NARR
variables_narr= c("uwnd.10m", "vwnd.10m")

years_narr=c(2018,2021)
directory_to_save_narr="input/covariates/narr/"

download_narr(
  variables = variables_narr,
  year = years_narr,
  directory_to_save = directory_to_save_narr,
  acknowledgement = TRUE,
  download = TRUE,
  remove_command = TRUE,
  hash = FALSE
)
