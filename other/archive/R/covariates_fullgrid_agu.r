# Create prediction grids and process covariates

library(ncdf4)
library(tidyverse)

setwd("/ddn/gs1/home/kassienma/HAPSGP/")

years_gridmet=(2018:2021)

directory_to_save_gridmet="input/covariates/gridmet/"
variables_gridmet= c("vs","th","sph","pr","tmmn","tmmx","srad")

for (i in 1:length(variables_gridmet)){
    for(y in 1:length(years_gridmet)){
    
    year_gridmet=years_gridmet[y]

    fname=paste0(directory_to_save_gridmet,variables_gridmet[i],"/",variables_gridmet[i],"_",year_gridmet,".nc")

    print(paste0(variables_gridmet[i],"_",year_gridmet,".nc"))

    # Open gridmet nc file
    nc_file <- nc_open(fname)

    # Extract variables
    lat <- ncvar_get(nc_file, "lat")       # Replace "lat" with the actual variable name for latitude
    lon <- ncvar_get(nc_file, "lon")       # Replace "lon" with the actual variable name for longitude

    if(i!=1){
    if(sum(lat==lat2)!=length(lat)){
        stop("lat coordinates difer between this and previous variable")
    }
    if(sum(lon==lon2)!=length(lon)){
        stop("lon coordinates difer between this and previous variable")
    }
    }

    time <- ncvar_get(nc_file, "day")     # Replace "time" with the actual variable name for time
    data <- ncvar_get(nc_file, names(nc_file$var))     # Replace "data" with the actual variable name for the dataset

    # Convert time if needed (e.g., from seconds since some date)
    time_units <- ncatt_get(nc_file, "day", "units")$value
    time_origin <- sub(".*since ", "", time_units)
    time_converted <- as.Date(time, origin = time_origin)

    # Combine data into a data.frame
    # Assuming `data` is structured as [lon, lat, time]
    df <- expand.grid(lon = lon, lat = lat, time = time_converted) %>%
    mutate(data = as.vector(data))

    colnames(df)[4]=variables_gridmet[i]

    if(y==1){
    year_df=df
    }else{
    year_df=rbind(year_df,df)
    }

    # Close the NetCDF file
    nc_close(nc_file)
    }#y

if(i==1){
 merged_df = year_df
  }else{
   # merged_df = merge(merged_df, df, by = c("lon", "lat", "time"), all.x = TRUE)
   merged_df=cbind(merged_df,year_df[4])
    }

lat2=lat
lon2=lon

} #i

saveRDS(merged_df,"input/covariates/gridmet/gridmet_all_df.RDS")

# Check: Plot the data
#library(dplyr)
#library(ggplot2)
#df_date=merged_df %>% filter(time== as.Date("2019-07-31"))
#df_date2=merged_df %>% filter(time== as.Date("2020-07-31"))

#ggplot(df_date, aes(x = lon, y = lat, fill = vs)) +
#  geom_tile() +
#  scale_fill_viridis_c(option = "viridis") +  # Viridis color scale for better visualization
#  labs(x = "Longitude",
#       y = "Latitude",
#       fill = "Value") +
#  theme_minimal()