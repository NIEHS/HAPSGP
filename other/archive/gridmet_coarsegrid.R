# Predict on gridmet coarse grid
# Load required libraries
library(sf)          # For handling spatial data
library(dplyr)       # For data manipulation
library(amadeus)

setwd("/ddn/gs1/home/kassienma/HAPSGP/")

#################
#### MAKE COARSE GRID IN THE CONUS ####

# Load the shapefile of the US
# Replace 'path_to_shapefile' with the actual path to your shapefile
us_shapefile <- st_read("/ddn/gs1/home/kassienma/HAPSGP/input/conterminous_us_boundary.shp")

# Get the bounding box of the shapefile
bbox <- st_bbox(us_shapefile)

# Create a grid using the bounding box
# Define grid resolution (e.g., 200 x 100 points)
n_lat <- 200
n_lon <- 100

lat_seq <- seq(bbox["ymin"], bbox["ymax"], length.out = n_lat)
lon_seq <- seq(bbox["xmin"], bbox["xmax"], length.out = n_lon)

# Create a grid of points
grid_points <- expand.grid(lon = lon_seq, lat = lat_seq)

# Convert grid to an sf object
grid_sf <- st_as_sf(grid_points, coords = c("lon", "lat"), crs = 4326)

# Filter points within the shapefile
grid_within <- st_intersection(grid_sf, us_shapefile)

# Convert back to a dataframe
grid_df <- as.data.frame(st_coordinates(grid_within))
colnames(grid_df)=c("lon","lat")
grid_df$idx=1:nrow(grid_df)

#################
#### CALCULATE GRIDMET ####
#
#years_gridmet=c(2018,2021)
directory_to_save_gridmet="input/covariates/gridmet/"

#Process
dates=c("2021-01-01", "2021-12-31")
dates_vec=seq(as.Date(dates[1]), as.Date(dates[2]), "days")

# Use lapply to create a list of dataframes, then bind them together
grid_df_st <- do.call(rbind, lapply(dates_vec, function(date) {
  grid_df$time <- date  # Add the date column
  return(grid_df)       # Return the modified dataframe
}))

variables_gridmet= c("vs","th","sph","pr","tmmn","tmmx","srad")
radiuses=c(0)#, 10000)
#variable_gridmet= c("vs")

for (i in 1:length(variables_gridmet)){
  for (j in 1:length(radiuses)){
    print(paste(variables_gridmet[i], "radius=", radiuses[j]))
    proc=process_gridmet(
      date = dates,
      variable = variables_gridmet[i],
      path = paste0(directory_to_save_gridmet,variables_gridmet[i]),
      extent = NULL
    )

    #Calculate
    calc=calculate_gridmet(
      from=proc,
      locs=grid_df,
      locs_id = "idx",
      radius = radiuses[j],
      fun = "mean",
      geom = FALSE
    )
    calc$time=as.character(calc$time)
    
    if(i==1&j==1){
      merged_data = merge(grid_df_st, calc, by = c("idx", "time"), all.x = TRUE)
      } else{
        merged_data = merge(merged_data, calc, by = c("idx", "time"), all.x = TRUE)
      }

  } # j radiuses
} # i covariates

fname="output/AGU/gridmet_coarsegrid_2021.rds"
saveRDS(merged_data,fname)