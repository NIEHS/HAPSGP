# Prediction grid for Texas
#state_list=("Texas")
#grid_size <- 10000  # 10 km in meters

make_grid_state=function(state_list,grid_size){
states_sel <- tigris::states(cb = TRUE, resolution = "20m", class = "sf") %>%
  filter(NAME %in% state_list)

states_sel <- st_transform(states_sel, 5070)  
# Define grid size (10 km ~ 0.1 degrees, but precise conversion varies by latitude)


# Create the grid
grid <- st_make_grid(states_sel, cellsize = grid_size, square = TRUE, what = "polygons")
grid_sf <- st_sf(geometry = grid)

# Crop grid to Texas boundary
grid_state <- grid_sf[st_intersects(grid_sf, states_sel, sparse = FALSE), ]

# Generate site IDs
grid_state$AMA_SITE_CODE <- seq_len(nrow(grid_state))

# Compute centroids
centroids <- st_centroid(grid_state)

# Convert to WGS84 for lat/lon extraction
centroids_wgs84 <- st_transform(centroids, 4326)

# Extract lon/lat
coords <- st_coordinates(centroids_wgs84)

# Add lon/lat as new columns
grid_state$lon <- coords[, 1]  # Longitude
grid_state$lat <- coords[, 2]  # Latitude

# Convert to data frame
#grid_df <- data.frame(
#  grid_id = grid_state$grid_id,
#  lon = coords[, 1],  # Longitude
#  lat = coords[, 2]   # Latitude
#)

return(grid_state)
}