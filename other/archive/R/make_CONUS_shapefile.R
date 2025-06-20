# Make USA shapefile

library(USAboundaries)
library(sf)

# Fetch state boundaries
us_states <- us_states(resolution = "low")  # Use "low" or "high" resolution

# Filter for the conterminous US (48 states + DC)
conus_states <- us_states[!us_states$name %in% c("Alaska", "Hawaii", "Puerto Rico"), ]

# Combine the states into a single boundary
conus_boundary <- st_union(conus_states)

# Save the boundary to a shapefile
st_write(conus_boundary, "/ddn/gs1/home/kassienma/HAPSGP/input/conterminous_us_boundary.shp")

# Optional: Plot the boundary to visualize
plot(st_geometry(conus_boundary))
