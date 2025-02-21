select_states=function(locs, state_list){
states_sel <- tigris::states(cb = TRUE, resolution = "20m", class = "sf") %>%
  filter(NAME %in% state_list)

locs_proj <- st_transform(locs, st_crs(states_sel))
locs_states <- st_intersection(locs_proj, st_geometry(states_sel))

return(locs_states)
}