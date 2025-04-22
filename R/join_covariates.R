# join gridmet and tri covariates
# nolint start
#gmet_process=tar_read("gmet_process")

#dt_feat_calc_tri=tar_read("dt_feat_calc_tri")
#dt_feat_calc_design=qs2::qs_read("/ddn/gs1/group/set/Projects/beethoven_targets/_targets/objects/dt_feat_calc_design")

#dt_feat_calc_nlcd=tar_read("dt_feat_calc_nlcd")

#dt_feat_calc_xyt=qs2::qs_read("/ddn/gs1/group/set/Projects/beethoven_targets/_targets/objects/dt_feat_calc_xyt")
#library(data.table)
join_covariates=function(gmet_process,dt_feat_calc_tri,dt_feat_calc_nlcd){


# Extract year from the daily time column
gmet_process[, year := year(time)]  # Extract year from Date

# Ensure the time column in dt_feat_calc_tri represents only years
dt_feat_calc_tri[, year := as.integer(time)]  # Convert to integer if necessary

dt_feat_calc_nlcd=as.data.table(dt_feat_calc_nlcd)
dt_feat_calc_nlcd[, year := as.integer(time)]

# Perform the join on "AMA_SITE_CODE", "lon", "lat", and "year"
merged_dt <- merge(
  dt_feat_calc_tri, dt_feat_calc_nlcd,
  by = c("AMA_SITE_CODE", "year"), 
  all.x = TRUE  # Keep all rows from gmet_process
)

merged_dt2 <- merge(
  gmet_process, merged_dt,
  by = c("AMA_SITE_CODE", "year"), 
  all.x = TRUE  # Keep all rows from gmet_process
)

# Drop time.y and tri_year columns if they exist
merged_dt2[, c("year","time.x","time.y", "tri_year","lon.y","lat.y") := NULL]
# Rename longitude
if ("lon.x" %in% names(merged_dt)){
setnames(merged_dt2, "lon.x", "lon")
setnames(merged_dt2, "lat.x", "lat")
}
return(merged_dt2)
}
#nolint end