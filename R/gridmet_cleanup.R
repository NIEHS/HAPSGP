# Clean up some columns in gridmet/ interim function
gridmet_cleanup = function(gmet_process, vars) {
  gmet_process$time = as.POSIXct(gmet_process$time)
  vars1 = c(vars[2:length(vars)], "lon", "lat")
  gmet_cleanup = gmet_process[, !names(gmet_process) %in% vars1, with = FALSE]
  return(gmet_cleanup)
}
