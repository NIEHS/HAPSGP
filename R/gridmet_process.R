# Process and calculate gridmet for HAPs targets pipeline
# nolint start
gridmet_process=function(
#fname="output/AGU/haps_gridmet_2018-2021.rds"
data,
#fname="output/AGU/gridmet_process_2018-2021.rds",
dates_gridmet,
input_dir="input/covariates/gridmet/",
variables_gridmet= c("vs","th","sph","pr","tmmn","tmmx","srad"),
radiuses=c(0, 10000),
output,
haps_locs){

  
#Initialize haps dataframe
if(output=="merged"){
#load("output/AGU/haps_df.Rda") # Load full haps file (produced via AMA_analysis_agu.R )
#df_full=haps_df %>% filter(time %in% dates_gridmet)
df_full=data %>% filter(time %in% dates_gridmet)
}

for (i in 1:length(variables_gridmet)){
  for (j in 1:length(radiuses)){
    print(paste(variables_gridmet[i], "radius=", radiuses[j]))
    proc=process_gridmet(
      date = c(dates_gridmet[1],dates_gridmet[length(dates_gridmet)]),
      variable = variables_gridmet[i],
      path = paste0(input_dir,variables_gridmet[i]),
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

    if(output=="merged"){
      if(i==1&j==1){
      merged_data = merge(df_full, calc, by = c("AMA_SITE_CODE", "time"), all.x = TRUE)
      } else{
        merged_data = merge(merged_data, calc, by = c("AMA_SITE_CODE", "time"), all.x = TRUE)
      }
    }else if (output=="covonly"){
      if(i==1&j==1){
     merged_data = calc
      }else{
        merged_data = merge(merged_data, calc, by = c("AMA_SITE_CODE", "time"), all.x = TRUE)
      }
    }else{
       stop("specify kind of output (merged or covonly)")
    }

  } # j radiuses
} # i covariates

#saveRDS(merged_data,fname)
return(merged_data)
}
# nolint end