#nolint starty
# download_edgar function

species=c("BC", "CO", "NH3", "NMVOC", "NOx", "OC", "PM10", "PM2.5", "SO2")
version=c("8.1")
temp_res=c("yearly", "monthly", "timeseries")
sector_yearly=c("AGS", "AWB", "CHE", "ENE", "IND", "MNM", "NMM", "PRU_SOL", "RCO", 
"REF_TRF", "SWD_INC", "SWD_LDF", "TNR_Aviation_CDS", "TNR_Aviation_CRS", 
"TNR_Aviation_LTO", "TNR_Aviation_SPS", "TNR_Other", "TNR_Ship", "TRO", "WWT")
sector_monthly=c("AGRICULTURE", "BUILDINGS", "FUEL_EXPLOITATION", "IND_COMBUSTION", "IND_PROCESSES", "POWER_INDUSTRY", "TRANSPORT", "WASTE")
format="nc"
output="emi"
year_range=c(2021,2022)
voc=seq(1:25)


# Initial if statements for download strings
 yearsvec <- seq(year_range[1], year_range[2])

# Error message for output=flux and format=txt. Could consider changing to a warning and downloading the txt file instead.
if (any(output == "flx" & format == "txt")) {
    stop("Output 'flux' is only supported for format 'nc'. Please check function documentation for acceptable inputs.")
}
# Error message for monthly resolution and txt format
if (any(temp_res == "monthly" & format == "txt")) {
    stop("Monthly resolution is only supported for format 'nc'. Please check function documentation for acceptable inputs.")
}

# Check if species is in the list of supported species, accepting lower or uppercase and supporting pm2.5 or pm25. If valid, transform all species to upper case.
if (any(species %in% c("bc", "BC", "co", "CO", "nh3", "NH3", "nox", "NOx", "oc", "OC", "pm10", "PM10", "pm2.5", "PM2.5", "pm25", "PM25", "so2", "SO2"))) {
    species <- toupper(species)
} else {
    stop("Input for 'species' is not supported. Please check function documentation for acceptable inputs.")
}

# Prep pm2.5 string for both pm25 and pm2.5
if (any(species %in% c("PM2.5", "PM25"))) {
    species_folder <- gsub("(?i)pm2\\.5|pm25", "PM2.5", species, perl = TRUE)
    species_file <- gsub("(?i)pm2\\.5|pm25", "PM25", species, perl = TRUE)
} else {
    species_folder <- species
    species_file <- species
}

# Fix NOx string
if ("NOX" %in% species){
  species_folder <- gsub("NOX", "NOx", species_folder, perl = TRUE)
  species_file <- gsub("NOX", "NOx", species_file, perl = TRUE)
}

#Main url
durl="https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/"

#Version
if (version=="8.1"){
    vers="v81_FT2022_AP_new/"
    vers_file="v8.1_FT2022_AP"

#Time series vs yearly vs monthly
if if (temp_res=="timeseries"){
    download_string <- paste0(durl,vers,"EDGAR_", species_file, "_1970_2022.zip")
    # Add patch for SO2 so its download string will end in 2022_v2.zip
    if ("SO2 %in% species_file"){
      download_string <- gsub("EDGAR_SO2_1970_2022.zip", "EDGAR_SO2_1970_2022_v2.zip", download_string)
    }
} else if (temp_res=="yearly"){
    if(sector_yearly){
      if(year_range){
        download_string <- paste0(durl,vers,species_folder,"/",sector_yearly,"/",output,"_",format,"/",vers_file,"_",species,"_",yearsvec,"_",sector_yearly,"_",output,"_",format,".zip")
      } else {
      download_string <- paste0(durl,vers,species_folder,"/",sector_yearly,"/",sector_yearly,"_",output,"_",format,".zip")
      }
    } else {
       if(year_range){
        download_string <- paste0(durl,vers,species_folder,"/TOTALS/",output,"_",format,"/",vers_file,"_",species,"_",yearsvec,"_TOTALS_",output,"_",format,".zip")
      } else{
      download_string <- paste0(durl,vers,species_folder,"/TOTALS/TOTALS_",output,"_",format,".zip")
      }
    }
} else if (temp_res=="monthly"){
if(!is.null(sector_monthly)){
download_string <- paste0(durl,vers,"monthly/",species_folder,"/bkl_",sector_monthly,"/bkl_",sector_monthly,"_",output,"_",format,".zip")

}else {
  download_string <- paste0(durl,vers,"monthly/EDGAR_", species_file,"_m_2000_2022.zip")
   # Add patch for SO2 so its download string will end in 2022_v2.zip
    if ("SO2 %in% species_file"){
      download_string <- gsub("EDGAR_SO2_m_2000_2022.zip", "EDGAR_SO2_m_2000_2022_v2.zip", download_string)
    }
}

   
} else {
    stop("Input for 'temp_res' is not supported. Please check function documentation for acceptable inputs.")
}

} else if (version=="8.1_voc"){
    vers="v81_FT2022_VOC_spec/"
    vers_file="v8.1_FT2022_VOC_spec"
    if(!is.null(sector_monthly)){
      if(!is.null(year_range)){
        download_string <- paste0(durl,vers,"voc",voc,"/bkl_",sector_monthly,"/",output,"_",format,"/",vers_file,"_voc",voc,"_",yearsvec,"_bkl_",sector_monthly,"_",output,"_",format,".zip")
      }else{
      download_string <- paste0(durl,vers,"voc",voc,"/bkl_",sector_monthly,"/bkl_",sector_monthly,"_",output,"_",format,".zip")
      }
    }else{
      download_string <- paste0(durl,vers,"EDGAR_voc",voc,"_1970_2022.zip")
    }
} else {
    stop("Input for 'version' is not supported. Please check function documentation for acceptable inputs.")
}

# After download string is created, check which URLs exist and download them. Output a message with any requested URLs that did not exist. 

#nolint end 