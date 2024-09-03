################################################################################
## PURPOSE: AMA_file.R compiles all the annual files, brings in additional 
##          fields, and creates a final excel spreadsheet
## INPUT:   data.dir - directory for all AMA files (type=character)
##          results.dir - directory for all results (type=character)
##          amayr - version year for the AMA (type=number;YYYY)
##          allyears - years of AMA data (type=vector of numbers;YYYY:YYYY)
## CREATES: AMA*amayr*_annual.Rda
##          AMA*amayr*_annual.xlsx
## PART 1:  Read in ancillary data 
## PART 2:  Combine all the annual averages together
## PART 3:  Add the rest of the fields
## PART 4:  Add the other tabs of the final excel spreadsheet
## ACRONYMS:AMA = Ambient Monitoring Archive
##          AQS = Air Quality System
##          NATTS = National Air Toxics Trends Sites
##          NEI = National Emissions Inventory
##          POC = Parameter Occurrence Code
##          ROS = Regression on Order Statistics
##          STD = standard conditions
################################################################################
AMA_file <- function(data.dir,results.dir,amayr,allyears){
  
  ##### PART 1: READ IN ANCILLARY DATA #####
  
  # warning: excel file must be closed for 'read.xlsx' to work
  
  # read tabs in look up tables readme file
  readme = read.xlsx(paste0('../AMA',amayr,'_lookups.xlsx'), sheet = 'readme') # readme 
  AMA_analysis = read.xlsx(paste0('../AMA',amayr,'_lookups.xlsx'), sheet = 'AMA_analysis') # code
  AMA_LC2STDRatio = read.xlsx(paste0('../AMA',amayr,'_lookups.xlsx'), sheet = 'AMA_LC2STDRatio') # code
  AMA_preprocessing = read.xlsx(paste0('../AMA',amayr,'_lookups.xlsx'), sheet = 'AMA_preprocessing') # code
  AMA_duration2daily = read.xlsx(paste0('../AMA',amayr,'_lookups.xlsx'), sheet = 'AMA_duration2daily') # code
  AMA_daily2annual = read.xlsx(paste0('../AMA',amayr,'_lookups.xlsx'), sheet = 'AMA_daily2annual') # code
  AMA_file = read.xlsx(paste0('../AMA',amayr,'_lookups.xlsx'), sheet = 'AMA_file') # code
  Durations = read.xlsx(paste0('../AMA',amayr,'_lookups.xlsx'), sheet = 'Durations') # sampling durations and minimum sampling counts
  Acronyms = read.xlsx(paste0('../AMA',amayr,'_lookups.xlsx'), sheet = 'Acronyms') # information about all acronyms used
  Descriptions = read.xlsx(paste0('../AMA',amayr,'_lookups.xlsx'), sheet = 'Descriptions') # final field descriptions
  
  # read in information about NATTS and format columns
  NATTS = data.table(read.xlsx(paste0('../AMA',amayr,'_lookups.xlsx'), sheet = 'NATTS')) %>%
    mutate(AMA_SITE_CODE = gsub('-', '', AQS.ID),
           NATTS_CITY = str_sub(Location, end=-4),
           NATTS_STATE = str_sub(Location, start=-2),
           NATTS_LOCATION = paste0(NATTS_CITY, ', ', NATTS_STATE)) %>%
    setnames(old='Setting',new='NATTS_SETTING')
  
  # read in information about AQS CAS numbers and format columns
  AQS_CAS = data.table(read.xlsx(paste0('../AMA',amayr,'_lookups.xlsx'), sheet = 'AQS_CAS')) %>%
    mutate(CAS_nodash = gsub('-','',CAS.Number)) %>%
    setnames(old='Parameter.Code',new='AQS_PARAMETER_CODE')
  
  # read in information about NEI and format columns
  NEI = data.table(read.xlsx(paste0('../AMA',amayr,'_lookups.xlsx'), sheet = 'NEI')) %>%
    setnames(old=c('Pollutant.Code','Pollutant.Description'),new=c('NEI_POLLUTANT_CODE','NEI_POLLUTANT_NAME'))
  
  # read in information about AirToxScreen and format columns
  AirToxScreen = data.table(read.xlsx(paste0('../AMA',amayr,'_lookups.xlsx'), sheet = 'AirToxScreen')) %>%
    setnames(old=c('NEI.Pollutant.Description','AirToxScreen.WEBSITE.POLL.NAME.(or.comment)',
                   'CMAQ.Y/N','AERMOD.Y/N','CMAQ.PAH.Group','Pollutant.Code.(CAS)'),
             new=c('NEI_POLLUTANT_NAME','AIRTOXSCREEN_POLLUTANT_NAME','CMAQ_HAP','AERMOD_HAP','PAH_GROUP','CAS_nodash'))
  
  # combine AQS, AirToxScreen, and NEI information
  AQSAirToxScreen = join(AQS_CAS %>% select(AQS_PARAMETER_CODE,CAS_nodash),
                         AirToxScreen %>% select(CAS_nodash,NEI_POLLUTANT_NAME,AIRTOXSCREEN_POLLUTANT_NAME,
                                                 CMAQ_HAP,AERMOD_HAP,PAH_GROUP)) %>%
    join(NEI %>% select(NEI_POLLUTANT_NAME,NEI_POLLUTANT_CODE))
  
  ##### PART 2: COMBINE ALL THE ANNUAL AVERAGES TOGETHER #####
  
  # looping through each year
  annual_ALL = list()
  for(i in 1:length(allyears)){
    
    # load year of data
    print(allyears[i])
    load(paste0(results.dir,'AMA',amayr,'_annual_',allyears[i],'.Rda'))
    annual_ALL[[i]] = annual # adding to the list
    
  } 
  annual_ALL = do.call('rbind', annual_ALL) # unlisting all annual averages into one data table
  
  ##### PART 3: ADD THE REST OF THE FIELDS #####
  
  # load site information
  load(paste0(data.dir,'AMA_SITE_INFORMATION.Rda'))
  
  # join annual_ALL with site information
  annual_ALL = join(annual_ALL,data.table(AMA_SITE_INFORMATION) %>%
                      select(AMA_SITE_CODE,AMA_SITE_NAME,COUNTY_NAME,
                             MONITOR_LATITUDE,MONITOR_LONGITUDE,CITY,
                             LOCATION_TYPE,LAND_USE,CBSA_NAME,
                             CENSUS_TRACT_ID_2010,CENSUS_TRACT_POPULATION_2010,
                             CENSUS_TRACT_ID_2020,CENSUS_TRACT_POPULATION_2020)) %>%
    join(NATTS %>% select(AMA_SITE_CODE,NATTS_LOCATION,NATTS_SETTING)) %>%
    join(AQSAirToxScreen %>% select(AQS_PARAMETER_CODE,NEI_POLLUTANT_CODE,
                                    NEI_POLLUTANT_NAME,AIRTOXSCREEN_POLLUTANT_NAME,
                                    CMAQ_HAP,AERMOD_HAP,PAH_GROUP))
  
  # save file
  save(annual_ALL,file=paste0(results.dir,'AMA',amayr,'_annual.Rda'))
  
  ##### PART 4: ADD THE OTHER TABS OF THE FINAL EXCEL SPREADSHEET #####
  
  # save excel file
  write.xlsx(list(readme = readme, Annual_Statistics = annual_ALL, Descriptions = Descriptions, 
                  AQS_CAS = AQS_CAS, NATTS = NATTS, NEI = NEI, AirToxScreen = AirToxScreen, 
                  Acronyms = Acronyms, Durations = Durations, 
                  AMA_analysis = AMA_analysis, 
                  AMA_LC2STDRatio = AMA_LC2STDRatio,
                  AMA_preprocessing = AMA_preprocessing, 
                  AMA_duration2daily = AMA_duration2daily,
                  AMA_daily2annual = AMA_daily2annual,
                  AMA_file = AMA_file), 
             paste0('../AMA',amayr,'_annual.xlsx'))
  
}