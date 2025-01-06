################################################################################
# nolint start
## PURPOSE: AMA_duration2daily.R calculates daily averages per
##          site/day/pollutant/sampling duration
## INPUT:   results.dir - directory for all results (type=character)
##          amayr - version year for the AMA (type=number;YYYY)
##          allyears - years of AMA data (type=vector of numbers;YYYY:YYYY)
## CREATES: AMA*amayr*_daily_YYYY.Rda
## PART 1:  Initialize global variables
## PART 2:  Compute daily averages from minute data
## PART 3:  Compute daily averages from hourly data
## PART 4:  Combine minute and hourly averages and average across POCs
## PART 5:  Calculate daily averages for remote data
## ACRONYMS:AMA = Ambient Monitoring Archive
##          AQS = Air Quality System
##          MIT = Massachusetts Institute of Technology
##          NOAA = National Oceanic Atmospheric Administration
##          POC = Parameter Occurrence Code
##          STD = standard conditions
################################################################################
AMA_duration2daily <- function(filelist,data.dir,results.dir,amayr,allyears){
  
  ##### PART 1: INITIALIZE GLOBAL VARIABLES #####
  
  frac_remove = 0.50 # when averaging across POCs, the fraction of zeros among averaged concentrations that warrants removal
  mode_STR <- function(x) { ux <- unique(x); ux[which.max(tabulate(match(x, ux)))] } # mode string in vector
  
  # reading in information about temporal scales, sampling durations, and minimum sampling counts
  duration_scales = read.xlsx(paste0(data.dir,'AMA',amayr,'_lookups.xlsx'), sheet = 'Durations') # warning: excel file must be closed
  durcount_hr2d = duration_scales[duration_scales$DURATION_DESC=='1 HOUR','MINIMUM_COUNT'] # min # hrs/day to calc dly avg
  dur_minute = duration_scales[duration_scales$AVERAGE_TO=='HOURLY','DURATION_DESC']
  dur_hour = duration_scales[duration_scales$AVERAGE_TO=='DAILY','DURATION_DESC']
  
  ##### PART 2, PART 3, PART 4, PART 5 #####
  filelist2=list()
  # looping through each year
  for(i in 1:length(filelist)){ 
    
    # load pre-processed AMA data
    print(allyears[i])
    #load(paste0(results.dir,'AMA',amayr,'_preprocessing_',allyears[i],'.Rda'))
    #load(filelist[[i]])
    AMA=filelist[[i]]

    AMA = AMA %>%  mutate(ALTERNATE_MDL = as.numeric(ALTERNATE_MDL)) #Patch for ALT_MDL
    
    AMA_sub = AMA %>%
      subset(!(PROGRAM %like% 'NOAA' | PROGRAM %like% 'MIT')) %>% # subset to non-remote data
      group_by(AQS_PARAMETER_CODE,AMA_SITE_CODE,YEAR,AQS_POC) %>% # group by pollutant/site/poc/year
      mutate(SAMPLING_FREQUENCY_MODE = mode_STR(SAMPLING_FREQUENCY_CODE)) %>% # mode samp freq for each pollutant/site/POC/year
      ungroup()
    
    ##### PART 2: COMPUTE DAILY AVERAGES FROM MINUTE DATA #####
    
    # average the minute sampling durations
    AMA_min2d = AMA_sub %>%
      subset(DURATION_DESC %in% dur_minute) %>% # subset to minute sampling durations
      group_by(SAMPLE_DATE,AQS_PARAMETER_CODE,AMA_SITE_CODE,AQS_POC,QUARTER,YEAR,DURATION_DESC,
               PROGRAM,STATE_ABBR,HOUR,SAMPLING_FREQUENCY_MODE,AQS_PARAMETER_NAME) %>% # group by pollutant/site/poc/day/hour/duration
      summarize(CONC_RATIO_HR=mean(CONC_RATIO,na.rm=T), # mean of derived concentrations using LCSTDratio
                SAMPLE_VALUE_STD_HR=mean(SAMPLE_VALUE_STD), # mean of concentrations in standard conditions
                MDL_STD_UG_M3_HR=mean(MDL_STD_UG_M3,na.rm=T), # mean of MDL in standard conditions
                ALTERNATE_MDL_HR=mean(ALTERNATE_MDL,na.rm=T), # mean of alternate MDL 
                N_HR=length(na.omit(CONC_RATIO)), # number of non-null values
                N0_HR=sum(CONC_RATIO==0,na.rm=T)) %>% # number of zeros
      ungroup() %>%
      join(duration_scales[,c('DURATION_DESC','MINIMUM_COUNT')]) %>% # join min threshold counts
      subset(N_HR>=MINIMUM_COUNT) %>% # subset to hrs that meet the min number of samples/hr
      group_by(SAMPLE_DATE,AQS_PARAMETER_CODE,AMA_SITE_CODE,AQS_POC,QUARTER,YEAR,
               DURATION_DESC,PROGRAM,STATE_ABBR,SAMPLING_FREQUENCY_MODE,AQS_PARAMETER_NAME) %>% # group by pollutant/site/poc/day/duration
      summarize(CONC_RATIO=mean(CONC_RATIO_HR,na.rm=T), # mean of hourly concentrations
                SAMPLE_VALUE_STD=mean(SAMPLE_VALUE_STD_HR,na.rm=T), # mean of hourly concentrations in standard conditions
                MDL_STD_UG_M3=mean(MDL_STD_UG_M3_HR,na.rm=T), # mean of hourly MDL in standard conditions
                ALTERNATE_MDL=mean(ALTERNATE_MDL_HR,na.rm=T), # mean of alternate MDL 
                N_DAY=length(na.omit(CONC_RATIO_HR)), # number of non-null hourly values in a day
                N0_DAY=sum(CONC_RATIO_HR==0,na.rm=T)) %>% # number of hourly zeros values in a day
      ungroup() %>%
      subset(N_DAY>=durcount_hr2d) # subset to days that meet the min number of samples/day
    if(dim(AMA_min2d)[1]>0){AMA_min2d = AMA_min2d %>%
      mutate_all(~case_when(is.nan(.) ~ NA, .default = .)) # add catch to convert NaN to NA
    }
    
    ##### PART 3: COMPUTE DAILY AVERAGES FROM HOURLY DATA #####
    
    # average the hourly sampling durations
    AMA_hr2d = AMA_sub %>%
      subset(DURATION_DESC %in% dur_hour) %>% # subset to hourly sampling durations
      mutate(PULL_DATE = as.numeric(str_sub(as.character(DATA_SOURCE),-8))) %>% # add pull date of data source
      ### CATCH START - deal with '24 HOURS' collocated by pollutant/site/POC/day/sampling duration (multiple pull dates)
      group_by(SAMPLE_DATE,AQS_PARAMETER_CODE,AMA_SITE_CODE,AQS_POC,QUARTER,YEAR,
               DURATION_DESC,PROGRAM,STATE_ABBR,SAMPLING_FREQUENCY_MODE,AQS_PARAMETER_NAME) %>% # group by pollutant/site/poc/day/duration
      mutate(N_DAY=length(na.omit(CONC_RATIO)), # number of non-null values
             N0_DAY=sum(CONC_RATIO==0,na.rm=T), # number of zeros
             ALL_EQUAL=length(na.omit(unique(CONC_RATIO)))==1, # if collocation, all values are equal (T/F)
             MIN_PULLDATE=min(PULL_DATE,na.rm=T), # if collocation, the min pull date of values
             MAX_PULLDATE=max(PULL_DATE,na.rm=T)) %>% # if collocation the max pull date of values
      ungroup() %>%
      subset(!(DURATION_DESC=='24 HOURS' & N_DAY==2 & N0_DAY==1 & CONC_RATIO==0)) %>% # 2 coll vals AND 1 val=0 -> remove the 0
      subset(!(DURATION_DESC=='24 HOURS' & N_DAY==2 & N0_DAY==0 & ALL_EQUAL==FALSE &
                 MIN_PULLDATE!=MAX_PULLDATE & PULL_DATE==MIN_PULLDATE)) %>% # 2 coll AND non-zero AND diff vals AND diff pull dates -> remove earlier val
      select(-c('N_DAY','N0_DAY','ALL_EQUAL','MIN_PULLDATE','MAX_PULLDATE')) %>% # remove all fields created for this catch
      ### CATCH END
      group_by(SAMPLE_DATE,AQS_PARAMETER_CODE,AMA_SITE_CODE,AQS_POC,QUARTER,YEAR,
               DURATION_DESC,PROGRAM,STATE_ABBR,SAMPLING_FREQUENCY_MODE,AQS_PARAMETER_NAME) %>% # group by pollutant/site/poc/day/duration
      summarize(CONC_RATIO_DAY=mean(CONC_RATIO,na.rm=T), # mean of derived concentrations using LCSTDratio
                SAMPLE_VALUE_STD_DAY=mean(SAMPLE_VALUE_STD,na.rm=T), # mean of concentrations in standard conditions
                MDL_STD_UG_M3_DAY=mean(MDL_STD_UG_M3,na.rm=T), # mean of hourly MDL in standard conditions
                ALTERNATE_MDL_DAY=mean(ALTERNATE_MDL,na.rm=T), # mean of alternate MDL 
                N_DAY=length(na.omit(CONC_RATIO)), # number of non-null values
                N0_DAY=sum(CONC_RATIO==0,na.rm=T)) %>% # number of zeros
      ungroup() %>%
      join(duration_scales[,c('DURATION_DESC','MINIMUM_COUNT')]) %>% # join min threshold counts
      subset(N_DAY>=MINIMUM_COUNT) %>% # subset to days that meet the min number of samples/day
      setnames(old=c('CONC_RATIO_DAY','SAMPLE_VALUE_STD_DAY','MDL_STD_UG_M3_DAY','ALTERNATE_MDL_DAY'),
               new=c('CONC_RATIO','SAMPLE_VALUE_STD','MDL_STD_UG_M3','ALTERNATE_MDL')) %>% # rename fields so rbind can be used next
      select(-MINIMUM_COUNT) %>% # remove field so rbind can be used next
      mutate_all(~case_when(is.nan(.) ~ NA, .default = .)) # add catch to convert NaN to NA
    
    # collocations by pollutant/site/poc/day/'24 HOURS'
    # NOTE: Can occur if DURATION_DESC='24 HOURS' but there are multiple start times in day.
    catch_collocation = AMA_hr2d %>% subset(DURATION_DESC=='24 HOURS' & N_DAY > 1)
    if(dim(catch_collocation)[1]!=0){print("Collocation by pollutant/site/POC/day/'24 HOURS'. Stop and fix.")}
    
    ##### PART 4: COMBINE MINUTE AND HOURLY AVERAGES AND AVERAGE ACROSS POCS #####
    
    # count the unique POCs by pollutant/site/year/sampling duration (NOTE: brought into 'AMA_daily2annual.R')
    AMA_POCCOUNT = rbind(AMA_min2d,AMA_hr2d) %>%
      group_by(YEAR,AQS_PARAMETER_CODE,AMA_SITE_CODE,DURATION_DESC) %>%
      summarize(POC_COUNT=length(unique(AQS_POC)))
    
    # combine AMA_min2d and AMA_hr2d and average across pocs
    daily = rbind(AMA_min2d,AMA_hr2d) %>%
      group_by(SAMPLE_DATE,QUARTER,AQS_PARAMETER_CODE,AMA_SITE_CODE,
               STATE_ABBR,YEAR,DURATION_DESC,AQS_PARAMETER_NAME) %>% # group by pollutant/site/day/duration (across pocs)
      mutate(CONC_DAILY=mean(CONC_RATIO,na.rm=T), # mean of values
             N_DAILY=length(na.omit(CONC_RATIO)), # number of non-null values
             N0_DAILY=sum(CONC_RATIO==0,na.rm=T)) %>% # number of zeros
      mutate(FRAC_ZERO = N0_DAILY/N_DAILY) %>% # fraction of zeros by pollutant/site/day/duration
      ungroup() %>%
      subset(!(N_DAILY>1 & CONC_DAILY!=0 & FRAC_ZERO>frac_remove &
                 CONC_RATIO!=0)) %>% # multiple samps/day AND mean(samps)!=0 AND high % of 0s -> remove non-zeros
      subset(!(N_DAILY>1 & CONC_DAILY!=0 & FRAC_ZERO<=frac_remove &
                 CONC_RATIO==0)) %>% # multiple samps/day AND mean(samps)!=0 AND low % of 0s -> remove zeros
      select(-c('CONC_DAILY','N_DAILY','N0_DAILY','FRAC_ZERO')) %>% # remove all fields created
      group_by(SAMPLE_DATE,QUARTER,AQS_PARAMETER_CODE,AMA_SITE_CODE,
               STATE_ABBR,YEAR,DURATION_DESC,AQS_PARAMETER_NAME) %>% # group by pollutant/site/day/duration (across pocs)
      summarize(CONC_DAILY_UG_M3=mean(CONC_RATIO,na.rm=T), # mean of values
                CONC_DAILY_STD=mean(SAMPLE_VALUE_STD,na.rm=T), # mean of values in standard conditions
                MDL_DAILY_STD_UG_M3=mean(MDL_STD_UG_M3,na.rm=T), # mean of MDLs in standard conditions
                ALTERNATE_MDL_DAILY=mean(ALTERNATE_MDL,na.rm=T), # mean of alternate MDL 
                SAMPLING_FREQUENCY_DAILY=mode_STR(SAMPLING_FREQUENCY_MODE), # mode sampling frequency
                POC_COUNT=length(na.omit(CONC_RATIO)), # number of non-null values
                ZERO_COUNT=length(na.omit(CONC_RATIO)==0)) %>% # number of zeros
      mutate(across(c(CONC_DAILY_UG_M3,CONC_DAILY_STD,MDL_DAILY_STD_UG_M3,ALTERNATE_MDL_DAILY),~ifelse(is.nan(.),NA,.))) # add catch to convert NaN to NA
    
    ##### PART 5: CALCULATE DAILY AVERAGES FOR REMOTE DATA #####
    
    # count the unique POCs by pollutant/site/year/sampling duration (NOTE: brought into 'AMA_daily2annual.R')
    AMA_POCCOUNT_REMOTE = AMA %>%
      subset(PROGRAM %like% 'NOAA' | PROGRAM %like% 'MIT') %>% # only look at the data from programs NOAA and MIT
      group_by(YEAR,AQS_PARAMETER_CODE,AMA_SITE_CODE,DURATION_DESC) %>%
      summarize(POC_COUNT=length(unique(AQS_POC)))
    
    daily_REMOTE = AMA %>%
      subset(PROGRAM %like% 'NOAA' | PROGRAM %like% 'MIT') %>% # only look at the data from programs NOAA and MIT
      group_by(AQS_PARAMETER_CODE,AMA_SITE_CODE,YEAR,AQS_POC) %>% # group by pollutant/site/poc/year
      mutate(SAMPLING_FREQUENCY_MODE = mode_STR(SAMPLING_FREQUENCY_CODE)) %>% # mode samp freq for each pollutant/site/poc/year
      ungroup() %>%
      group_by(SAMPLE_DATE,QUARTER,AQS_PARAMETER_CODE,AMA_SITE_CODE,
               STATE_ABBR,YEAR,DURATION_DESC,AQS_PARAMETER_NAME) %>% # group by pollutant/site/day/duration
      summarize(CONC_DAILY_UG_M3=mean(CONC_RATIO,na.rm=TRUE), # mean of values
                CONC_DAILY_STD=mean(SAMPLE_VALUE_STD,na.rm=TRUE), # mean of values in standard conditions
                MDL_DAILY_STD_UG_M3=mean(MDL_STD_UG_M3,na.rm=TRUE), # mean of MDLs in standard conditions
                ALTERNATE_MDL_DAILY=mean(ALTERNATE_MDL,na.rm=T), # mean of alternate MDL 
                SAMPLING_FREQUENCY_DAILY=mode_STR(SAMPLING_FREQUENCY_MODE)) %>% # mode sampling frequency
      ungroup() %>%
      mutate_all(~case_when(is.nan(.) ~ NA, .default = .)) # add catch to convert NaN to NA
    
    # save file
    fname=paste0(results.dir,'AMA',amayr,'_daily_',allyears[i],'.Rda')
    save(daily,AMA_POCCOUNT,daily_REMOTE,AMA_POCCOUNT_REMOTE,file=fname)

    variable_list <- list(
    daily = daily,
    AMA_POCCOUNT = AMA_POCCOUNT,
    daily_REMOTE = daily_REMOTE,
    AMA_POCCOUNT_REMOTE = AMA_POCCOUNT_REMOTE)

    #filelist2[[i]]=fname
    filelist2[[i]]=variable_list
  } # looping through each AMA year
  return(filelist2)
}
#nolint end