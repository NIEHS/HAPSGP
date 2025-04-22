################################################################################
## PURPOSE: AMA_daily2annual.R calculates annual averages from daily 
##          averages per site/pollutant/year/sampling duration
## INPUT:   results.dir - directory for all results (type=character)
##          amayr - version year for the AMA (type=number;YYYY)
##          allyears - years of AMA data (type=vector of numbers;YYYY:YYYY)
## CREATES: AMA*amayr*_annual_YYYY.Rda
## PART 1:  Initialize global variables
## PART 2:  Create fields needed for annual statistics 
## PART 3:  Calculate quarterly statistics
## PART 4:  Calculate annual statistics
## ACRONYMS:AMA = Ambient Monitoring Archive
##          AQS = Air Quality System
##          POC = Parameter Occurrence Code
##          ROS = Regression on Order Statistics
##          STD = standard conditions
## NOTES: (1) steps for creating CONC_DAILY_ROS and CONC_DAILY_STDROS
##            (a) replace NDs with MDLs
##            (b) replace MDLs with annual min where annual min < MDL
##            (c) remove values where annual percent NDs exceeds threshold
##        (2) CONC_DAILY_UG_M3 not used to find daily minimum because it 
##            contains zeros while CON_DAILY_ROS and CONC_DAILY_STDROS do not 
##            contain zeros
################################################################################
AMA_daily2annual <- function(results.dir,amayr,allyears){
  
  ##### PART 1: INITIALIZE GLOBAL VARIABLES #####
  
  dNquart_min = 6 # minimum number of daily averages needed to create a valid quarter
  quartNyr_min = 3 # minimum number of valid quarters needed to create a valid annual average
  per_NDROS = 80 # percent of non-detects needed for a given pollutant/site/year/duration to not calculate ROS
  minNROS = 6 # minimum number of values to calculate a ROS average
  
  # defining function used in this script
  mode_STR <- function(x) { ux <- unique(x); ux[which.max(tabulate(match(x, ux)))] } # most frequent (i.e. mode) string in vector
  
  ##### PART 2, PART 3, PART 4 #####
  
  # looping through each year
  for(i in 1:length(allyears)){ 
    
    # load data
    print(allyears[i])
    load(paste0(results.dir,'AMA',amayr,'_LCSTDRatio_',allyears[i],'.Rda')) # load data from 'AMA_LC2STDRatio.R'
    load(paste0(results.dir,'AMA',amayr,'_preprocessing_',allyears[i],'.Rda')) # load data from 'AMA_preprocessing.R'
    load(file=paste0(results.dir,'AMA',amayr,'_daily_',allyears[i],'.Rda')) # load data from 'AMA_duration2daily.R'
    rm(AMA) # remove to clear up space
    
    # adding a catch just in case there are years with no missing STD daily averages
    AMA_STDmiss = AMA_STDmiss %>% mutate(drop_STD = ifelse(dim(AMA_STDmiss)[1] > 0,'NO STD',NA))
    
    # adding a catch to make sure that the remote and non-remote data do not have collocated site/days
    # NOTE: in practice, this should not occur
    dailytemp = rbind(data.table(daily),data.table(daily_REMOTE),fill=TRUE)
    a = dim(dailytemp)[1]
    b = dim(dailytemp %>% select(AQS_PARAMETER_CODE,AMA_SITE_CODE,SAMPLE_DATE,DURATION_DESC) %>% distinct())[1]
    if(a!=b){print('Collocation in pollutant/site/day/sampling duration in remote and non-remote data. Combine these collocated values.')}
    rm(dailytemp) # remove to clear up space
    
    ##### PART 2: CREATE FIELDS NEEDED FOR ANNUAL STATISTICS #####
    
    dailyFULL = rbind(data.table(daily),data.table(daily_REMOTE),fill=TRUE) %>% # combine non-remote and remote data
      join(AMA_STDmiss) %>% # join with AMA_STDmiss
      mutate(CONC_CENSORED = ifelse(CONC_DAILY_UG_M3==0,TRUE,FALSE), # where daily averages are censored
             CONC_DAILY_ROS = ifelse(CONC_DAILY_UG_M3==0,MDL_DAILY_STD_UG_M3,CONC_DAILY_UG_M3), # NDs -> MDLs
             CONC_DAILY_STDROS = case_when(drop_STD=='NO STD' ~ NA, # missing STD -> NA
                                           CONC_DAILY_UG_M3==0 ~ MDL_DAILY_STD_UG_M3, # NDs -> MDLs
                                           .default = CONC_DAILY_STD)) %>% # otherwise -> CONC_DAILY_STD
      group_by(AQS_PARAMETER_CODE,AMA_SITE_CODE,YEAR,DURATION_DESC) %>% # group by pollutant/site/year/duration (across days)
      mutate(PER_ND=100*sum(CONC_DAILY_UG_M3==0,na.rm=T)/length(na.omit(CONC_DAILY_UG_M3)), # % of NDs by poll/site/year/duration
             min_DAYNYRROS=min(CONC_DAILY_ROS,na.rm=T), # min daily sample of CONC_DAILY_ROS by poll/site/year/duration 
             min_DAYNYRSTDROS=min(CONC_DAILY_STDROS,na.rm=T)) %>% # min daily sample of CONC_DAILY_STDROS by poll/site/year/duration
      ungroup() %>% 
      mutate_all(~case_when(. == Inf ~ NA, .default = .)) %>% # add catch when min=Inf
      mutate(CONC_DAILY_ROS = case_when(PER_ND>per_NDROS ~ NA, # PER_ND > per_NDROS -> NA
                                        min_DAYNYRROS<MDL_DAILY_STD_UG_M3 & CONC_DAILY_UG_M3==0 ~ min_DAYNYRROS, # min<MDL & 0 -> min
                                        .default = CONC_DAILY_ROS), # otherwise -> CONC_DAILY_ROS
             CONC_DAILY_STDROS = case_when(PER_ND>per_NDROS ~ NA, # PER_ND > per_NDROS -> NA
                                           min_DAYNYRSTDROS<MDL_DAILY_STD_UG_M3 & CONC_DAILY_UG_M3==0 ~ min_DAYNYRSTDROS, # min<MDL & 0 -> min
                                           .default = CONC_DAILY_STDROS)) # otherwise -> CONC_DAILY_STDROS
    
    ##### PART 3: CALCULATE QUARTERLY STATISTICS #####
    
    quarterly = dailyFULL %>%
      select(AQS_PARAMETER_CODE,AMA_SITE_CODE,STATE_ABBR,YEAR,AQS_PARAMETER_NAME,DURATION_DESC,
             QUARTER,SAMPLING_FREQUENCY_DAILY,CONC_DAILY_UG_M3,CONC_DAILY_STD) %>% # fields to select
      group_by(AQS_PARAMETER_CODE,AMA_SITE_CODE,STATE_ABBR,YEAR,
               AQS_PARAMETER_NAME,DURATION_DESC,QUARTER) %>% # group by poll/site/duration/year/quarter
      summarize(COUNT = length(na.omit(CONC_DAILY_UG_M3)), # number of values
                MEAN_UG_M3 = mean(CONC_DAILY_UG_M3,na.rm=T), # mean of values
                MEAN_STD_UG_M3 = mean(CONC_DAILY_STD,na.rm=T), # mean of STD values
                MODEFREQ = mode_STR(SAMPLING_FREQUENCY_DAILY), # mode of sampling frequencies
                VALID = COUNT>=dNquart_min) %>% # quarters containing >= dNquart_min samples
      ungroup() %>%
      mutate_all(~case_when(is.nan(.) ~ NA, .default = .)) %>% # add catch to convert NaN to NA
      group_by(AQS_PARAMETER_CODE,AMA_SITE_CODE,STATE_ABBR,YEAR,AQS_PARAMETER_NAME,DURATION_DESC) %>% # group by poll/site/duration/year
      mutate(COUNT_QWDATA = sum(COUNT>0,na.rm=T), # number of quarters that have data
             MEAN_UG_M3_Q1234 = mean(MEAN_UG_M3,na.rm=T), # mean of quarterly means
             MEAN_STD_UG_M3_Q1234 = mean(MEAN_STD_UG_M3,na.rm=T), # mean of quarterly STD means
             MODEFREQ_Q1234 = mode_STR(MODEFREQ), # mode of quarterly sampling frequencies
             VALID_QNYR = sum(VALID,na.rm=T)>=quartNyr_min) %>% # quarters per year containing >= quartNyr_min valid quarters
      ungroup() %>%
      mutate_all(~case_when(is.nan(.) ~ NA, .default = .)) %>% # add catch to convert NaN to NA
      pivot_wider(names_from = QUARTER, values_from = c(COUNT,MEAN_UG_M3,MEAN_STD_UG_M3,MODEFREQ,VALID),
                  names_sort = TRUE, names_prefix = 'Q') # long to wide format
    
    ##### PART 4: CALCULATE ANNUAL STATISTICS #####
    
    # calculate annual statistics ------> ALL VALUES
    annual = dailyFULL %>%
      group_by(AQS_PARAMETER_CODE,AMA_SITE_CODE,STATE_ABBR,YEAR,AQS_PARAMETER_NAME,DURATION_DESC) %>% # group by poll/site/year/duration
      summarize(MEAN_UG_M3 = mean(CONC_DAILY_UG_M3,na.rm=T), # mean
                VAR_UG_M3 = var(CONC_DAILY_UG_M3,na.rm=T), # variance
                MAX_UG_M3 = max(CONC_DAILY_UG_M3,na.rm=T), # max
                P10_UG_M3 = quantile(CONC_DAILY_UG_M3,0.10,na.rm=T), # 10th %tile
                P25_UG_M3 = quantile(CONC_DAILY_UG_M3,0.25,na.rm=T), # Q1
                P50_UG_M3 = quantile(CONC_DAILY_UG_M3,0.50,na.rm=T), # median
                P75_UG_M3 = quantile(CONC_DAILY_UG_M3,0.75,na.rm=T), # Q3
                P90_UG_M3 = quantile(CONC_DAILY_UG_M3,0.90,na.rm=T), # 90th %tile
                MEAN_MDL_UG_M3 = mean(MDL_DAILY_STD_UG_M3,na.rm=T), # mean MDL
                MIN_MDL_UG_M3 = min(MDL_DAILY_STD_UG_M3,na.rm=T), # min MDL
                MAX_MDL_UG_M3 = max(MDL_DAILY_STD_UG_M3,na.rm=T), # max MDL
                N = length(na.omit(CONC_DAILY_UG_M3)), # n
                ZERO_COUNT = sum(CONC_DAILY_UG_M3==0,na.rm=T), # number of 0s
                PER_ND = 100*sum(CONC_DAILY_UG_M3==0,na.rm=T)/length(na.omit(CONC_DAILY_UG_M3)), # % non-detect
                BELOWMDL_COUNT = sum(CONC_DAILY_STD<MDL_DAILY_STD_UG_M3,na.rm=T), # n below MDL
                PER_BELOWMDL = 100*sum(CONC_DAILY_STD<MDL_DAILY_STD_UG_M3,na.rm=T)/length(na.omit(CONC_DAILY_UG_M3)), # % below MDL
                MODE_FREQ = mode_STR(SAMPLING_FREQUENCY_DAILY), # mode sampling frequency
                MEAN_STD_UG_M3 = mean(CONC_DAILY_STD,na.rm=T), # mean ------> STD VALUES
                VAR_STD_UG_M3 = var(CONC_DAILY_STD,na.rm=T), # variance
                MAX_STD_UG_M3 = max(CONC_DAILY_STD,na.rm=T), # max
                P10_STD_UG_M3 = quantile(CONC_DAILY_STD,0.10,na.rm=T), # 10th %tile
                P25_STD_UG_M3 = quantile(CONC_DAILY_STD,0.25,na.rm=T), # Q1
                P50_STD_UG_M3 = quantile(CONC_DAILY_STD,0.50,na.rm=T), # median
                P75_STD_UG_M3 = quantile(CONC_DAILY_STD,0.75,na.rm=T), # Q3
                P90_STD_UG_M3 = quantile(CONC_DAILY_STD,0.90,na.rm=T)) %>% # 90th %tile
      ungroup() %>%
      mutate_all(~case_when(. == Inf ~ NA, . == -Inf ~ NA, .default = .)) %>% # add catch when Inf/-Inf
      mutate_all(~case_when(is.nan(.) ~ NA, .default = .)) # add catch to convert NaN to NA
    
    # calculate annual statistics ------> ROS ONLY
    annualROS = dailyFULL %>%
      group_by(AQS_PARAMETER_CODE,AMA_SITE_CODE,STATE_ABBR,YEAR,AQS_PARAMETER_NAME,DURATION_DESC) %>% # group by poll/site/year/duration
      mutate(N_ROS=length(na.omit(CONC_DAILY_ROS)), # n needed for ros
             CALC_ROS = !is.na(CONC_DAILY_ROS) & N_ROS>=minNROS) %>% # where the ros can be calculated
      subset(CALC_ROS) %>%
      summarize(MEAN_ROS_UG_M3 = mean(ros(CONC_DAILY_ROS,CONC_CENSORED)),
                P10_ROS_UG_M3=quantile(ros(CONC_DAILY_ROS,CONC_CENSORED),0.10), 
                P25_ROS_UG_M3=quantile(ros(CONC_DAILY_ROS,CONC_CENSORED),0.25), 
                P50_ROS_UG_M3=quantile(ros(CONC_DAILY_ROS,CONC_CENSORED),0.50), 
                P75_ROS_UG_M3=quantile(ros(CONC_DAILY_ROS,CONC_CENSORED),0.75), 
                P90_ROS_UG_M3=quantile(ros(CONC_DAILY_ROS,CONC_CENSORED),0.90)) %>%
      ungroup()
    
    # calculate annual statistics ------> STD ROS ONLY
    annualSTDROS = dailyFULL %>%
      group_by(AQS_PARAMETER_CODE,AMA_SITE_CODE,STATE_ABBR,YEAR,AQS_PARAMETER_NAME,DURATION_DESC) %>% # group by poll/site/year/duration
      mutate(N_STDROS=length(na.omit(CONC_DAILY_STDROS)), # n needed for std ros
             CALC_STDROS = !is.na(CONC_DAILY_STDROS) & N_STDROS>=minNROS) %>% # where std ros can be calculated
      subset(CALC_STDROS) %>%
      summarize(MEAN_STDROS_UG_M3 = mean(ros(CONC_DAILY_STDROS,CONC_CENSORED)),
                P10_STDROS_UG_M3=quantile(ros(CONC_DAILY_STDROS,CONC_CENSORED),0.10), 
                P25_STDROS_UG_M3=quantile(ros(CONC_DAILY_STDROS,CONC_CENSORED),0.25), 
                P50_STDROS_UG_M3=quantile(ros(CONC_DAILY_STDROS,CONC_CENSORED),0.50), 
                P75_STDROS_UG_M3=quantile(ros(CONC_DAILY_STDROS,CONC_CENSORED),0.75), 
                P90_STDROS_UG_M3=quantile(ros(CONC_DAILY_STDROS,CONC_CENSORED),0.90)) %>%
      ungroup()
    
    # combine all data tables
    annualFULL = join_all(list(annual, annualROS, annualSTDROS, quarterly), 
                          by=c('AQS_PARAMETER_CODE','AMA_SITE_CODE','STATE_ABBR',
                               'YEAR','AQS_PARAMETER_NAME','DURATION_DESC'), type='left') %>%
      mutate(MEANROSvMEAN = MEAN_ROS_UG_M3/MEAN_UG_M3, # ratio of mean ros to mean
             MEDIANROSvMEDIAN = P50_ROS_UG_M3/P50_UG_M3) %>% # ratio of median ros to median
      mutate_all(~case_when(. == Inf ~ NA, .default = .)) %>% # add catch with Inf
      join(AMA_POCCOUNT) %>% # add POC_COUNT
      join(AMA_TYPECOUNT) # add LC/STD conditions count
    
    # do some subsetting
    annual = annualFULL %>%
      subset(VALID_QNYR) %>% # subset to only valid years
      select(-c(matches(paste0('MODEFREQ_Q',1:4)),contains('VALID'))) # remove fields
    
    # save annual file 
    save(dailyFULL,quarterly,annualFULL,annual,file=paste0(results.dir,'AMA',amayr,'_annual_',allyears[i],'.Rda'))
    
  } # looping through each AMA year 
  
}