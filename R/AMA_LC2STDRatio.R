################################################################################
# nolint start
## PURPOSE: AMA_LC2STDRatio.R creates the ratio between LC and standard 
##          conditions of air toxics in the AMA data set. This ratio is then 
##          later used to estimate LC when only standard conditions are known.
## INPUT:   data.dir - directory for all AMA files (type=character)
##          results.dir - directory for all results (type=character)
##          amayr - version year for the AMA (type=number;YYYY)
##          allyears - years of AMA data (type=vector of numbers;YYYY:YYYY)
## CREATES: AMA*amayr*_LCSTDRatio_YYYY.Rda
##          AMA*amayr*_LCSTDRatio_Site.Rda
## PART 1:  Initialize AQS parameter lists
## PART 2:  Find where STD is incomplete and computer LC/STD ratios
## PART 3:  Combine all the ratio data together 
## NOTE:    A site does not have a complete STD if there is a 
##          SAMPLE_VALUE_FINAL_STD_UG_M3 but not SAMPLE_VALUE_STD.
## ACRONYMS:AMA = Ambient Monitoring Archive
##          AQS = Air Quality System 
##          LC = Local Conditions 
##          STD = standard conditions
################################################################################
AMA_LC2STDRatio <- function(filelist){
#AMA_LC2STDRatio <- function(filelist,results.dir,amayr,allyears){
  ##### PART 1: INITIALIZE AQS PARAMETER LISTS #####
  
  AQS_4CLBD = '17902' # Dibenzo[b,e][1,4]dioxin,2,3,7,8-tetrachloro (TSP) STP
  AQS_phos = c('14152','85152','86152','88152') # phosphorous
  AQS_uran = c('14179','86179','88179', '85179') # uranium
  AQS_polybi = c('16232','16233','16235','16237','16239','16241','16242',
                 '16243','16244','16245','16247','16248','16250','16251',
                 '16254','16256','16258','16259','16260','16261','16262',
                 '16264','16266','16267','16268','16270','16271','16272',
                 '16274','16275','16277','16278','16279','16280','16281',
                 '16282','16283','16284','16285','16286','16287','16288',
                 '16289','16290','16293','16294','16295','16296','16298') # 162* are polychlorinated biphenyls
  AQS_other = c('16300','16301','16302','16303','16305','16306','16307',
                '16308','16309','16310','16311','16313','16318','16319',
                '16320','16321','16322','16323','16324','16325','16327',
                '16328','16326','16329','16330','16331','16332','16333',
                '16334','16338','16343','16344') # radionuclides
  AQS_crsmet = c('83102','83103','83110','83112','83113','83115','83128',
                 '83132','83136','83142','83152','83154','83179','86102',
                 '86103','86110','86112','86113','86115','83128','83132',
                 '83136','83142','83152','83154','83179','86102','86103',
                 '86110','86112','86113','86115','86128','86132','86136',
                 '86142','86152','86154','86179') # coarse metals
  
  ##### PART 2: FIND WHERE STD IS INCOMPLETE AND COMPUTE LC/STD RATIOS #####
  filelist2=list()
  # looping through each year
  for(i in 1:length(filelist)){
    
    # load year of AMA data
    #load(paste(data.dir,'AMA_select_',allyears[i],'.Rda',sep=''))
    #load(paste(data.dir,'AMA_select_',allyears[i],'.Rda',sep=''))
    #load(filelist[[i]])
    AMA=filelist[[i]]
    # subsetting data
    AMA_sub = data.table(AMA) %>%
      mutate(AQS_PARAMETER_CODE = AQS_PARAMETER_CODE_FINAL,
             AQS_PARAMETER_NAME = AQS_PARAMETER_NAME_FINAL) %>% # use final fields
      select(-c(AQS_PARAMETER_CODE_FINAL,AQS_PARAMETER_NAME_FINAL)) %>% # delete 'FINAL' columns
      mutate(SAMPLE_VALUE_STD_FINAL_UG_M3 = ifelse(SAMPLE_VALUE_STD==0 & SAMPLE_VALUE_STD_FINAL_UG_M3>0 & !is.na(SAMPLE_VALUE_STD),
                                                   0, SAMPLE_VALUE_STD_FINAL_UG_M3)) %>%
      subset(!(SAMPLE_VALUE_STD_FINAL_UG_M3=='' | is.na(SAMPLE_VALUE_STD_FINAL_UG_M3) | 
                 SAMPLE_VALUE_STD_FINAL_UG_M3==0)) %>% # remove nulls and '0' - ratios determined from detectable values
      subset(!(AQS_PARAMETER_CODE %in% c(AQS_4CLBD,AQS_phos,AQS_uran,AQS_polybi,AQS_other,AQS_crsmet))) %>% # remove some parameters
      subset(!(AMA_SITE_CODE=='271630446' & YEAR%in%'2010' & AQS_PARAMETER_CODE%in%c('43502','43503','43802','43804','45203'))) %>% # MN site 2010
      subset(!(AMA_SITE_CODE=='271630446' & YEAR%in%'2011' & AQS_PARAMETER_CODE%in%c('43502','43503'))) # MN site 2011
    
    # finding unique pollutant/site/year/sampling durations where STD is missing
    AMA_STDmiss = AMA_sub %>%
      subset(is.na(SAMPLE_VALUE_STD)) %>% # where STD is missing
      distinct(AQS_PARAMETER_CODE,AMA_SITE_CODE,DURATION_DESC,YEAR,AQS_PARAMETER_NAME) # unique pollutant/site/year/durations
    
    # ratio (where it can be computed) by year for pollutant/POC/sampling duration/day
    AMA_RatioPollPOCDayDur = AMA_sub %>%
      subset(SAMPLE_VALUE_STD_FINAL_TYPE=='L' & !is.na(SAMPLE_VALUE_STD)) %>% # where ratios can be computed
      group_by(YEAR,AQS_PARAMETER_CODE,AMA_SITE_CODE,DURATION_DESC,AQS_POC,QUARTER,SAMPLE_DATE) %>%
      summarize(avgLC_PollPOCDayDur=mean(SAMPLE_VALUE_STD_FINAL_UG_M3),
                avgSTD_PollPOCDayDur=mean(SAMPLE_VALUE_STD)) %>%
      ungroup() %>%
      mutate(LCSTDratio_PollPOCDayDur = avgLC_PollPOCDayDur/avgSTD_PollPOCDayDur) %>%
      select(-c(avgLC_PollPOCDayDur,avgSTD_PollPOCDayDur)) # remove unnecessary columns
    
    # ratio by year for site/day
    AMA_RatioSiteDay = AMA_RatioPollPOCDayDur %>%
      group_by(YEAR,AMA_SITE_CODE,QUARTER,SAMPLE_DATE) %>%
      summarize(LCSTDratio_SiteDay=mean(LCSTDratio_PollPOCDayDur)) %>%
      ungroup()
    
    # ratio by year for site/quarter
    AMA_RatioSiteQuart = AMA_RatioSiteDay %>%
      group_by(YEAR,AMA_SITE_CODE,QUARTER) %>%
      summarize(LCSTDratio_SiteQuart=mean(LCSTDratio_SiteDay)) %>%
      ungroup()
    
    # ratio by year for site
    AMA_RatioSiteYr = AMA_RatioSiteQuart %>%
      group_by(YEAR,AMA_SITE_CODE) %>%
      summarize(LCSTDratio_SiteYr=mean(LCSTDratio_SiteQuart)) %>%
      ungroup()
    
    # save file
   # fname=paste(results.dir,'AMA',amayr,'_LCSTDRatio_',allyears[i],'.Rda',sep='')
   # save(AMA_STDmiss,AMA_RatioPollPOCDayDur,AMA_RatioSiteDay,AMA_RatioSiteQuart,AMA_RatioSiteYr,
   #      file=fname)
    
    LC2STD_list= list(
    AMA_STDmiss = AMA_STDmiss,
    AMA_RatioPollPOCDayDur = AMA_RatioPollPOCDayDur,
    AMA_RatioSiteDay = AMA_RatioSiteDay,
    AMA_RatioSiteQuart = AMA_RatioSiteQuart,
    AMA_RatioSiteYr = AMA_RatioSiteYr)

    filelist2[[i]]=LC2STD_list
    #filelist2[[i]]=fname

  } # looping through each AMA year 
  return(filelist2)
} # End function

AMA_LC2STDRatio_Site=function(filelist){
#AMA_LC2STDRatio_Site=function(filelist,results.dir,amayr,allyears){
  ##### PART 3: COMBINE ALL THE RATIO DATA TOGETHER #####  
  # combine all the years together and get a ratio across all years per site
  
  AMA_RatioAll = list() # create empty list to put all the ratio data in
  # looping through each year
  for(i in 1:length(filelist)){
   # print(allyears[i])
    ylist=filelist[[i]] # load year of data
    AMA_RatioAll[[i]] = ylist$AMA_RatioSiteYr # adding to the list
  }
  AMA_RatioAll = do.call('rbind', AMA_RatioAll) # unlisting all ratio data into one dataframe
  
  # ratio across all years for pollutant/POC
  AMA_RatioSite = AMA_RatioAll %>% group_by(AMA_SITE_CODE) %>% mutate(LCSTDratio_Site=mean(LCSTDratio_SiteYr))
  
 # fname=paste(results.dir,'AMA',amayr,'_LCSTDRatio_Site','.Rda',sep='')
  # save file
 # save(AMA_RatioSite,file=fname)
  
  return(AMA_RatioSite)
  #return(fname)
}
#nolint end