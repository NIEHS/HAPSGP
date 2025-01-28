################################################################################
#nolint start
## PURPOSE: AMA_preprocessing.R performs pre-processing of the data to get it in 
##          a usable form to calculate daily averages
## INPUT:   data.dir - directory for all AMA files (type=character)
##          results.dir - directory for all results (type=character)
##          amayr - version year for the AMA (type=number;YYYY)
##          allyears - years of AMA data (type=vector of numbers;YYYY:YYYY)
## CREATES: AMA*amayr*_preprocessing_YYYY.Rda
## PART 1:  Initialize AQS parameter lists
## PART 2:  Derive local conditions from LC/STD ratios
## ACRONYMS:AMA = Ambient Monitoring Archive
##          AQS = Air Quality System
##          LC = Local Conditions 
##          STD = standard conditions
################################################################################
AMA_preprocessing <- function(filelist_ama,filelist_lcstd,AMA_RatioSite){
#AMA_preprocessing <- function(filelist_ama,filelist_lcstd,AMA_RatioSite,results.dir,amayr,allyears){
    
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
  
  ##### DERIVE LOCAL CONDITIONS FROM LC/STD RATIOS #####
  filelist=list()
  # looping through each year
  for(i in 1:length(filelist_ama)){
    
    # load year of AMA data
   # print(allyears[i])
    #load(paste(data.dir,'AMA_',allyears[i],'.Rda',sep=''))
    #load(filelist_ama[[i]])
    AMA=filelist_ama[[i]]
    # remove select data and rename select fields
    AMA_sub = data.table(AMA) %>%
      mutate(HOUR=substr(SAMPLE_START_TIME,1,2)) %>% # create field with the hourly start times
      mutate(AQS_PARAMETER_CODE = AQS_PARAMETER_CODE_FINAL,
             AQS_PARAMETER_NAME = AQS_PARAMETER_NAME_FINAL) %>% # use final fields
      select(-c(AQS_PARAMETER_CODE_FINAL,AQS_PARAMETER_NAME_FINAL)) %>% # delete 'FINAL' columns
      mutate(SAMPLE_VALUE_STD_FINAL_UG_M3 = ifelse(SAMPLE_VALUE_STD==0 & SAMPLE_VALUE_STD_FINAL_UG_M3>0 & !is.na(SAMPLE_VALUE_STD),
                                                   0, SAMPLE_VALUE_STD_FINAL_UG_M3)) %>%
      subset(!(SAMPLE_VALUE_STD_FINAL_UG_M3=='' | is.na(SAMPLE_VALUE_STD_FINAL_UG_M3))) %>% # remove null measurements
      subset(!(AQS_PARAMETER_CODE=='85129' & AMA_SITE_CODE=='120573002' & AQS_POC==1)) %>% # remove data
      subset(!(DURATION_DESC=='1 MONTH' | DURATION_DESC=='Integrated Passive 2-weeks')) %>% # remove data
      subset(!(AQS_PARAMETER_CODE=='17201' & AMA_SITE_CODE=='130890002' & SAMPLE_DATE=='02/09/2013' & AQS_POC=='9')) %>%  # GA C16H10 2013
      subset(!(AQS_PARAMETER_CODE %in% c(AQS_4CLBD,AQS_phos,AQS_uran,AQS_polybi,AQS_other,AQS_crsmet))) %>% # remove some parameters
      mutate(AQS_PARAMETER_CODE = case_when(as.character(AMA_SITE_CODE)=='06037BALD' & 
                                              as.character(AQS_PARAMETER_CODE)=='17141' ~ '45850',
                                            TRUE ~ as.character(AQS_PARAMETER_CODE)),
             AQS_PARAMETER_NAME = case_when(as.character(AMA_SITE_CODE)=='06037BALD' & 
                                              as.character(AQS_PARAMETER_CODE)=='45850' ~ 'Naphthalene',
                                            TRUE ~ as.character(AQS_PARAMETER_NAME))) %>% # 060367BALD naphthalene is PPB so change from 17141 to 45850 and parameter name
      mutate(DURATION_DESC = case_when(as.character(AMA_SITE_CODE)=='06037BALD' & as.numeric(as.character(YEAR)) == 2013 & 
                                         as.character(DURATION_DESC) == '24 HOURS' ~ '1 HOUR', 
                                       TRUE ~ DURATION_DESC)) %>% # added 05/19/2022 - some 2013 060367BALD records were mislabeled
      mutate(AMA_SITE_CODE = case_when(AMA_SITE_CODE=='410610123' & YEAR=='2016' ~ '410610119', 
                                       TRUE ~ as.character(AMA_SITE_CODE))) %>% # La Grande changed from 410610119 to 410610123 in 2016 between Q3 and Q4
      mutate(AQS_PARAMETER_CODE = recode(AQS_PARAMETER_CODE, '85129' = '85128'), # change lead name and parameter code
             AQS_PARAMETER_NAME = recode(AQS_PARAMETER_NAME, 'Lead Pm10 Lc Frm/Fem' = 'Lead Pm10 Lc')) 
    
    # load LC/STD ratio data
    #load(paste(results.dir,'AMA',amayr,'_LCSTDRatio_',allyears[i],'.Rda',sep='')) # load sites with LC/STD ratio for this year
    #load(paste(results.dir,'AMA',amayr,'_LCSTDRatio_Site','.Rda',sep='')) # load sites with LC/STD ratio across all years
    #load(filelist_lcstd[[i]])# load sites with LC/STD ratio for this year
    list2env(filelist_lcstd[[i]], envir = .GlobalEnv) # load sites with LC/STD ratio for this year
    #load(file_lcstdsite)# load sites with LC/STD ratio across all years
    # derive local concentrations from LC/STD ratios
    AMA = AMA_sub %>% 
      join(AMA_RatioPollPOCDayDur) %>% # join by pollutant/POC/day/sampling duration LC/STD ratios 
      join(AMA_RatioSiteDay) %>% # join by site/day LC/STD ratios 
      join(AMA_RatioSiteQuart) %>% # join by site/quarter LC/STD ratios 
      join(AMA_RatioSiteYr) %>% # join by site/year LC/STD ratios 
      join(AMA_RatioSite) %>% # join by site LC/STD ratios 
      mutate(LCSTDratio = LCSTDratio_PollPOCDayDur) %>% # establish the final LC/STD ratio field
      mutate(LCSTDratio = ifelse(is.na(LCSTDratio), LCSTDratio_SiteDay, LCSTDratio)) %>% # if poll/poc/day/dur missing, fill with site/day
      mutate(LCSTDratio = ifelse(is.na(LCSTDratio), LCSTDratio_SiteQuart, LCSTDratio)) %>% # if site/day missing, fill with site/quarter
      mutate(LCSTDratio = ifelse(is.na(LCSTDratio), LCSTDratio_SiteYr, LCSTDratio)) %>% # if site/quarter missing, fill with site/year
      mutate(LCSTDratio = ifelse(is.na(LCSTDratio), LCSTDratio_Site, LCSTDratio)) %>% # if site/year missing, fill with site
      mutate(LCSTDratio = ifelse(is.na(LCSTDratio), 1, LCSTDratio)) %>% # if site missing, fill with '1' (i.e. LC=STD)
      mutate(CONC_RATIO = case_when(SAMPLE_VALUE_STD_FINAL_TYPE == 'S' ~ SAMPLE_VALUE_STD_FINAL_UG_M3 * LCSTDratio, 
                                    TRUE ~ SAMPLE_VALUE_STD_FINAL_UG_M3)) %>% # calculating derived concentrations using the LCSTDratio
      mutate(TYPE_RATIO = case_when(SAMPLE_VALUE_STD_FINAL_TYPE == 'L' ~ 'LOCALCONDITIONS_COUNT',
                                    SAMPLE_VALUE_STD_FINAL_TYPE == 'S' & LCSTDratio != 1 ~ 'DERIVEDLOCALCONDITIONS_COUNT',
                                    TRUE ~ 'STANDARDCONDITIONS_COUNT')) # id where the derived conc is local, standard, or derived
    
    # count the types after removing zeros by pollutant/site/year/sampling duration 
    # NOTE: this will be brought into 'AMA_daily2annual.R'
    AMA_TYPECOUNT = AMA %>%
      subset(CONC_RATIO!=0) %>%
      select(AQS_PARAMETER_CODE,AMA_SITE_CODE,YEAR,DURATION_DESC,TYPE_RATIO,CONC_RATIO) %>%
      pivot_wider(names_from=TYPE_RATIO, values_from=CONC_RATIO, values_fn=length, values_fill = 0)
    
    # add a catch just in case there a certain type is missing
    # NOTE: if none of the types are missing, this will produce a warning
    typenms = c('STANDARDCONDITIONS_COUNT','LOCALCONDITIONS_COUNT','DERIVEDLOCALCONDITIONS_COUNT')
    typenms_fill = typenms[!(typenms %in% names(AMA_TYPECOUNT))] # the fields that are missing
    AMA_TYPECOUNT[,typenms_fill] = 0
    
   # fname=paste(results.dir,'AMA',amayr,'_preprocessing_',allyears[i],'.Rda',sep='')
    # save file
   # save(AMA,AMA_TYPECOUNT,file=fname)
    #filelist[[i]]=fname
    filelist[[i]]=AMA

  } # looping through each AMA year 
  return(filelist)
}
# nolint end