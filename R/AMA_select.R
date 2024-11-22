# AMA_select.R
# Author: Mariana Kassien
################################################################################
## PURPOSE: AMA_select.R selects a subset of the AMA chemicals
## INPUT:   data.dir - directory for all AMA files (type=character)
##          results.dir - directory for all results (type=character)
##          allyears - years of AMA data (type=vector of numbers;YYYY:YYYY)
##          chemlist - names of chemicals of interest
## CREATES: AMA_YYYY.Rda 
## NOTE: The output files for this function have the same name as the input, so 
##       input and output directories must be different to avoid overwriting
################################################################################

AMA_select <- function(data.dir,results.dir,allyears,chemlist){
    if(data.dir==results.dir){
    stop("input and output directories cannot be the same, or function will overwrite input.")
    }
for(i in 1:length(allyears)){
    # load year of AMA data
    print(allyears[i])
    load(paste(data.dir,'AMA_',allyears[i],'.Rda',sep=''))
    
    # remove select data and rename select fields
    AMA = AMA %>% dplyr::filter(AQS_PARAMETER_NAME %in% chemlist)
 
 save(AMA, file=paste0(results.dir,'AMA_',allyears[i],'.Rda'))
 
}}
