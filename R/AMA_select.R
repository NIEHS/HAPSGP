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
# nolint start
AMA_select <- function(data.dir,allyears,chemlist){

filelist=list()
for(i in 1:length(allyears)){
    # load year of AMA data
    print(allyears[i])
    load(paste(data.dir,'AMA_',allyears[i],'.Rda',sep=''))
    
    # remove select data and rename select fields
    AMA = AMA %>% dplyr::filter(AQS_PARAMETER_NAME %in% chemlist)

 #   fname=paste0(results.dir,'AMA_select_',allyears[i],'.Rda')
 
 #save(AMA, file=fname)
 
 #filelist[[i]]=fname
 filelist[[i]]=AMA

}
return(filelist)
}
#nolint end