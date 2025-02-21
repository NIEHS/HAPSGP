# Run model with full dataset
# nolint start
# Run PrestoGP model on sh

# PrestoGP on HAPS + GRIDMET
# devtools::install_github("NIEHS/PrestoGP")
#library(PrestoGP)
#library(dplyr)
#dates=c("2021-01-01","2021-12-31")
#data="output/AGU/haps_gridmet_208-2021.rds"

#setwd("/ddn/gs1/home/kassienma/HAPSGP/")

pgp_fit=function(data,dates, vars){

# Read in HAPS
#df=readRDS(data)
df=as.data.frame(data)

# change rows with measurement=0 to very small values 
df <- df %>%
  mutate(CONC_DAILY_STD = if_else(CONC_DAILY_STD <= 0, MDL_DAILY_STD_UG_M3 / 20, CONC_DAILY_STD)) %>%
  filter(CONC_DAILY_STD < 300 )

# Change NA's or <=0s in MDL 
df <- df %>%
  group_by(AQS_PARAMETER_CODE) %>%
  mutate(MDL_DAILY_STD_UG_M3 = if_else(MDL_DAILY_STD_UG_M3 <= 0 | is.na(MDL_DAILY_STD_UG_M3),
                                       min(CONC_DAILY_STD[CONC_DAILY_STD > 0], na.rm = TRUE) / 10,
                                       MDL_DAILY_STD_UG_M3)) %>%
  ungroup()

# Select only one instance in case of repeat site-times with different duration_desc
df=df %>% group_by(AQS_PARAMETER_CODE,AMA_SITE_CODE,time) %>% # group by pollutant/site/year/duration (across days)
    filter(DURATION_DESC == min(DURATION_DESC)) %>%
    ungroup() 

df2=df%>% filter(time %in% dates)%>%
  #convert days to time 
  mutate(time=as.numeric(as.Date(time)))

#Initialize lists
ym <- list()
Xm <- list()
locsm <- list()
lodsm <- list()

#Loop through chemicals to populate lists
chemlist=sort(unique(df2$AQS_PARAMETER_NAME))

#chemlist="Hexane"
for(i in 1:length(chemlist)){
dfchem=as.data.frame(df2 %>% filter(AQS_PARAMETER_NAME == chemlist[i]))

#Get covariate indeces
#cov_buffers=c("_0|10000") #Gonna work with just one buffer for now
noncov_names=c(vars,"time","lon","lat","year")
cov_ind <- which(!names(dfchem) %in% noncov_names)
loc_ind = c(which(colnames(dfchem)== "lon"), which(colnames(dfchem)== "lat"),which(colnames(dfchem)== "time"))

# remove rows with NA's in covariates
dfchem <- dfchem[!Reduce(`|`, lapply(dfchem[, cov_ind, drop = FALSE], is.na)), ]

y3 <- log(dfchem$CONC_DAILY_STD)
X3<- as.matrix(dfchem[,cov_ind])[,1:length(cov_ind)]
colnames(X3)=paste0(colnames(X3), chemlist[i])

# columns 1+2 are location coordinates; column 3 is time
locs3 <- as.matrix(dfchem[,loc_ind])[,1:3]
lods3 = log(dfchem$MDL_DAILY_STD_UG_M3)

ym[[i]] <- y3
Xm[[i]] <- X3
locsm[[i]] <- locs3
lodsm[[i]] <- lods3

}

# Multivariate Vecchia model
all.mvm <-  new("MultivariateVecchiaModel", n_neighbors = 10)
all.mvm2 <- prestogp_fit(all.mvm, ym, Xm, locsm, lod=lodsm, scaling = c(1, 1, 2),impute.y=TRUE, verbose=TRUE,relax=TRUE)

print("Model fit done. Saving results...")

# Add output to lists
results=list()
results$dates_vec=dates
results$model=all.mvm2
results$chemlist=chemlist

#saveRDS(results, paste0("output/AGU/pgpagu_fulldata_",dates[1],"_",dates[length(dates)],".RDS"))
return(results)
}
#nolint end