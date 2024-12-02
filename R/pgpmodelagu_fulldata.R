# Run model with full dataset

# Run PrestoGP model on sh

# PrestoGP on HAPS + GRIDMET
# devtools::install_github("NIEHS/PrestoGP")
library(PrestoGP)
library(dplyr)

setwd("/ddn/gs1/home/kassienma/HAPSGP/")

dates=c("2018-01-01","2021-12-31")
dates_vec=seq(as.Date(dates[1]), as.Date(dates[2]), "days")

# Read in HAPS
df=readRDS("output/AGU/haps_gridmet_208-2021.rds")

# remove rows with measurement=0 (will change this later)
df= df %>% filter(CONC_DAILY_UG_M3 != 0)

# remove rows with NA's in MDL (may change this later)
mdl_ind = which(colnames(df)== "MDL_DAILY_STD_UG_M3")
df <- df[!Reduce(`|`, lapply(df[, mdl_ind, drop = FALSE], is.na)), ]

# Select only one instance in case of repeat site-times with different duration_desc
df=df %>% group_by(AQS_PARAMETER_CODE,AMA_SITE_CODE,time) %>% # group by pollutant/site/year/duration (across days)
    filter(DURATION_DESC == min(DURATION_DESC)) %>%
    ungroup() 

df2=df%>% filter(time %in% dates_vec)%>%
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

#Get indeces
#cov_buffers=c("_0|10000") #Gonna work with just one buffer for now
cov_buffers=c("_0")
cov_ind <- grep(cov_buffers, names(dfchem))
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
all.mvm2 <- prestogp_fit(all.mvm, ym, Xm, locsm, lod=lodsm, scaling = c(1, 1, 2),impute.y=TRUE, verbose=TRUE)

# Add output to lists
results=list()
results$dates_vec=dates_vec
results$model=all.mvm2

saveRDS(results, paste0("output/AGU/pgpagu_fulldata_",dates[1],"_",dates[2],".RDS"))
