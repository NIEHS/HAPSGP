# Run PrestoGP model on sh

# PrestoGP on HAPS + GRIDMET
# devtools::install_github("NIEHS/PrestoGP")
library(PrestoGP)
library(dplyr)
library(rsample)
library(spatialsample)
library(sf)
library(Metrics)

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

# Convert to sf
#df_sf = st_as_sf(df2, coords=c("lon","lat"),crs="EPSG:4326")

# Separate into cross-validation sets
#cvsplit <- spatial_block_cv(df_sf, v = 5, method = "snake")
#for (h in 1:5){
#h=2
#dftrain <- spatialsample::analysis(cvsplit$splits[[h]])%>% 
#         mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])
#dftest <- spatialsample::assessment(cvsplit$splits[[h]])%>% 
#         mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])
#} CV

set.seed(33)
rep=5
otr<-list()
otst<-list()

model.list <-list()
pred.list <-list()
ytest.list<-list()
mse.list<-list()

for(r in 1:rep){
  print (paste("CV fold", r))
n <- nrow(df2)
otrvec <- rep(TRUE, n)
otrvec[sample(1:n, size=floor(n/10))] <- FALSE
otstvec <- !otrvec

otr[[r]]<-otrvec
otst[[r]]<-otstvec

dftrain=df2[otr[[r]],]
dftest=df2[otst[[r]],]

#Initialize lists
ym <- list()
Xm <- list()
locsm <- list()
lodsm <- list()

ytest <- list()
Xtest <- list()
locstest <- list()

#Loop through chemicals to populate lists
chemlist=sort(unique(df2$AQS_PARAMETER_NAME))

#chemlist="Hexane"
for(i in 1:length(chemlist)){
dfchem=as.data.frame(dftrain %>% filter(AQS_PARAMETER_NAME == chemlist[i]))
#dfchem=as.data.frame(df_sf %>% filter(AQS_PARAMETER_NAME == chemlist[i])%>% 
#         mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2]))
#dfchem=df2 %>% filter(AQS_PARAMETER_NAME == chemlist[i])
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

# set up test X and locs
dfchem_test=dftest %>% filter(AQS_PARAMETER_NAME == chemlist[i])

# remove rows with NA's in covariates
dfchem_test <- dfchem_test[!Reduce(`|`, lapply(dfchem_test[, cov_ind, drop = FALSE], is.na)), ]

yt<- log(dfchem_test$CONC_DAILY_STD)
Xt<- as.matrix(dfchem_test[,cov_ind])[,1:length(cov_ind)]
colnames(Xt)=paste0(colnames(Xt), chemlist[i])

locst <- as.matrix(dfchem_test[,loc_ind])[,1:3]

ytest[[i]] <- yt
Xtest[[i]] <- Xt
locstest[[i]] <- locst
}

# Multivariate Vecchia model
all.mvm <-  new("MultivariateVecchiaModel", n_neighbors = 10)
all.mvm2 <- prestogp_fit(all.mvm, ym, Xm, locsm, lod=lodsm, scaling = c(1, 1, 2),impute.y=TRUE, verbose=TRUE)

pred <- prestogp_predict(model=all.mvm2,X = Xtest,locs = locstest)

model.mse=mse(exp(unlist(ytest)),exp(unlist(pred)))

# Add output to lists
ytest.list[[r]]=ytest
model.list[[r]]=all.mvm2
pred.list[[r]]=unlist(pred)
mse.list[[r]]=model.mse

} #r

results=list()

results$dates_vec=dates_vec
results$otr=otr
results$otst=otst
results$ytest.list=ytest.list
results$model.list=model.list
results$pred.list=pred.list
results$mse.list=mse.list

saveRDS(results, paste0("output/AGU/pgpagu_cvfit_",dates[1],"_",dates[2],".RDS"))