# PrestoGP on HAPS + GRIDMET
# devtools::install_github("NIEHS/PrestoGP")
library(PrestoGP)
library(dplyr)
#library(openair)
# Read in HAPS
df=readRDS("output/AGU/haps_gridmet_208-2021.rds")

#cov_buffers=c("_0|10000") #Gonna work with just one buffer for now
cov_buffers=c("_0")
cov_ind <- grep(cov_buffers, names(df))
loc_ind = c(which(colnames(df)== "lon"), which(colnames(df)== "lat"))

# remove rows with NA's in covariates
df <- df[!Reduce(`|`, lapply(df[, cov_ind, drop = FALSE], is.na)), ]

# remove rows with measurement=0 (will change this later)
df= df %>% filter(CONC_DAILY_UG_M3 != 0)

# remove rows with NA's in MDL (may change this later)
mdl_ind = which(colnames(df)== "MDL_DAILY_STD_UG_M3")
df <- df[!Reduce(`|`, lapply(df[, mdl_ind, drop = FALSE], is.na)), ]

# Select only one instance in case of repeat site-times with different duration_desc
df=df %>% group_by(AQS_PARAMETER_CODE,AMA_SITE_CODE,time) %>% # group by pollutant/site/year/duration (across days)
    filter(DURATION_DESC == min(DURATION_DESC)) %>%
    ungroup()

# check variables correlation
#cortest=corPlot(ben[,cov_ind])
#cordf=cortest$data
# Buffers are too correlated with each other, use only one buffer distance per model

#######
# SINGLE DAY SINGLE POLLUTANT ANALYSIS
#######
# 2D matrix with site*cov 
# Separate a single pollutant for initial analysis (to minimize bamboozlement)
ben=df %>% filter(AQS_PARAMETER_NAME == "Benzene")

ben01=ben %>% filter(time == "2021-07-01")

# Create training and test sets
n <- nrow(ben01)
otr <- rep(FALSE, n)
otr[sample(1:n, size=floor(n/2))] <- TRUE
otst <- !otr


y=ben01$CONC_DAILY_STD
#ynames="Concentration"

X=as.matrix(ben01[,cov_ind])
#Xnames=colnames(ben01)[cov_ind]

locs = as.matrix(ben01[,loc_ind]) 
lods = ben01$MDL_DAILY_STD_UG_M3

#Vecchia Model
ben.vm <- new("VecchiaModel", n_neighbors = 10)
ben.vm2 <- prestogp_fit(ben.vm, y[otr], X[otr,], locs[otr,], impute.y=TRUE, lod=lods[otr], verbose=TRUE)

# Perform predictions on the test set
ben.yhat <- prestogp_predict(ben.vm2, X[otst,], locs[otst,])



#############
# Multi day analysis
#############
# Separate a single pollutant for initial analysis (to minimize bamboozlement)
ben=df %>% filter(AQS_PARAMETER_NAME == "Benzene")

# Make toy example with 2 days for now
dates=c("2021-01-01","2021-01-03")
dates_vec=seq(as.Date(dates[1]), as.Date(dates[2]), "days")

ben_3=ben %>% filter(time %in% dates_vec) %>%
  #group_by(AMA_SITE_CODE) %>%
  #filter(all(dates %in% time)) %>%  # Keep sites with all required dates
  mutate(time=as.numeric(as.Date(time)))#%>%
  #ungroup()

loc_ind = c(which(colnames(ben)== "lon"), which(colnames(ben)== "lat"))
time_ind = which(colnames(ben)== "time")

y2 <- log(ben_3$CONC_DAILY_STD)
X2 <- as.matrix(ben_3[,cov_ind])
# columns 1+2 are location coordinates; column 3 is time
locs2 <- as.matrix(ben_3[,c(loc_ind,time_ind)])
lods2 = log(ben_3$MDL_DAILY_STD_UG_M3)

# Create training and test sets
n <- nrow(ben_3)
otr <- rep(FALSE, n)
otr[sample(1:n, size=floor(n/2))] <- TRUE
otst <- !otr

ben.vm <- new("VecchiaModel", n_neighbors = 10)
# fit separate scale parameters for location and elevation
ben.vm2 <- prestogp_fit(ben.vm, y2[otr], X2[otr,], locs2[otr,], scaling = c(1, 1, 2),impute.y=TRUE, lod=lods2[otr], verbose=TRUE)

ben.2yhat <- prestogp_predict(ben.vm2, X2[otst,], locs2[otst,])

#############
# Multi-pollutant, multi-day analysis
#############

# Make toy example with 2 days for now
dates=c("2018-01-01","2018-01-31")
dates_vec=seq(as.Date(dates[1]), as.Date(dates[2]), "days")

df_3=df %>% filter(time %in% dates_vec) %>%
 # filter(AQS_PARAMETER_NAME != "Hexane") %>%
 # group_by(AMA_SITE_CODE,AQS_PARAMETER_CODE) %>%
 # filter(all(dates %in% time)) %>%  # Keep sites with all required dates
  mutate(time=as.numeric(as.Date(time)))#%>%
 # ungroup()

loc_ind2 = c(which(colnames(df_3)== "lon"), which(colnames(df_3)== "lat"))
time_ind2 = which(colnames(df_3)== "time")

#Initialize lists
ym <- list()
Xm <- list()
locsm <- list()
lodsm <- list()

Xmtest <- list()
locsmtest <- list()

n <- nrow(df_3)
otr <- rep(TRUE, n)
otr[sample(1:n, size=floor(n/10))] <- FALSE
otst <- !otr

dftrain=df_3[otr,]
dftest=df_3[otst,]
#Loop through chemicals to populate lists
chemlist=unique(df_3$AQS_PARAMETER_NAME)
for(i in 1:length(chemlist)){
#dfchem=df_3 %>% filter(AQS_PARAMETER_NAME == chemlist[i])
dfchem=dftrain %>% filter(AQS_PARAMETER_NAME == chemlist[i])


y3 <- log(dfchem$CONC_DAILY_STD)
X3<- as.matrix(dfchem[,cov_ind])
colnames(X3)=paste0(colnames(X3), chemlist[i])

# columns 1+2 are location coordinates; column 3 is time
locs3 <- as.matrix(dfchem[,c(loc_ind2,time_ind2)])
lods3 = log(dfchem$MDL_DAILY_STD_UG_M3)

ym[[i]] <- y3
Xm[[i]] <- X3
locsm[[i]] <- locs3
lodsm[[i]] <- lods3

# set up test X and locs
dfchem_test=dftest %>% filter(AQS_PARAMETER_NAME == chemlist[i])
Xt<- as.matrix(dfchem_test[,cov_ind])
colnames(Xt)=paste0(colnames(Xt), chemlist[i])

locst <- as.matrix(dfchem_test[,c(loc_ind,time_ind)])

Xmtest[[i]] <- Xt
locsmtest[[i]] <- locst

}

# Multivariate Vecchia model
all.mvm <-  new("MultivariateVecchiaModel", n_neighbors = 10)
all.mvm2 <- prestogp_fit(all.mvm, ym, Xm, locsm, lod=lodsm, scaling = c(1, 1, 2),impute.y=TRUE, verbose=TRUE)

pred <- prestogp_predict(model=all.mvm2,X = Xmtest,locs = locsmtest)


library(sf)
library(mapview)

df_locs= df %>% distinct(AMA_SITE_CODE,lon,lat)

dfsf=st_as_sf(df_locs, coords=c("lon","lat"), crs="EPSG:4326")
mapview(dfsf)
