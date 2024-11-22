# PrestoGP on HAPS + GRIDMET
# devtools::install_github("NIEHS/PrestoGP")
library(PrestoGP)
library(dplyr)
library(openair)
# Read in HAPS
df=readRDS("output/AGU/haps_gridmet_208-2021.rds")

# Select only one instance in case of repeat site-times with different duration_desc
df=df %>% group_by(AQS_PARAMETER_CODE,AMA_SITE_CODE,time) %>% # group by pollutant/site/year/duration (across days)
    filter(DURATION_DESC == min(DURATION_DESC)) %>%
    ungroup()

# Separate a single pollutant for initial analysis (to minimize bamboozlement)
ben=df %>% filter(AQS_PARAMETER_NAME == "Benzene")

#cov_buffers=c("_0|10000") #Gonna work with just one buffer for now
cov_buffers=c("_0")
cov_ind <- grep(cov_buffers, names(ben))
loc_ind = c(which(colnames(ben)== "lon"), which(colnames(ben)== "lat"))

# remove rows with NA's
ben <- ben[!Reduce(`|`, lapply(ben[, cov_ind, drop = FALSE], is.na)), ]

# check variables correlation
#cortest=corPlot(ben[,cov_ind])
#cordf=cortest$data
# Buffers are too correlated with each other, use only one buffer distance per model

#######
# SINGLE DAY SINGLE POLLUTANT ANALYSIS
#######
# 2D matrix with site*cov 
ben01=ben %>% filter(time == "2021-07-01")

y=ben01$CONC_DAILY_STD

X=as.matrix(ben01[,cov_ind])

locs = as.matrix(ben01[,loc_ind]) 

#Vecchia Model
ben.vm <- new("VecchiaModel", n_neighbors = 10)
ben.vm2 <- prestogp_fit(ben.vm, y, X, locs)

#############
# Multi day analysis
#############

# Make toy example with 2 days for now
dates=c("2021-07-01","2021-07-02")
ben_3=ben %>% filter(time %in% dates) %>%
  group_by(AMA_SITE_CODE) %>%
  filter(all(dates %in% time)) %>%  # Keep sites with all required dates
  mutate(time=as.numeric(as.Date(time)))%>%
  ungroup()

loc_ind = c(which(colnames(ben)== "lon"), which(colnames(ben)== "lat"))
time_ind = which(colnames(ben)== "time")

y2 <- ben_3$CONC_DAILY_STD
X2 <- as.matrix(ben_3[,cov_ind])
# columns 1+2 are location coordinates; column 3 is time
locs2 <- as.matrix(ben_3[,c(loc_ind,time_ind)])

ben.vm2 <- new("VecchiaModel", n_neighbors = 10)
# fit separate scale parameters for location and elevation
ben.vm2 <- prestogp_fit(soil.vm2, y2, X2, locs2, scaling = c(1, 1, 2))

#############
# Multi-pollutant, multi-day analysis
#############
# 3D Matrix pol*site*cov
# Need to go back and extract covariates at all sites*times

df_3=df %>% filter(time %in% dates) %>%
  group_by(AMA_SITE_CODE) %>%
  filter(all(dates %in% time)) %>%  # Keep sites with all required dates
  mutate(time=as.numeric(as.Date(time)))%>%
  ungroup()

loc_ind2 = c(which(colnames(df_3)== "lon"), which(colnames(df_3)== "lat"))
time_ind2 = which(colnames(df_3)== "time")

# Reshape to array
# Ensure data is sorted
ben_3 <- ben_3 %>% arrange(AMA_SITE_CODE, time)

# Extract unique site and time
sites <- unique(ben_3$AMA_SITE_CODE)
covariates <- names(ben_3)[cov_ind]

# Create 3D arrays
y=array(ben_3$CONC_DAILY_STD, dim=c(length(sites), length(times)))
X=array(ben_3[,cov_ind],dim=c(length(sites), length(times),length(covariates)))

#Multivariate Model
ym <- list()
ym[[1]] <- y[,1]             # predict two nitrogen concentration levels
ym[[2]] <- y[,2]

Xm <- list()
Xm[[1]] <- X[,1,]             # predict two nitrogen concentration levels
Xm[[2]] <- X[,2,]

locs = as.matrix(ben_3[1:length(sites),loc_ind])
locsm <- list()
locsm[[1]] <- locsm[[2]] <- locs

ben.mvm <-  new("MultivariateVecchiaModel", n_neighbors = 10)
ben.mvm <- prestogp_fit(ben.mvm, ym, Xm, locsm)


#Vecchia Model
ben.vm <- new("MultivariateVecchiaModel", n_neighbors = 10)
ben.vm2 <- prestogp_fit(ben.vm, y, X, locsm)



