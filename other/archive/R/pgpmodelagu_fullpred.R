library(dplyr)
library(PrestoGP)

setwd("/ddn/gs1/home/kassienma/HAPSGP/")

args <- commandArgs(trailingOnly = TRUE)
day <- as.Date(args[1], format = "%Y-%m-%d")

if (is.na(day)) stop("Invalid date provided!")

dates=c("2021-01-01","2021-12-31")
print(dates)

# Proceed with predictions for pred_date
# ...

#Load CV fit
#results=readRDS(paste0("output/AGU/pgpagu_cvfit_",dates[1],"_",dates[2],".RDS"))

#results$dates_vec -> dates_vec
#results$otr->otr
#results$otst->otst
#results$ytest.list->ytest.list
#results$model.list->model.list
#results$pred.list->pred.list
#results$mse.list->mse.list

#Load full model results
fullmodel=readRDS(paste0("output/AGU/pgpagu_fulldata_",dates[1],"_",dates[2],".RDS"))

fullmodel$dates_vec->dates_vec2
fullmodel$model->all.mvm2

all.mvm2

#Load covariates
print("Loading Covariates...")
#covs_df=readRDS("input/covariates/gridmet/gridmet_all_df.RDS")
covs_df=readRDS("output/AGU/gridmet_coarsegrid_2021.rds")
print("Omitting NAs...")
covs_df<- na.omit(covs_df)
#covs_df<- df[!Reduce(`|`, lapply(covs_df[, cov_ind, drop = FALSE], is.na)), ]
#706947219
print("Filtering to one year...")
covs_y=covs_df %>% filter(as.Date(time) %in% dates_vec2)%>% mutate(time=as.numeric(time))

#day=as.numeric(as.Date("2021-07-01"))
covs_d=covs_y %>% filter(time == as.numeric(day)) 

chemnum=4
loc_ind=1:3
cov_ind=4:10

Xm <- list()
locsm <- list()

print("Formatting prediction set...")
# Make prediction matrix
#X=as.matrix(covs_y[,cov_ind])
#locs2=as.matrix(covs_y[,loc_ind])

X=as.matrix(covs_d[,cov_ind])
locs2=as.matrix(covs_d[,loc_ind])

for(i in 1:chemnum){
Xm[[i]] <- X
locsm[[i]] <- locs2
}

print("Predicting...")
pred <- prestogp_predict(model=all.mvm2,X = Xm,locs = locsm)

saveRDS(pred, paste0("output/AGU/pgpagu_prediction_",day,".RDS"))

#ptest=readRDS(paste0("output/AGU/pgpagu_prediction_",dates[1],"_",dates[2],".RDS"))

#ptest=ptest[[1]]

#dim(ptest[[1]])
