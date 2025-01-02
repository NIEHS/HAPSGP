#library(dplyr)
#library(PrestoGP)

#setwd("/ddn/gs1/home/kassienma/HAPSGP/")
#pred_grid=readRDS("output/AGU/gridmet_coarsegrid_2021.rds")

#args <- commandArgs(trailingOnly = TRUE)
#day <- as.Date(args[1], format = "%Y-%m-%d")

#if (is.na(day)) stop("Invalid date provided!")

pgp_pred=function(pred_dates,model,pred_grid){
#Load full model results
fullmodel=readRDS(model)

#Extract fit data
model_fit=fullmodel$model

#Make dates vector
dates_vec=seq(as.Date(pred_dates[1]), as.Date(pred_dates[2]), "days")

#Load covariates
print("Loading Covariates...")
#covs_df=readRDS("input/covariates/gridmet/gridmet_all_df.RDS")
covs_df=readRDS(pred_grid)
print("Omitting NAs...")
covs_df<- na.omit(covs_df)
#covs_df<- df[!Reduce(`|`, lapply(covs_df[, cov_ind, drop = FALSE], is.na)), ]
#706947219
print("Filtering to one year...")
covs_y=covs_df %>% filter(as.Date(time) %in% dates_vec)%>% mutate(time=as.numeric(time))

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
pred <- prestogp_predict(model=model.fit,X = Xm,locs = locsm)

saveRDS(pred, paste0("output/AGU/pgpagu_prediction_",day,".RDS"))
return(pred)
}
