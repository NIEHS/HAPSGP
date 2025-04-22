#library(dplyr)
#library(PrestoGP)
# nolint start
#setwd("/ddn/gs1/home/kassienma/HAPSGP/")
#pred_grid=readRDS("output/AGU/gridmet_coarsegrid_2021.rds")

#args <- commandArgs(trailingOnly = TRUE)
#day <- as.Date(args[1], format = "%Y-%m-%d")

#if (is.na(day)) stop("Invalid date provided!")

pgp_pred=function(pred_dates,fullmodel2,pred_grid,vars){
fullmodel=readRDS(fullmodel2)
#Extract fit data
model_fit=fullmodel$model
chemlist=fullmodel$chemlist

print(paste0("date= ",pred_dates))
#Load covariates
print("Loading Covariates...")
#covs_df=readRDS("input/covariates/gridmet/gridmet_all_df.RDS")
#covs_df=readRDS(pred_grid)
covs_df=as.data.frame(pred_grid)
print("Omitting NAs...")
covs_df<- na.omit(covs_df)
#covs_df<- df[!Reduce(`|`, lapply(covs_df[, cov_ind, drop = FALSE], is.na)), ]
#706947219
print("Filtering to day...")
covs_d=covs_df %>% dplyr::filter(as.Date(time) %in% pred_dates)%>% dplyr::mutate(time=as.numeric(as.Date(time)))

print(nrow(covs_d))

noncov_names=c(vars,"time","lon","lat","year","grid_id")
cov_ind <- which(!names(covs_d) %in% noncov_names)
loc_ind = c(which(colnames(covs_d)== "lon"), which(colnames(covs_d)== "lat"),which(colnames(covs_d)== "time"))

chemnum=length(chemlist)

Xm <- list()
locsm <- list()

print("Formatting prediction set...")
# Make prediction matrix
#X=as.matrix(covs_y[,cov_ind])
#locs2=as.matrix(covs_y[,loc_ind])
for(i in 1:chemnum){
X=as.matrix(covs_d[,cov_ind])
colnames(X)=paste0(colnames(X), chemlist[i])

colnames(X) <- gsub("TRI_PC0", "TRI_PC", colnames(X))
column_match=which(colnames(X) %in% colnames(model_fit@X_train))
X_filter=X[,column_match]

matched_indices <- (match(colnames(model_fit@X_train), colnames(X_filter)))
matched_indices <-matched_indices[!is.na(matched_indices)]

X_filter <- X_filter[, matched_indices]


locs2=as.matrix(covs_d[,loc_ind])

#for(i in 1:chemnum){
Xm[[i]] <- X_filter
locsm[[i]] <- locs2
}

print("Predicting...")
pred <- prestogp_predict(model=model_fit,X = Xm,locs = locsm)
#saveRDS(pred, paste0("output/AGU/pgpagu_prediction_",day,".RDS"))

# extract results for reformatting
plist=pred[[1]]
names(plist)=chemlist

# join predictions back with location data
pred_df=locs2
for(i in 1:chemnum){
cnames=c(colnames(pred_df),chemlist[i])    
pred_df=cbind(pred_df,as.data.frame(exp(plist[[i]])))
colnames(pred_df)=cnames   
}

#Append index number to dataframe for easy rejoining later
idx=c(1:nrow(pred_df))
pred_df=cbind(idx,pred_df)

return(pred_df)
}
#nolint end