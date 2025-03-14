# Map CV fold selection
library(targets)
data=as.data.frame(tar_read(covs_pctri_nc))
pgp_crossvalidation_nc=tar_read(pgp_crossvalidation_nc)
otst=pgp_crossvalidation_nc$otst
otr=pgp_crossvalidation_nc$otr
state_list=c("Texas")

idx=otst[[1]]
data_cv1=data[idx,]
sites_cv1=data_cv1 %>% distinct(AMA_SITE_CODE, .keep_all = TRUE)

idx=otst[[2]]
data_cv2=data[idx,]
sites_cv2=data_cv2 %>% distinct(AMA_SITE_CODE, .keep_all = TRUE)

idx=otst[[3]]
data_cv3=data[idx,]
sites_cv3=data_cv3 %>% distinct(AMA_SITE_CODE, .keep_all = TRUE)

idx=otst[[4]]
data_cv4=data[idx,]
sites_cv4=data_cv4 %>% distinct(AMA_SITE_CODE, .keep_all = TRUE)

idx=otst[[5]]
data_cv5=data[idx,]
sites_cv5=data_cv5 %>% distinct(AMA_SITE_CODE, .keep_all = TRUE)


states_sel <- tigris::states(cb = TRUE, resolution = "20m", class = "sf") %>%
  filter(NAME %in% state_list)

library(ggplot2)
ggplot() +
  geom_sf(data = states_sel, fill = "lightgray", color = "black") +  # Texas background
  geom_point(data = sites_cv1, aes(x = lon, y = lat), color = "red", size = 5) +
  geom_point(data = sites_cv2, aes(x = lon, y = lat), color = "orange", size = 5) +
  geom_point(data = sites_cv3, aes(x = lon, y = lat), color = "yellow", size = 5) +
  geom_point(data = sites_cv4, aes(x = lon, y = lat), color = "purple", size = 5) +
  geom_point(data = sites_cv5, aes(x = lon, y = lat), color = "magenta", size = 5) +  # Plot sites
  theme_minimal() +
  labs(title = "Site Locations in Texas", x = "Longitude", y = "Latitude")


# calculate RMSE then divide by the mean of the fold
library(Metrics)

ytest=pgp_crossvalidation_nc$ytest.list
pred=pgp_crossvalidation_nc$pred.list

i=2
model.nrmse=list()
for(i in 1:5){
model.nrmse[i]=rmse(exp(unlist(ytest[[i]])),exp(unlist(pred[[i]])))/mean(exp(unlist(ytest[[i]])))
}

nrmse1=list()
nrmse2=list()
for(i in 1:5){
# Plot pred vs ytest
ytest_f=ytest[[i]]
pred_f=pred[[i]]

metric_df=data.frame(ytest=ytest_f[[1]],pred=pred_f[1:length(ytest_f[[1]])])
nrmse1[i]=rmse(exp(metric_df$ytest),exp(metric_df$pred))/mean(exp(metric_df$ytest))

metric_df2=data.frame(ytest=ytest_f[[2]],pred=pred_f[(length(ytest_f[[1]])+1):(length(ytest_f[[1]])+length(ytest_f[[2]]))])
nrmse2[i]=rmse(exp(metric_df2$ytest),exp(metric_df2$pred))/mean(exp(metric_df2$ytest))
}#


plot(ytest_f[[1]],pred_f[1:length(ytest_f[[1]])], main=paste("Benzene CV fold",i))
abline(c(0,1),col="red")
plot(ytest_f[[2]],pred_f[(length(ytest_f[[1]])+1):(length(ytest_f[[1]])+length(ytest_f[[2]]))],
main=paste("Benzene CV fold",i))
abline(c(0,1),col="red")
}

length(unlist(ytest_f))
length(unlist(pred_f))
plot(exp(unlist(ytest_f)),exp(pred_f))

plot(exp(unlist(ytest[[i]])),exp(pred[[i]]))
abline(c(0,1), col="red")
