# Get all covariate predictions together
setwd("/ddn/gs1/home/kassienma/HAPSGP/")
library(dplyr)
library(sf)

dates=c("2021-01-01","2021-12-31")
print(dates)

fullmodel=readRDS(paste0("output/AGU/pgpagu_fulldata_",dates[1],"_",dates[2],".RDS"))
fullmodel=readRDS(paste0("output/AGU/pgpagu_fulldata_",dates[1],"_",dates[2],".RDS"))

fullmodel$dates_vec->dates_vec
fullmodel$model->all.mvm2

all.mvm2

chemlist=c("Benzene","Carbon tetrachloride","Chloroform","Hexane")
#Load covariates
print("Loading Covariates...")
#covs_df=readRDS("input/covariates/gridmet/gridmet_all_df.RDS")
covs_df=readRDS("output/AGU/gridmet_coarsegrid_2021.rds")
print("Omitting NAs...")
covs_df<- na.omit(covs_df)
#covs_df<- df[!Reduce(`|`, lapply(covs_df[, cov_ind, drop = FALSE], is.na)), ]
#706947219
print("Filtering to one year...")
covs_y=covs_df %>% filter(as.Date(time) %in% dates_vec)%>% mutate(time=as.numeric(time))

# Set up names and indeces
chemnum=length(chemlist)
loc_ind=2:4
cov_ind=5:11

# Check covariate yearly averages
covs_yavg=covs_y%>%
  group_by(lon,lat) %>%
  summarise(across(all_of(colnames(covs_y)[cov_ind]), list(mean = ~mean(.x, na.rm = TRUE))),
            .groups = "drop" # Ungroup the result if grouping is not needed after summarization
  )
# Plot covariates

covs_sf=st_as_sf(covs_yavg,coords=c("lon","lat"), crs = 4326)
plot(covs_sf, pch=15, cex=0.5) # Looks good

for(d in 1:length(dates_vec)){
  #for(d in 1:2){
  day=dates_vec[d]
  print(day)
  covs_d=covs_y %>% filter(time == as.numeric(day))

  Xm <- list()
  locsm <- list()

  X=as.matrix(covs_d[,cov_ind])
  locs2=as.matrix(covs_d[,loc_ind])

  for(i in 1:chemnum){
    Xm[[i]] <- X
    locsm[[i]] <- locs2
  }

  locs=rbind(locsm[[1]],locsm[[2]],locsm[[3]],locsm[[4]])
  pred=readRDS(paste0("output/AGU/pgpagu_prediction_",day,".RDS"))[[1]]

  locspred=cbind(locs,exp(pred))

  idx=seq(1,by=nrow(locs2),length=chemnum+1)

  predlist = list()
  for(j in 1:chemnum){
    ind=idx[j]:(idx[j+1]-1)
    predlist[[j]]=locspred[ind,]
  } #J chemicals
  names(predlist)=chemlist

  #Append all days per chemical
  if(d==1){
    predlist_year=predlist
  }else{
    for(k in 1:chemnum){
      predlist_year[[k]]=rbind(predlist_year[[k]],predlist[[k]])
    }#k
  }#ifelse

}# day

p_bz=as.data.frame(predlist_year$Benzene)
p_ct=as.data.frame(predlist_year$`Carbon tetrachloride`)
p_ch=as.data.frame(predlist_year$Chloroform)
p_hx=as.data.frame(predlist_year$Hexane)

#Join all together
all_p=cbind(p_bz,p_ct[,4],p_ch[,4],p_hx[,4])
colnames(all_p)=c("time","lon","lat",chemlist)

all_y <- all_p %>%
  group_by(lon, lat) %>%
  summarise(
    across(
      all_of(colnames(all_p)[4:7]),
      list(mean = ~mean(.x[.x > 0], na.rm = TRUE)) # Filter values > 0 before computing the mean
    ),
    .groups = "drop" # Ungroup the result if grouping is not needed after summarization
  )

all_sf=st_as_sf(all_y,coords=c("lon","lat"), crs = 4326)
plot(all_sf, pch=15, cex=0.5) # Looks good

plot((p_bz$s1[p_bz$s1>0]))
plot(all_y$Benzene_mean)

y_ct= p_ct %>%
  dplyr::group_by(lon,lat) %>%
  dplyr::summarize(avg=mean(s1,na.rm=T),
                   var=var(s1,na.rm=T))%>%
  ungroup()

# Yearly averages
 y_bz= p_bz %>%
  mutate(s1= s1[s1<=0] = NA) %>%
  group_by(lon,lat) %>%
  summarize(avg=mean(s1,na.rm=T))%>%
  ungroup()
y_ct= p_ct %>%
  dplyr::group_by(lon,lat) %>%
  filter(s1>0)%>%
  dplyr::summarize(avg=mean(s1,na.rm=T))%>%
  ungroup()
y_ch= p_ch %>%
  dplyr::group_by(lon,lat) %>%
  filter(s1>0)%>%
  dplyr::summarize(avg=mean(s1,na.rm=T))%>%
  ungroup()
y_hx= p_hx %>%
  dplyr::group_by(lon,lat) %>%
  filter(s1>0)%>%
  dplyr::summarize(avg=mean(s1,na.rm=T))%>%
  ungroup()

#Join all together
all_y=cbind(y_bz,y_ct[,3],y_ch[,3],y_hx[,3])
colnames(all_y)=c("lon","lat",chemlist)

saveRDS(all_y, "prediction_avg_2021_agu.RDS")
###########
# Plotting
###########
all_y=readRDS("prediction_avg_2021_agu.RDS")
all_sf=st_as_sf(all_y,coords=c("lon","lat"), crs = 4326)
plot(all_sf, pch=15, cex=0.5, logz=TRUE) # Looks good

library(RColorBrewer)
library(viridis)
library(map)

all_sf_lc <- sf::st_transform(all_sf, "+proj=aea +lat_1=29.5
    +lat_2=45.5 +lat_0=23
    +lon_0=-96 +x_0=0 +y_0=0
    +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

plot(all_sf_lc[1],pch=19, cex=0.5,breaks=mybreaks,pal = viridis)
plot(all_sf[1],pch=15, cex=0.3,breaks=mybreaks,pal = viridis)

mybreaks=seq(0, 0.002,length.out=11)
plot(st_geometry(us_shapefile), col = NA, border = "white")
plot(all_sf[1],pch=15, cex=1,breaks=mybreaks,pal = viridis, add=T)
plot(st_geometry(us_shapefile), col = NA, border = "white", add=T)
plot(all_sf[1],pch=15, cex=1,breaks=mybreaks,pal = viridis)

mybreaks2=seq(0, 0.6,length.out=11)
plot(st_geometry(us_shapefile), col = NA, border = "white")
plot(all_sf[2],pch=15, cex=1,breaks=mybreaks2,pal = viridis, add=T)
plot(st_geometry(us_shapefile), col = NA, border = "white", add=T)
plot(all_sf[2],pch=15, cex=1,breaks=mybreaks2,pal = viridis)


mybreaks3=seq(0, 0.0003,length.out=11)
plot(st_geometry(us_shapefile), col = NA, border = "white")
plot(all_sf[3],pch=15, cex=1,breaks=mybreaks3,pal = viridis, add=T)
plot(st_geometry(us_shapefile), col = NA, border = "white", add=T)
plot(all_sf[3],pch=15, cex=1,breaks=mybreaks3,pal = viridis)

mybreaks4=seq(0, 0.2,length.out=11)
plot(st_geometry(us_shapefile), col = NA, border = "white")
plot(all_sf[4],pch=15, cex=1,breaks=mybreaks4,pal = viridis, add=T)
plot(st_geometry(us_shapefile), col = NA, border = "white", add=T)
plot(all_sf[4],pch=15, cex=1,breaks=mybreaks4,pal = viridis)

