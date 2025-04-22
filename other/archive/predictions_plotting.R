# Plotting Texas prediction results
library(targets)
library(dplyr)
library(sf)
library(RColorBrewer)
library(viridis)
library(data.table)
library(PrestoGP)


pred=tar_read("prediction_nc")
sites=tar_read("haps_locs_nc")

#Yearly avg
pred_avg= pred %>%
  group_by(lon,lat) %>%
  summarize(avg_Ben=mean(Benzene,na.rm=T),
            avg_Hex=mean(Hexane,na.rm=T))%>%
  ungroup()

#Plotting

pred_sf=st_as_sf(pred_avg,coords=c("lon","lat"), crs = 4326)
sites_sf=st_as_sf(sites,coords=c("lon","lat"), crs = 4326) %>% st_transform(5070)
plot(pred_sf, pch=15, cex=0.5, logz=TRUE) # Looks good


#library(map)
all_sf_al <- st_transform(pred_sf, 5070)

all_sf_lc <- sf::st_transform(pred_sf, "+proj=aea +lat_1=29.5
    +lat_2=45.5 +lat_0=23
    +lon_0=-96 +x_0=0 +y_0=0
    +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

plot(all_sf_al[2],pch=15, cex=3)#,breaks=mybreaks,pal = viridis)
plot(all_sf_lc[2],pch=19, cex=3)#,breaks=mybreaks,pal = viridis)
plot(pred_sf[1],pch=15, cex=3)#,breaks=mybreaks,pal = viridis)

#Benzene
mybreaks=seq(0, 5,length.out=11)
plot(st_geometry(sites_sf))
plot(all_sf_al[1],pch=15, cex=2.5,breaks=mybreaks,pal = viridis, main="Benzene",add=TRUE)
plot(st_geometry(sites_sf), col = "red", pch = 16, cex = 1.5, add = TRUE)

plot(all_sf_al[1],pch=15, cex=2.5,breaks=mybreaks,pal = viridis, main="Benzene")

#Hexane
mybreaks=seq(0, 5,length.out=11)
plot(st_geometry(sites_sf))
plot(all_sf_al[2],pch=15, cex=2.5,breaks=mybreaks,pal = viridis, main="Hexane",add = TRUE)
plot(st_geometry(sites_sf), col = "red", pch = 16, cex = 1.5, add = TRUE)
plot(all_sf_al[2],pch=15, cex=2.5)#,breaks=mybreaks,pal = viridis, main="Hexane")


# Plot covariates
covs=tar_read(covs_pctri_pred)

vars=tar_read(vars)
noncov_names=c(vars,"time","lon","lat","year")
covariate_cols <- setdiff(names(covs), noncov_names)

# Compute the mean over time for each site
covs_avg <- covs[, lapply(.SD, mean, na.rm = TRUE), by = .(AMA_SITE_CODE, lon, lat), .SDcols = covariate_cols]

# Filter covariates selected by the model
fit=tar_read("model_fit_nc")
model=fit$model

Xnames=gsub("TRI_PC0", "TRI_PC",colnames(model@X_train))
Xnames=gsub("Benzene", "", Xnames)
Xnames=gsub("Hexane", "", Xnames)
Xnames=unique(Xnames)
#Xnames <- gsub("TRI_PC0", "TRI_PC00", Xnames)

cnames=c("AMA_SITE_CODE", "lon", "lat",Xnames)
cnames=cnames[1:length(cnames)-1] #delete last empty column

colnames(covs_avg)=gsub("TRI_PC0", "TRI_PC",colnames(covs_avg)) #Delete first trailing 0
colnames(covs_avg)=gsub("TRI_PC0", "TRI_PC",colnames(covs_avg)) #Delete second trailing 0

covs_avg2 <- covs_avg[, ..cnames]

#Reorder land use names
lu_ind=c(11:58)

# Identify column names in positions 11 to 58
cols_to_sort <- names(covs_avg2)[11:58]

# Sort them alphabetically
sorted_cols <- sort(cols_to_sort)

# Reconstruct the data.table with reordered columns
covs_avg2 <- covs_avg2[, c(names(covs_avg2)[1:10], sorted_cols, names(covs_avg2)[59:ncol(covs_avg2)]), with = FALSE]

covs_sf=st_as_sf(covs_avg2,coords=c("lon","lat"), crs = 4326) %>% st_transform(5070)

plot(covs_sf[,2:8],pch=15) #Meteo covariates
plot(covs_sf[,9:32],pch=15,max.plot=25) #Land use
plot(covs_sf[,33:56],pch=15,max.plot=24) #Land use 2
plot(covs_sf[,57:80],pch=15,max.plot=30) #TRI
plot(covs_sf[,81:102],pch=15,max.plot=30) #TRI 2


library(mapview)
mapview(dt_avg, zcol = names(dt_avg)[2:8])


########### Plot nice, code from ChatGPT ######
# Load required libraries
library(sf)
library(ggplot2)
library(ggspatial)
library(scales)
library(viridis)

# Read in the spatial data
gridded_data <- all_sf_al    # Replace with actual file name
site_locations <- sites_sf # Replace with actual file name

texas_boundary <- tigris::states(cb = TRUE, resolution = "20m", class = "sf") %>%
  filter(NAME %in% "Texas")
texas_boundary <- st_transform(texas_boundary, 5070)

gridded_data <- st_transform(gridded_data, crs = 5070)
site_locations <- st_transform(site_locations, crs = 5070)
texas_boundary <- st_transform(texas_boundary, crs = 5070)

ggplot() +
  # Plot gridded avg_Ben as squares
  geom_sf(data = gridded_data, aes(color = avg_Ben), shape = 15, size = 3) +
  scale_color_viridis_c(name = "log10", trans = "log10") +
  # Overlay site locations as hollow circles
  geom_sf(data = site_locations, color = "black", shape = 1, size = 2, stroke = 0.5) +
  # Add Texas boundary outline
  geom_sf(data = texas_boundary, color = "black", fill = NA, linewidth = 0.5) +
  theme_minimal() +
  labs(title = "Mean Benzene 2021")

ggplot() +
  # Plot gridded avg_Ben as squares
  geom_sf(data = gridded_data, aes(color = avg_Hex), shape = 15, size = 3) +
  scale_color_viridis_c(name = "log10", trans = "log10") +
  # Overlay site locations as hollow circles
  geom_sf(data = site_locations, color = "black", shape = 1, size = 2, stroke = 0.5) +
  # Add Texas boundary outline
  geom_sf(data = texas_boundary, color = "black", fill = NA, linewidth = 0.5) +
  theme_minimal() +
  labs(title = "Mean Hexane 2021")
