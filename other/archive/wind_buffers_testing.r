# Test windrose frequency analysis
library(amadeus)
library(targets)
library(sf)
library(mapview)
library(terra)
library(dplyr)

source("R/windfreq.R")
source("R/get_bearing.R")
# 1. Import one HAPS site and nearing TRI sites
# Import all of texas and use mapview to select a good candidate
locs=tar_read("haps_locs_nc")
locs_sf=st_as_sf(locs, coords=c("lon","lat"),crs=4326) %>% st_transform(4326)
#mapView(locs_sf)

input_dir="/ddn/gs1/group/set/Projects/NRT-AP-Model/input/tri"

states_sel <- tigris::states(cb = TRUE, resolution = "20m", class = "sf") %>%
  dplyr::filter(NAME %in% "Texas")

states_sel <- st_transform(states_sel, 4326) 

ptri=process_tri(
  path=input_dir,
  year=2021,
  variables = c(1, 13, 12, 14, 20, 34, 36, 47, 48, 49),
  extent = terra::ext(states_sel)
)
data <- as.data.frame(ptri)

tri_sites <- unique(data[, c("LONGITUDE", "LATITUDE")])
tri_sf <- st_as_sf(tri_sites, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
mapview(tri_sf, col.regions="red") +
mapview(locs_sf)

# Extract site near Abilene 484411509 
site_tx=locs %>% dplyr::filter(AMA_SITE_CODE=="484411509")
#mapview(site_tx, col.regions="green")

# 2. Get wind frequency bins for this site
# Import wind data
covs=tar_read(covariates_nc)
covs_tx=covs%>% dplyr::filter(AMA_SITE_CODE=="484411509")


library(openair)
colnames(covs_tx)[colnames(covs_tx)=="vs_0"]="ws"
colnames(covs_tx)[colnames(covs_tx)=="th_0"]="wd"

# Wind frequency
wf=windfreq(covs_tx, ws_col="ws", wd_col="wd", ws.int=max(covs_tx$ws), wd.int=4, calm.thres = 0, 
                     statistic = "fraction", format = "data.frame")

# Use openair too to get a visual of the bins
pf=polarFreq(covs_tx,ws.int=8.6,wd.nint=4)$data # They match well with my windfreq

# 3. Get bearings
# Make sure the bearing angles and the wind angles match right - may have to invert
  # Wind angles are FROM downwind TO site, so wd of 360 means wind blowing FROM the North

# Get closest TRI sites from Abilene
  from = ptri 
  locs=site_tx
  locs_id = "AMA_SITE_CODE"
  radius = 2e4L
  geom = FALSE
  sedc_bandwidth= radius

  #target_fields = tri_cols
  # Here I ran parts of the calc_tri and sum_edc functions to extract the nearby sites
       if (!methods::is(locs, "SpatVector")) {
    if (methods::is(locs, "sf")) {
      locs <- terra::vect(locs)
    }
  }
     
      locs_re <- terra::project(locs, terra::crs(from))
      # split by year: locs and tri locations
      tri_cols <- grep("_AIR", names(from), value = TRUE)
      # error fix: no whitespace
      tri_cols <- sub(" ", "_", tri_cols)
      # inner lapply
      list_radius <- split(radius, radius)
      locs = locs_re
      target_fields = tri_cols

      len_point_locs <- seq_len(nrow(locs))

          locs$from_id <- len_point_locs
          locs_buf <-
            terra::buffer(
              locs,
              width = sedc_bandwidth * 2,
              quadsegs = 90
            )

          from_in <- from[locs_buf, ]
          len_point_from <- seq_len(nrow(from_in))

          # len point from? len point to?
          from_in$to_id <- len_point_from
          dist <- NULL

          # near features with distance argument: only returns integer indices
          # threshold is set to the twice of sedc_bandwidth
          res_nearby <-
            terra::nearby(locs, from_in, distance = sedc_bandwidth * 2)
          # attaching actual distance
          dist_nearby <- terra::distance(locs, from_in)
          dist_nearby_df <- as.vector(dist_nearby)
          # adding integer indices
          dist_nearby_tdf <-
            expand.grid(
              from_id = len_point_locs,
              to_id = len_point_from
            )
          dist_nearby_df <- cbind(dist_nearby_tdf, dist = dist_nearby_df)

          # summary
          res_sedc <- res_nearby |>
            dplyr::as_tibble() |>
            dplyr::left_join(data.frame(locs)) |>
            dplyr::left_join(data.frame(from_in)) |>
            dplyr::left_join(dist_nearby_df)

  # Plot sites
  df=res_sedc %>% dplyr::select(c(colnames(res_sedc)[1:8],"dist"))
  df$bearing <- mapply(get_bearing, df$lon, df$lat, df$LONGITUDE, df$LATITUDE)
  sedc_sf <- st_as_sf(df, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  mapview(sedc_sf, col.regions="red") +
  mapview(site_tx)

# Assign bins
# 1. Extract the bin intervals from the wf$wd_bin labels
wd_intervals <- extract_intervals(wf$wd_bin)

# 2. Ensure proper wrap-around logic for bearings (0-360 degrees)
# Adjust the last interval to account for the circular nature
wd_intervals[nrow(wd_intervals), 2] <- 360

# 4. Apply the matching function to the bearing column
df$freq <- sapply(df$bearing, match_bin, 
                          wd_intervals = wd_intervals, 
                          freq_values = wf$freq)

# 4. Calculate normal SEDC
res_sedc <- res_nearby |>
      dplyr::as_tibble() |>
      dplyr::left_join(data.frame(locs)) |>
      dplyr::left_join(data.frame(from_in)) |>
      dplyr::left_join(dist_nearby_df) |>
      # per the definition in
      # https://mserre.sph.unc.edu/BMElab_web/SEDCtutorial/index.html
      # exp(-3) is about 0.05 * (value at origin)
      dplyr::mutate(w_sedc = exp((-3 * dist) / sedc_bandwidth)) |>
      dplyr::group_by(!!rlang::sym(locs_id)) |>
      dplyr::summarize(
        dplyr::across(
          dplyr::all_of(target_fields),
          ~sum(w_sedc * ., na.rm = TRUE)
        )
      ) |>
      dplyr::ungroup()
    idx_air <- grep("_AIR_", names(res_sedc))
    names(res_sedc)[idx_air] <-
      sprintf("%s_%05d", names(res_sedc)[idx_air], sedc_bandwidth)

    if (geom %in% c("sf", "terra")) {
      res_sedc <- merge(
        terra::as.data.frame(locs, geom = "WKT")[, c("site_id", "geometry")],
        res_sedc,
        "site_id"
      )
    }

    res_sedc_return <- calc_return_locs(
      covar = res_sedc,
      POSIXt = TRUE,
      geom = geom,
      crs = terra::crs(from)
    )

    attr(res_sedc_return, "sedc_bandwidth") <- sedc_bandwidth
    attr(res_sedc_return, "sedc_threshold") <- sedc_bandwidth * 2


# 5. Calculate SEDC by wind bins
res_sedc <- res_nearby |>
      dplyr::as_tibble() |>
      dplyr::left_join(data.frame(locs)) |>
      dplyr::left_join(data.frame(from_in)) |>
      dplyr::left_join(dist_nearby_df)
res_sedc$bearing <- mapply(get_bearing, res_sedc$lon, res_sedc$lat, res_sedc$LONGITUDE, res_sedc$LATITUDE)
res_sedc$freq <- sapply(res_sedc$bearing, match_bin, 
                          wd_intervals = wd_intervals, 
                          freq_values = wf$freq) 

res_sedc<-res_sedc|>
dplyr::mutate(w_sedc = freq * exp((-3 * dist) / sedc_bandwidth)) |>
      dplyr::group_by(!!rlang::sym(locs_id)) |>
      dplyr::summarize(
        dplyr::across(
          dplyr::all_of(target_fields),
          ~sum(w_sedc * ., na.rm = TRUE)
        )
      ) |>
      dplyr::ungroup()

idx_air <- grep("_AIR_", names(res_sedc))
    names(res_sedc)[idx_air] <-
      sprintf("%s_%05d", names(res_sedc)[idx_air], sedc_bandwidth)

    if (geom %in% c("sf", "terra")) {
      res_sedc <- merge(
        terra::as.data.frame(locs, geom = "WKT")[, c("site_id", "geometry")],
        res_sedc,
        "site_id"
      )
    }

    res_sedc_return2 <- calc_return_locs(
      covar = res_sedc,
      POSIXt = TRUE,
      geom = geom,
      crs = terra::crs(from)
    )
    attr(res_sedc_return2, "sedc_bandwidth") <- sedc_bandwidth
    attr(res_sedc_return2, "sedc_threshold") <- sedc_bandwidth * 2

#Sum(sedc$freq >1 , does it makes sense? Do I have to re-normalize per bin?)

# Compare
  #Remove all non-zero columns
  res_sedc_return = res_sedc_return[, colSums(res_sedc_return != 0) > 0]
  res_sedc_return2 = res_sedc_return2[, colSums(res_sedc_return2 != 0) > 0]

plot(unlist(as.vector(res_sedc_return[,2:13])),
unlist(as.vector(res_sedc_return2[,2:13])))

tri_df=data.frame(tri1=unlist(as.vector(res_sedc_return[,2:13])),
tri2=unlist(as.vector(res_sedc_return2[,2:13])))
