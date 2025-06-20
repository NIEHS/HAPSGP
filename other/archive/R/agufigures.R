# AGU figures

#1- Plot pollutants data
library(ggplot2)
library(ggridges)
library(dplyr)
library(scales)

dates = c("2021-01-01", "2021-12-31")
print(dates)
dates_vec = seq(as.Date(dates[1]), as.Date(dates[2]), "days")

# Read in HAPS
df = readRDS("output/AGU/haps_gridmet_208-2021.rds")

# remove rows with measurement=0 (will change this later)
df = df %>% filter(CONC_DAILY_UG_M3 < 100)
# remove rows with NA's in MDL (may change this later)
mdl_ind = which(colnames(df) == "MDL_DAILY_STD_UG_M3")
df <- df[!Reduce(`|`, lapply(df[, mdl_ind, drop = FALSE], is.na)), ]

# Select only one instance in case of repeat site-times with different duration_desc
df = df %>%
  group_by(AQS_PARAMETER_CODE, AMA_SITE_CODE, time) %>% # group by pollutant/site/year/duration (across days)
  filter(DURATION_DESC == min(DURATION_DESC)) %>%
  ungroup()

df2 = df %>%
  filter(time %in% dates_vec) %>%
  #convert days to time
  mutate(time = as.numeric(as.Date(time)))

# Create left_censored values
df2 = df2 %>%
  mutate(
    left_censored = CONC_DAILY_UG_M3 == 0,
    below_mdl = CONC_DAILY_UG_M3 != 0 & CONC_DAILY_UG_M3 < MDL_DAILY_STD_UG_M3
  )

###################
# Statistics table
##################
### Statistics of dates available per location
df_date_stats <- df2 %>%
  dplyr::group_by(AQS_PARAMETER_NAME, lat, lon) %>%
  dplyr::summarise(distinct_dates = n_distinct(time)) %>%
  dplyr::group_by(AQS_PARAMETER_NAME) %>%
  dplyr::summarise(
    max_DatesPerSite = max(distinct_dates),
    min_DatesPerSite = min(distinct_dates),
    mean_DatesPerSite = mean(distinct_dates)
  )
### All other statistics, join dates statistics at the end
df_stats <- df2 %>%
  dplyr::group_by(AQS_PARAMETER_NAME) %>%
  dplyr::summarise(
    Distinct_Locations = n_distinct(paste(lat, lon)),
    NonDetects = 100 * sum(left_censored) / length(CONC_DAILY_UG_M3),
    BelowMDL = 100 * sum(below_mdl) / length(CONC_DAILY_UG_M3),
    AboveMDL = 100 *
      (sum(!below_mdl & !left_censored)) /
      length(CONC_DAILY_UG_M3),
    MeanMDL = mean(MDL_DAILY_STD_UG_M3, na.rm = T)
  ) %>%
  ungroup() %>%
  left_join(df_date_stats, by = "AQS_PARAMETER_NAME")

write.csv(df_stats, "haps_stats_agu.csv")

############
# Plot MDL values
############
# Create left_censored values
library(tidyr) # for pivot_longer function

# Convert to long format
df_long <- df2 %>%
  pivot_longer(
    cols = c(MDL_DAILY_STD_UG_M3, CONC_DAILY_UG_M3),
    names_to = "Type",
    values_to = "Value"
  ) %>%
  mutate(
    Type = factor(
      Type,
      levels = c("MDL_DAILY_STD_UG_M3", "CONC_DAILY_UG_M3"),
      labels = c("MDL", "Concentration")
    )
  )

ggplot(
  df_long,
  aes(x = Value, y = AQS_PARAMETER_NAME, fill = Type),
  rel_min_height = 0.001
) +
  geom_density_ridges(alpha = 0.7, scale = 1) + # Alpha for transparency, scale for overlap
  labs(
    title = "Ridgeline Plot of Chemical Densities",
    x = "Value",
    y = "Chemical",
    fill = "Type"
  ) +
  scale_fill_manual(values = c("pink", "red")) + # Custom colors for clarity
  scale_x_continuous(limits = c(-0.2, 1.5)) +
  theme_ridges() + # Cleaner ridgeline theme
  theme(axis.text.y = element_text(size = 8)) # Adjust text size for readability

###############
# Map sites
##############
library(ggplot2)
library(dplyr)
library(sf)
library(tidyr)
library(maps)


chemlist = sort(unique(df2$AQS_PARAMETER_NAME))
#i=1
cols = c("red", "darkorange", "darkgreen", "blue")
chemical_data = list()
for (i in 1:length(chemlist)) {
  chemical_data[[i]] <- df %>%
    filter(AQS_PARAMETER_NAME == chemlist[i], lon < 0) %>% # Replace "ChemicalName" with the desired chemical
    distinct(AMA_SITE_CODE, lon, lat) # Remove duplicate site locations
}
# Load the US map (you can use maps::map() or usmap package for more options)
us_map <- st_as_sf(map("state", fill = TRUE, plot = FALSE))

# Create the plot
ggplot() +
  # Plot the US map
  geom_sf(data = us_map, fill = "lightgray", color = "white") +
  # Plot the unique site locations for the specific chemical
  geom_point(
    data = chemical_data[[1]],
    aes(x = lon, y = lat),
    color = cols[1],
    alpha = 0.5
  ) +
  labs(title = paste(chemlist[1], "2021 sites")) + # Customize the title
  theme_minimal() +
  theme(legend.position = "none")
ggplot() +
  # Plot the US map
  geom_sf(data = us_map, fill = "lightgray", color = "white") +
  # Plot the unique site locations for the specific chemical
  geom_point(
    data = chemical_data[[2]],
    aes(x = lon, y = lat),
    color = cols[2],
    alpha = 0.5
  ) +
  labs(title = paste(chemlist[2], "2021 sites")) + # Customize the title
  theme_minimal() +
  theme(legend.position = "none")
ggplot() +
  # Plot the US map
  geom_sf(data = us_map, fill = "lightgray", color = "white") +
  # Plot the unique site locations for the specific chemical
  geom_point(
    data = chemical_data[[3]],
    aes(x = lon, y = lat),
    color = cols[3],
    alpha = 0.5
  ) +
  labs(title = paste(chemlist[3], "2021 sites")) + # Customize the title
  theme_minimal() +
  theme(legend.position = "none")
ggplot() +
  # Plot the US map
  geom_sf(data = us_map, fill = "lightgray", color = "white") +
  # Plot the unique site locations for the specific chemical
  geom_point(
    data = chemical_data[[4]],
    aes(x = lon, y = lat),
    color = cols[4],
    alpha = 0.5
  ) +
  labs(title = paste(chemlist[4], "2021 sites")) + # Customize the title
  theme_minimal() +
  theme(legend.position = "none")
