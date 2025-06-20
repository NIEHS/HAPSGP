# Haps variograms
# Load libraries
library(dplyr)
library(tidyr)
library(geosphere)
library(gstat)
library(sp)
library(ggplot2)
library(gridExtra)

df = as.data.frame(tar_read(pgp_cleanup_nc))
df = as.data.frame(tar_read(process))


# Parameters
bin_width <- 100 # km
max_distance <- 1000 # max lag distance (km)

# Summarize concentration per site and chemical
site_summary <- df %>%
  group_by(AQS_PARAMETER_NAME, AMA_SITE_CODE, lon, lat) %>%
  summarise(mean_conc = mean(CONC_DAILY_STD, na.rm = TRUE), .groups = "drop")

# Function to compute variogram and generate both plots
compute_variogram_plots <- function(
  chemical_name,
  data,
  bin_width,
  max_distance
) {
  chem_data <- data %>% filter(AQS_PARAMETER_NAME == chemical_name)

  # Spatial conversion
  coordinates(chem_data) <- ~ lon + lat
  proj4string(chem_data) <- CRS("+proj=longlat +datum=WGS84")

  g <- gstat(id = "mean_conc", formula = mean_conc ~ 1, data = chem_data)

  # Compute variogram with custom binning
  v <- variogram(g, width = bin_width, cutoff = max_distance)
  v$chemical <- chemical_name
  total_var <- var(chem_data$mean_conc, na.rm = TRUE)
  v$covariance <- total_var - v$gamma

  # Semivariance plot
  vario_plot <- ggplot(v, aes(x = dist, y = gamma)) +
    geom_point() +
    geom_line(
      stat = "smooth",
      method = "loess",
      span = 0.3,
      se = FALSE,
      color = "blue"
    ) +
    labs(
      title = paste("Empirical Variogram -", chemical_name),
      x = "Distance (km)",
      y = "Semivariance"
    ) +
    theme_minimal()

  #Covariance plot
  cov_plot <- ggplot(v, aes(x = dist, y = covariance)) +
    geom_line(color = "darkgreen") +
    geom_point(color = "darkgreen") +
    labs(
      title = paste("Empirical Covariance -", chemical_name),
      x = "Distance (km)",
      y = "Covariance"
    ) +
    theme_minimal()

  # Number of pairs per bin plot
  count_plot <- ggplot(v, aes(x = dist, y = np)) +
    geom_col(fill = "gray50") +
    labs(
      title = paste("Number of Pairs per Bin -", chemical_name),
      x = "Distance (km)",
      y = "Point Pairs"
    ) +
    theme_minimal()

  return(list(
    variogram = vario_plot,
    counts = count_plot,
    covariance = cov_plot
  ))
}

# Apply to each chemical
chemical_names <- unique(site_summary$AQS_PARAMETER_NAME)
plots_list <- lapply(
  chemical_names,
  compute_variogram_plots,
  data = site_summary,
  bin_width = bin_width,
  max_distance = max_distance
)

# Extract plots and stack them vertically
all_plots <- do.call(
  grid.arrange,
  c(
    lapply(
      plots_list,
      function(x) list(x$variogram, x$counts, x$covariance)
    ) %>%
      unlist(recursive = FALSE),
    ncol = 1
  )
)
