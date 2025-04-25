# Check covariance matrix for all covariates
library(targets)
library(dplyr)
library(corrplot)

#df=as.data.frame(tar_read("pgp_cleanup_nc"))
df = as.data.frame(tar_read("dt_feat_calc_xyt"))
vars = tar_read("vars")

noncov_names <- c(vars, "time", "lon", "lat", "year")
cov_names <- setdiff(names(df), noncov_names)
cov_ind <- which(names(df) %in% cov_names)

chemlist <- sort(unique(df$AQS_PARAMETER_NAME))

df1 = df %>% dplyr::filter(df$AQS_PARAMETER_NAME == chemlist[1])
df2 = df %>% dplyr::filter(df$AQS_PARAMETER_NAME == chemlist[1])

cov_data1 = df1[, cov_ind]
filtered_data <- cov_data1[, sapply(cov_data1, function(col) {
  vals <- col[!is.na(col)]
  length(unique(vals)) > 1 && any(vals != 0)
})]
cor_matrix <- cor(filtered_data, use = "pairwise.complete.obs")
#heatmap(cor_matrix)
corrplot::corrplot(cor_matrix)

# Save to PNG
png("corrplotUSA_large.png", width = 9000, height = 9000, res = 1000)
corrplot(cor_matrix, method = "color", tl.cex = 0.2) # Adjust tl.cex to scale labels
dev.off()

View(cor_matrix)
View(cov_data1)
