#devtools::install_github("NIEHS/PrestoGP")
library(PrestoGP)

#Data Preprocessing
data(soil)
soil <- soil[!is.na(soil[,5]),] # remove rows with NA's
y <- soil[,4]                   # predict moisture content
X <- as.matrix(soil[,5:9])
locs <- as.matrix(soil[,1:2]) 

#Vecchia Model
soil.vm <- new("VecchiaModel", n_neighbors = 10)
soil.vm2 <- prestogp_fit(soil.vm, y, X, locs)

#Impute Missing y’s
miss <- sample(1:nrow(soil), 20)
y.miss <- y
y.miss[miss] <- NA
soil.vm2 <- new("VecchiaModel", n_neighbors = 10)
soil.vm2 <- prestogp_fit(soil.vm, y, X, locs, impute.y = TRUE)

# Impute missing Ys due to limit of detection
soil.lod <- quantile(y, 0.1)
y.lod <- y
y.lod[y.lod <= soil.lod] <- NA
soil.vm3 <- new("VecchiaModel", n_neighbors = 10)
soil.vm3 <- prestogp_fit(soil.vm, y, X, locs, impute.y = TRUE, lod = soil.lod)

#Full Model
soil.fm <- new("FullModel")
soil.fm <- prestogp_fit(soil.fm, y, X, locs)

#Multivariate Model
ym <- list()
ym[[1]] <- soil[,5]             # predict two nitrogen concentration levels
ym[[2]] <- soil[,7]
Xm <- list()
Xm[[1]] <- Xm[[2]] <- as.matrix(soil[,c(4,6,8,9)])
locsm <- list()
locsm[[1]] <- locsm[[2]] <- locs

soil.mvm <-  new("MultivariateVecchiaModel", n_neighbors = 10)
soil.mvm <- prestogp_fit(soil.mvm, ym, Xm, locsm)

#Space/Elevation Model
data(soil250, package="geoR")
y2 <- soil250[,7]               # predict pH level
X2 <- as.matrix(soil250[,c(4:6,8:22)])
# columns 1+2 are location coordinates; column 3 is elevation
locs2 <- as.matrix(soil250[,1:3])

soil.vm2 <- new("VecchiaModel", n_neighbors = 10)
# fit separate scale parameters for location and elevation
soil.vm2 <- prestogp_fit(soil.vm2, y2, X2, locs2, scaling = c(1, 1, 2))

#Test Set Prediction

# Create training and test sets
n <- length(y)
otr <- rep(FALSE, n)
otr[sample(1:n, size=floor(n/2))] <- TRUE
otst <- !otr

# Fit the model on the training set
soil.vmp <- new("VecchiaModel", n_neighbors = 10)
soil.vmp <- prestogp_fit(soil.vm, y[otr], X[otr,], locs[otr,])

# Perform predictions on the test set
soil.yhat <- prestogp_predict(soil.vmp, X[otst,], locs[otst,])
