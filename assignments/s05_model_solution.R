# ==================================================================================================
# MSc Earth Observation Exercise [5]: Machine learning classification
# ==================================================================================================
library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(randomForest)
library(dplyr)
library(tidyr)


# Define the folder that contains your data
dir_data <- "C:\\Users\\leonx\\Desktop\\gcg_eo_s05"
setwd(dir_data)


# ==================================================================================================
# 1) Training a Random Forest model
# ==================================================================================================
# list image files
l_img <- list.files('composites', pattern=".tif$", recursive=T, full.names=T)
print(l_img)

# select and load files into raster objects
pbc_135 <- brick(l_img[3])[[1:6]]  # spectral bands only
pbc_288 <- brick(l_img[7])[[1:6]]
pbc_multitemp <- stack(pbc_135, pbc_288)

# read training data
train_points <- readOGR(dsn='training', layer='train_merge')

# extract predictors for training points and remove nodata
df_train <- raster::extract(pbc_multitemp, train_points)
df_train <- data.frame(classID = as.factor(train_points$classID), df_train) # prediction target is categorical (classification)
df_train <- drop_na(df_train)

# build RF classification models for each predictor space
set.seed(30)  # set seed for reproducibility of results (randomness is actually pseudo-random and fully algorithmic)
rf_135 <- randomForest(x=select(df_train, contains("DOY135")), y=df_train[,'classID'])
rf_258 <- randomForest(x=select(df_train, contains("DOY258")), y=df_train[,'classID'])
rf_full <- randomForest(classID~., data = df_train)


# ==================================================================================================
# 2) Investigating model performance
# ==================================================================================================
# retrieve out-of-bag (OOB) error estimates from individual models
df_oob <- data.frame(cbind(1:500, rf_135$err.rate[,1], rf_258$err.rate[,1], rf_full$err.rate[,1]))
# [,1] first column is the overall OOB error; the following columns are class wise
colnames(df_oob) <- c("ntree", "DOY135", "DOY258", "FULL")
df_oob <- gather(df_oob, key, value, -ntree)

# plot
ggplot(df_oob, aes(ntree, value, color = key)) + geom_line(size = 1) + xlab("Number of Trees") + ylab("OOB Error")
# The OOB error decreases strongly for all models when the number of trees is still small (ntree < 50);
# Convergences happens around 200-300 trees for almost all model (exception: DOY319 in this particular run! remember the random component)
# Leaving ntree at 500 is acceptable, because it is (1) computationally feasible and (2) guarantees convergence
# The full model with all predictors has the lowest OOB error (~ 2-3% better than second best)

# In the model with the lowest OOB error, Which of the four classes has the highest OOB error?
df_oob_full <- data.frame(cbind(1:500, rf_135$err.rate[,2:5]))
colnames(df_oob_full) <- c("ntree", "Deciduous forest", "Mixed forest", "Coniferous forest", "Non-forest")
df_oob_full <- gather(df_oob_full, key, value, -ntree)
ggplot(df_oob_full, aes(ntree, value, color = key)) + geom_line(size = 1) + xlab("Number of Trees") + ylab("OOB Error")
# --> Non-forest by far (~ 10%), followed by Coniferous (~20%), Mixed (~29%) and Deciduous (~43%)


# ==================================================================================================
# 3) Final model parametrization and variable importances
# ==================================================================================================
# Train a final model with the best combination of images and ntrees
# --> rf_full 

# Variable importance plot
varImpPlot(rf_full)
# --> most important predictor: red reflectance in DOY258 imagery 


# Partial dependency plot for most important predictor
pd_cl1 <- partialPlot(rf_full, df_train[,c(2:ncol(df_train))], 'DOY258.3', which.class=1)  # deciduous
pd_cl2 <- partialPlot(rf_full, df_train[,c(2:ncol(df_train))], 'DOY258.3', which.class=2)  # mixed
pd_cl3 <- partialPlot(rf_full, df_train[,c(2:ncol(df_train))], 'DOY258.3', which.class=3)  # evergreen
pd_cl4 <- partialPlot(rf_full, df_train[,c(2:ncol(df_train))], 'DOY258.3', which.class=4)  # non-forest

df_pd <- data.frame(x = pd_cl1$x, deciduous=pd_cl1$y, mixed=pd_cl2$y, evergreen=pd_cl3$y, nonforest=pd_cl4$y) %>%
  gather(key, value, -x)
ggplot(df_pd, aes(x/10000, value, color = key)) + geom_line(size = 1) + xlab("DOY258 Red Reflectance (%)") + ylab("mean marginal response")
# y-axis = mean marginal response (logit scale)
# All three vegetation types share the relative change that they make their outcome less likely as the reflectance increases; 
# Simultaneously, the mean marginal response of non-forest increases with an increase in greater red reflectance, both of which makes
# sense given the spectral behavior of the surface materials

# ==================================================================================================
# 4) Classification
# ==================================================================================================
# predict model into space
beginCluster()
classification <- clusterR(pbc_multitemp, predict, args = list(rf_full))
endCluster()

# write classification to disk
dir.create(file.path(dir_data, 'classification'), showWarnings = FALSE)
writeRaster(classification, 'classification/LND_multitemp_RF_classification.tif', datatype="INT2S")

# predict individual class probabilities for each pixel and write to dish
beginCluster()
rfp <- clusterR(pbc_multitemp, predict, args = list(rf_full, type="prob", index=c(1:4)))
endCluster()

writeRaster(rfp*100, 'classification/LND_multitemp_RF_rfp.tif', datatype="INT2S", overwrite=T)


# ==================================================================================================
# Advanced assignment: Automated hyperparameter optimization
# ==================================================================================================
library(e1071)

# Define accuracy from 5-fold cross-validation as optimization measure
cv <- tune.control(cross = 5) 

# Use tune.randomForest to assess the optimal combination of ntree and mtry
rf.tune <- tune.randomForest(classID~., data = df_train, ntree=750, mtry=c(2:10), tunecontrol = cv)

# Store the best model in a new object for further use
rf.best <- rf.tune$best.model

# Is the parametrization and/or different from your previous model?
print(rf.best)

# --> RF do not recquire heavy optimization, mtry (and some others) may sometimes be optimized but here
# the largest increase in model performance is to be expected when enrichening the feature space (e.g.
# by non-linear combinations of input spectra, additional STMs or auxilary information such as a DEM).


# ==================================================================================================
# Advanced assignment: Support-Vector Machines (SVMs)
# ==================================================================================================

cv <- tune.control(cross = 5) 
svm.tune <- tune.svm(classID~., data = df_train, kernel = 'radial', gamma = 10^(-3:3), cost = 10^(-3:3), tunecontrol = cv)
svm.best <- svm.tune$best.model

# Which parameters performed best?
print(svm.best$gamma)
print(svm.best$cost)

beginCluster()
classification <- clusterR(pbc_multitemp, predict, args = list(svm.best))
endCluster()

writeRaster(classification, 'classification/LND_multitemp_SVM_classification.tif', datatype="INT2S")


# EOF