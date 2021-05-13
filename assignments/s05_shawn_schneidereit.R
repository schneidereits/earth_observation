#############################################################################
# MSc Earth Observation Assignment 5
# [Shawn Schneidereit]
#############################################################################

#############################################################################
# Library and data import
#############################################################################

library(randomForest)
library(rgdal)
library(raster)
library(tidyverse)

#############################################################################
# 1) Training a Random Forest model
#############################################################################

# Load the vector file containing your training data points using readOGR().
# Next, create a stack() / brick() of your favourite pixel-based composite (last week´s result).

training_data <- readOGR(dsn='data/gcg_eo_s03/sr_data', layer='training_points')

composite <- stack("data/gcg_eo_s04/composite_1.tif")


# Use extract() to create a data.frame with training points as rows, and class labels 
# (classID) as well as the spectral bands of your composites as columns. 
# Remove the day of year and year flags (band 7 and 8) for the next steps.

spatial_df <- raster::extract(composite, training_data, sp=T) 

df <- as.data.frame(spatial_df) %>% 
  select(-Class.name, -composite_1.7, -composite_1.8, -coords.x1, -coords.x2) %>% 
  na.omit()



# As we want to train a classification (and not a regression), the randomForest() 
# function expects the dependent variable to be of type factor. 
# Use as.factor() for conversion of the classID column. The RF algorithm cannot 
# deal with NoData (NA) values. Remove NAs from the data.frame.


df$classID <- as.factor(df$classID)

str(df)

# Train a randomForest() classification model with the data.frame created in the 
# prior step. Make sure to include only useful predictors.

rf <- randomForest(classID~., df, ntree= 20000)

# Repeat the RF training procedure and produce additional model objects. 
# Use i) the other pixel-based composite, 
composite2 <- stack("data/gcg_eo_s05/composites/DOY196.tif")

spatial_df <- raster::extract(composite2, training_data, sp=T) 

df <- as.data.frame(spatial_df) %>% 
  select(-c(2,9:12)) %>% 
  na.omit()

df$classID <- as.factor(df$classID)



rf2 <- randomForest(classID~., df)

# and ii) a stack of both composites as input features.
composite_combo <- stack(composite, composite2)


spatial_df <- raster::extract(composite_combo, training_data, sp=T) 

df <- as.data.frame(spatial_df) %>% 
  select(-c(2,9:10, 17:20)) %>% 
  na.omit()

df$classID <- as.factor(df$classID)


rf3 <- randomForest(classID~., df, ntree= 2000)

#############################################################################
# 2) Investigating model performance
#############################################################################


# Which model has the lowest OOB error?

mean(rf$err.rate[,1])
mean(rf2$err.rate[,1])
mean(rf3$err.rate[,1])

# model 3 has the lowest OOB error
  

# How does the OOB behave when increasing the number of trees in your model (ntrees)? 
# You can access the OOB per number of trees via err.rate. Use this information to 
# determine a suitable value for ntrees. From a model’s predicitive performance
# ntrees cannot be too high, only the computational performance will decrease. 
# The aim is to find the spot where the error rate stabilises (converges) and 
# adding more trees would no longer improve accuracy.

OOB_plot <- as.data.frame((rf3$err.rate[,1])) 
names(OOB_plot) <- "error"

OOB_plot <- OOB_plot %>% 
  mutate(cumulative_sum = cummean(error),
         n_tree = c(1:2000))

ggplot(OOB_plot, aes(n_tree, cumulative_sum)) +
  geom_line() +
  theme_classic()



# In the model with the lowest OOB error, Which of the four classes has the highest OOB error?
  
mean(rf3$err.rate[,2]) # deciduous
mean(rf3$err.rate[,3]) # mixed
mean(rf3$err.rate[,4]) # coniferous 
mean(rf3$err.rate[,5]) # non-forest

# Coniferous had the lowest OOB, while deciduous forest had the highest OOB

# In case you are not satisfied with your model performance, consider joining forces with another team and merge your training datasets. Feel free to experiment and document your findings.

# we are satisfied 



#############################################################################
# 3) Final model parametrization and variable importances
#############################################################################

# Train a final model with the best combination of images and ntrees.

rfm <- randomForest(classID~., df, ntree= 1000)

# Investigate the variable importances using varImpPlot(). 
# Use partialPlot() to produce partial dependence plots for your most important predictor and all four classes. Can you explain the differences between classes?
  
varImpPlot(rfm)
par(mfrow=c(2,2))
partialPlot(rfm,  pred.data = df, x.var= DOY196.1, which.class=1)
partialPlot(rfm,  pred.data = df, x.var= DOY196.1, which.class=2)
partialPlot(rfm,  pred.data = df, x.var= DOY196.1, which.class=3)
partialPlot(rfm,  pred.data = df, x.var= DOY196.1, which.class=4)
dev.off()

# Based on the partial plot of our best variable DOY196.1 (blue band)
# we can see why non-forest plots had the lowest OOB in the random forest

#############################################################################
# 4) Classification
#############################################################################

### Run predict() to store RF predictions
map <- predict(composite_combo, rfm)

### Write classification to disk
writeRaster(map, "data/gcg_eo_s05/RF_predictions.tif",  format = "GTiff", 
            datatype = "INT2S",
            overwrite = T)

### Run predict() to store RF probabilities for class 1-4
rfp <- predict(composite_combo, rfm, type = "prob", index=c(1:4))

### Scale probabilities to integer values 0-100 and write to disk
writeRaster(rfp*100, "data/gcg_eo_s05/RF_probabilty.tif", 
            datatype="INT1S", 
            overwrite=T)


#############################################################################
# Advanced assignment: Automated hyperparameter optimization
#############################################################################

library(e1071)

# Define accuracy from 5-fold cross-validation as optimization measure
cv <- tune.control(cross = 5) 

# Use tune.randomForest to assess the optimal combination of ntree and mtry
rf.tune <- tune.randomForest(classID~., 
                             data        = df, 
                             ntree       =750, 
                             mtry        =c(2:10), 
                             tunecontrol = cv)

# Store the best model in a new object for further use
rf.best <- rf.tune$library(e1071)

# Define accuracy from 5-fold cross-validation as optimization measure
cv <- tune.control(cross = 5) 

# Use tune.randomForest to assess the optimal combination of ntree and mtry
rf.tune <- tune.randomForest(classID~., 
                             data = train.df,
                             ntree=750, 
                             mtry=c(2:10), 
                             tunecontrol = cv)

# Store the best model in a new object for further use
rf.best <- rf.tune$best.model

# Is the parametrization and/or different from your previous model?
print(rf.best)


#############################################################################
# Advanced assignment: Support-Vector Machines (SVMs)
#############################################################################

library(e1071)

# Define accuracy from 5-fold cross-validation as optimization measure
cv <- tune.control(cross = 5) 

# Use tune.svm() for a grid search of the gamma and cost parameters
svm.tune <- tune.svm(classID~., 
                     data = df, 
                     kernel = 'radial', 
                     gamma = (0.001:1000), 
                     cost = 10^(-3:3), 
                     tunecontrol = cv)

# Store the best model in a new object
svm.best <- svm.tune$best.model

# Which parameters performed best?
print(svm.best$gamma)
print(svm.best$cost)

svm_predictions <- predict(composite_combo, svm.best, type = "prob", index=c(1:4))

writeRaster(svm_predictions*1, "data/gcg_eo_s05/RF_svm.tif", 
            datatype="INT1S", 
            overwrite=T)
