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


rf3 <- randomForest(classID~., df)

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

OOB_plot <- as.data.frame((rf$err.rate[,1])) 
names(OOB_plot) <- "error"

OOB_plot <- OOB_plot %>% 
  mutate(cumulative_sum = cummean(error),
         n_tree = c(1:20000))

ggplot(OOB_plot, aes(n_tree, cumulative_sum)) +
  geom_line() +
  theme_classic()



# In the model with the lowest OOB error, Which of the four classes has the highest OOB error?
  
# In case you are not satisfied with your model performance, consider joining forces with another team and merge your training datasets. Feel free to experiment and document your findings.




#############################################################################
# 3) Final model parametrization and variable importances
#############################################################################


#############################################################################
# 4) Classification
#############################################################################


#############################################################################
# Advanced assignment: Automated hyperparameter optimization
#############################################################################

#############################################################################
# Advanced assignment: Support-Vector Machines (SVMs)
#############################################################################

