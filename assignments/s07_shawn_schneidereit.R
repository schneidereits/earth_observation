#############################################################################
# MSc Earth Observation Assignment 7
# [Shawn Schneidereit]
#############################################################################

#############################################################################
# Library and data import
#############################################################################

library(tidyverse)
library(rgdal)
library(raster)
library(randomForest)



#############################################################################
# 1) Choosing an approach for change detection
#############################################################################

stack <- stack("data/gcg_eo_s07/sr_data/LT052260682000061001T1-SC20170927060330_crp.tif",
               "data/gcg_eo_s07/sr_data/LT052260682005060801T1-SC20170927060440_crp.tif",
               "data/gcg_eo_s07/sr_data/LT052260682010062201T1-SC20170927060241_crp.tif")



#############################################################################
# 2) Screening of input data and training data collection
#############################################################################

#############################################################################
# 3) Model training & classification
#############################################################################

training_data <- readOGR(dsn='data/gcg_eo_s07/training_data.shp')


# Use extract() to create a data.frame with training points as rows, and class labels 
# (classID) as well as the spectral bands of your composites as columns. 
# Remove the day of year and year flags (band 7 and 8) for the next steps.

spatial_df <- raster::extract(stack, training_data, sp=T) 

df <- as.data.frame(spatial_df) %>% 
  select(-1) %>% 
  na.omit() %>% 
  select(-coords.x1, -coords.x2)

names(df)


# As we want to train a classification (and not a regression), the randomForest() 
# function expects the dependent variable to be of type factor. 
# Use as.factor() for conversion of the classID column. The RF algorithm cannot 
# deal with NoData (NA) values. Remove NAs from the data.frame.


df$catagory <- as.factor(df$catagory)

str(df)

# Train a randomForest() classification model with the data.frame created in the 
# prior step. Make sure to include only useful predictors.

rf <- randomForest(catagory~., df, ntree= 1000)

##########################################################################

library(e1071)

# Define accuracy from 5-fold cross-validation as optimization measure
cv <- tune.control(cross = 5) 

# Use tune.randomForest to assess the optimal combination of ntree and mtry
rf.tune <- tune.randomForest(catagory~., 
                             data        = df, 
                             ntree       =750, 
                             mtry        =c(2:10), 
                             tunecontrol = cv)

# Store the best model in a new object for further use
rf.best <- rf.tune$library(e1071)


# Is the parametrization and/or different from your previous model?
print(rf.tune)

#############################################################################
# 4) Change map validation & area estimates
#############################################################################

### Run predict() to store RF predictions
map <- predict(stack, rf)
plot(map)

### Write classification to disk
writeRaster(map, "data/gcg_eo_s07/RF_predictions.tif",  format = "GTiff", 
            datatype = "INT2S",
            overwrite = T)


#  Area-adjusted accuracy assessment
freq(map) %>% na.omit()

class_freq <- as.data.frame(freq(map, useNA = "no"))
class_freq$w_i <- class_freq$count/sum(class_freq$count)
sum(class_freq$w_i)



reference_data <- readOGR("data/gcg_eo_s07/validation/validation_points.shp")

reference_data <- as.data.frame(reference_data) %>%  na.omit() %>% 
  rename(reference  = layer)

predictions <- raster::extract(map, reference_data[3:4])

reference_data$predictions <- predictions

confusion_matrix <- table(reference_data[c(5,2)])

library(caret)

 confusionMatrix(data = as.factor(reference_data$reference),
                reference = as.factor(reference_data$predictions))


ni <- apply(confusion_matrix, 1, sum)  # should be 25 for each class but might be different when calculating sampling size differently

# a)
confusion_matrix_prob <- confusion_matrix / ni * class_freq$w_i

# b)
OA <- sum(diag(confusion_matrix_prob))
print(OA)

PA <- diag(confusion_matrix_prob)/apply(confusion_matrix_prob, 2, sum)
print(PA)

UA <- diag(confusion_matrix_prob)/apply(confusion_matrix_prob, 1, sum)
print(UA)

# c)
ref_class_proportion <- apply(confusion_matrix_prob, 2, sum)

area_adjusted <- (ref_class_proportion * sum(class_freq$count)) * (30^2) / 10000
area_unadjusted <- class_freq$count * (30^2) / 10000
print(data.frame(class = 1:5, area_unadjusted, area_adjusted))
