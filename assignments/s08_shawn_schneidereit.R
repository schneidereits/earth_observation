#############################################################################
# MSc Earth Observation Assignment 8
# [Shawn Schneidereit]
#############################################################################

#############################################################################
# Library and data import
#############################################################################

library(tidyverse)
library(rgdal)
library(raster)
library(randomForest)

stack <- stack(paste0("data/gcg_eo_s08/spectemp/TCB/TCB_",c("iqr",
                                                            "max",
                                                            "mean",
                                                            "min",
                                                            "p25",
                                                            "p50",
                                                            "p75",
                                                            "std"),".tif"))
                                                     

plot(stack)

#############################################################################
# 1) Inspect the input data
#############################################################################

#############################################################################
# 2) Assess suitability of spectral-temporal metrics
#############################################################################

# Compare the temporal behavior of the two classes, identify the main
# characteristics and differences. Please record your insights as 
# comments in your script. Select spectral-temporal metrics that you 
# believe capture the differences between croplands and pastures well.
# Select at most six different metrics from the ones provided.

training_data <- readOGR(dsn='data/gcg_eo_s08/training/training_data_50.shp')

spatial_df <- raster::extract(stack, training_data, sp=T) 

df <- as.data.frame(spatial_df) %>% 
  dplyr::select(-coords.x1, -coords.x2) %>% 
  mutate(classID = as.factor(classID))

df_filtered <- df %>% dplyr::select(classID,TCB_max,TCB_p75)

df_long <- df %>% 
  pivot_longer(names_to = "variable", values_to = "value", cols = c(2:9)) %>% 
  mutate(classID = as.factor(classID))

#failed attempt
library(readr)
Landsat_2010_Sensor_Date <- read_delim("data/gcg_eo_s08/ts_stacks/Landsat_2010_Sensor_Date.csv", 
                                       ";", escape_double = FALSE, trim_ws = TRUE)
head(Landsat_2010_Sensor_Date)

TC_brightness <- stack("data/gcg_eo_s08/ts_stacks/TCB_stack.tif")
TCB_df <- raster::extract(TC_brightness, training_data, sp=T)

TCB_df <- as.data.frame(TCB_df)  %>% dplyr::select(-coords.x1, -coords.x2)

names(TCB_df)[2:31] <- Landsat_2010_Sensor_Date$date


TCB_df_long <- TCB_df %>% 
  pivot_longer(names_to = "date", values_to = "value", cols = c(2:31)) %>% 
  mutate(classID = as.factor(classID)) %>% 
  filter(classID != 3)

ggplot(TCB_df_long, aes(date, value, color=classID)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45)) 


  #facet_wrap(,~variable)

# From out plot we can see that the main defining feature 
# for distinguishing classes is max/p75 brightness. Class 3 (other)
# tends to be less bright and has a larger variability than crop and pasture
# Croplands tend to have the highest brightness and lowest variability.


# We decided that min, iqr, and std where the worst at differentiating 
# between classes and thus exuded them


#############################################################################
# 3) Compute additional spectral-temporal metrics
#############################################################################

# Which temporal subset of your time series highlights the seasonal
# differences between croplands and pastures best (e.g. ‘dry season’, 
# ‘third quarter’ or ‘August’)?

# Jan-march and sept-oct seem like the best time periods to differentiate
# between the classes
  
#  Which statistics provide a high information content? Your metrics can
# be for instance additional percentiles, information on the timing of
# specific events, such as the observation date of the minimum value, or 
# differences between metrics. Be creative!

ggplot(df_long, aes(y=value, group=classID)) +
  geom_boxplot(aes( color=classID)) +
  facet_wrap(~variable) +
  theme_classic()

# From out plot we can see that the main defining feature 
# for distinguishing classes is max/p75 brightness.


# Read TCb time series stack
tcb_stack <- stack('data/gcg_eo_s08/ts_stacks/TCb_stack.tif')

# Convert to matrix
tcb_matrix <- as.matrix(tcb_stack)

# Calculate median across rows in matrix
tcb_median <- apply(tcb_matrix, 1, FUN=median, na.rm=T)

# Write results to empty raster
tcb_median_raster <- raster(nrows=tcb_stack@nrows, 
                            ncols=tcb_stack@ncols, 
                            crs  =tcb_stack@crs, 
                            vals =tcb_median,
                            ext  =extent(tcb_stack))

# Plot result
plot(tcb_median_raster)




#############################################################################
# 4) Classify post-deforestation land use
#############################################################################
# Train a RF or SVM model based on your selected and computed
# spectral-temporal metrics. As always, make an informed decision about 
# the model parameters.

library(e1071)
# Random Forest model
# Define accuracy from 5-fold cross-validation as optimization measure
cv <- tune.control(cross = 5)          

# Use tune.randomForest to assess the optimal combination of ntree and mtry
rf.tune <- tune.randomForest(classID~., 
                             data        = df_filtered, 
                             ntree       =750, 
                             mtry        =c(2), 
                             tunecontrol = cv)

rf <- randomForest(classID~., df_filtered, ntree= 750, mtry=2)


### Run predict() to store RF predictions
map <- predict(stack, rf)
hist(map)
plot(map)
# 
# # SVM model
# 
# # Define accuracy from 5-fold cross-validation as optimization measure
# cv <- tune.control(cross = 5)
# 
# # Use tune.svm() for a grid search of the gamma and cost parameters
# svm.tune <- tune.svm(classID~.,
#                      data = df,
#                      kernel = 'radial',
#                      gamma = (0.01:100),
#                      cost = 10^(-3:3),
#                      tunecontrol = cv)
# 
# # Store the best model in a new object
# svm.best <- svm.tune$best.model
# 
# # Which parameters performed best?
# print(svm.best$gamma)
# print(svm.best$cost)
# 
# svm_predictions <- predict(stack, svm.best, index=c(1)) #type = "prob"
# plot(svm_predictions)
# hist(svm_predictions)
# writeRaster(svm_predictions*1, "data/gcg_eo_s08/RF_svm.tif",
#             datatype="INT1S",
#             overwrite=T)

# last weeks predicitons
last_wk_pred <- stack("data/gcg_eo_s07/RF_predictions.tif")
last_wk_pred_masked <- mask(map, last_wk_pred, maskvalue=c(1,4))
plot(last_wk_pred_masked)

writeRaster(last_wk_pred_masked, "data/gcg_eo_s08/RF_predictions.tif", 
            format = "GTiff", 
            datatype = "INT2S",
            overwrite = T)
#############################################################################
# 5) Accuracy assessment and area estimates
#############################################################################



reference_data <- readOGR("data/gcg_eo_s08/validation/validation_sample.shp")

reference_data <- as.data.frame(reference_data) %>%  na.omit() #%>% rename(reference  = layer)

predictions <- raster::extract(last_wk_pred_masked, reference_data[2:3])

reference_data$predictions <- predictions
reference_data <- na.omit(reference_data)

(confusion_matrix <- table(reference_data[c(1,4)]))

library(caret)

confusionMatrix(data = as.factor(reference_data[1]),
                reference = as.factor(reference_data[4]))



#  Area-adjusted accuracy assessment
freq(map) %>% na.omit()

class_freq <- as.data.frame(freq(map, useNA = "no"))
class_freq$w_i <- class_freq$count/sum(class_freq$count)
sum(class_freq$w_i)


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
print(data.frame(class = 1:3, area_unadjusted, area_adjusted))

# B) Combine the resulting map with your forest change map from last week

#stable non forest 
non_forest <- mask(map, last_wk_pred, maskvalue=c(5), inverse=T)
plot(non_forest)

# pasture
freq(non_forest)$layer[1,2]/freq(last_wk_pred)$RF_predictions[5,2]
# crop
freq(non_forest)$layer[2,2]/freq(last_wk_pred)$RF_predictions[5,2]
# other
freq(non_forest)$layer[3,2]/freq(last_wk_pred)$RF_predictions[5,2]


# forest conversion 2005
deforest_pred_2005 <- mask(map, last_wk_pred, maskvalue=c(2), inverse=T)
plot(deforest_pred_2005)

# pasture
freq(deforest_pred_2005)$layer[1,2]/freq(last_wk_pred)$RF_predictions[2,2]
# crop
freq(deforest_pred_2005)$layer[2,2]/freq(last_wk_pred)$RF_predictions[2,2]
# other
freq(deforest_pred_2005)$layer[3,2]/freq(last_wk_pred)$RF_predictions[2,2]

# forest conversion 2010
deforest_pred_2010 <- mask(map, last_wk_pred, maskvalue=c(3), inverse=T)
plot(deforest_pred_2010)

# pasture
freq(deforest_pred_2010)$layer[1,2]/freq(last_wk_pred)$RF_predictions[3,2]
# crop
freq(deforest_pred_2010)$layer[2,2]/freq(last_wk_pred)$RF_predictions[3,2]
#other
freq(deforest_pred_2010)$layer[3,2]/freq(last_wk_pred)$RF_predictions[3,2]





