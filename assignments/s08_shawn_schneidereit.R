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
  dplyr::select(-coords.x1, -coords.x2)

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

#############################################################################
# 5) Accuracy assessment and area estimates
#############################################################################




