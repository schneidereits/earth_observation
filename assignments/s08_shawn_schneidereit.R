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

training_data <- readOGR(dsn='data/gcg_eo_s08/training')

/Users/shawn/Documents/humbolt/semester_02/earth_observation/data/gcg_eo_s08/training

df <- as.data.frame(stack) 
 
df_long <- df %>% 
  pivot_longer(names_to = "variable", values_to = "value", cols = c(1:8))

ggplot(df_long, aes(x=variable, y=value, color=variable)) +
  geom_boxplot()


#############################################################################
# 4) Classify post-deforestation land use
#############################################################################

#############################################################################
# 5) Accuracy assessment and area estimates
#############################################################################




