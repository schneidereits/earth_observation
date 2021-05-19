#############################################################################
# MSc Earth Observation Assignment 6
# [Shawn Schneidereit]
#############################################################################

#############################################################################
# Library and data import
#############################################################################

library(tidyverse)
library(rgdal)
library(raster)



#############################################################################
# 1) Producing reference data
#############################################################################

forest_map <- raster("data/gcg_eo_s05/RF_predictions.tif")

sample <- sampleStratified(forest_map, size = 25, sp=T, na.rm=T)

writeOGR(sample, dsn = "data/gcg_eo_s06/sample_stratified", overwrite_layer = T,
         driver = "ESRI Shapefile", layer = "ref")

#############################################################################
# 2) Producing reference data
#############################################################################
library(caret)
freq(forest_map) %>% na.omit()


reference_data <- readOGR("data/gcg_eo_s06/sample_stratified/ref.shp")

reference_data <- as.data.frame(reference_data)

confusionMatrix(data = as.factor(reference_data$RF_prdc),
                reference = as.factor(reference_data$ref_id))

#  Which class has the highest / lowest user´s accuracy?
#  Which class has the highest / lowest producer´s accuracy?
#  How does the overall accuracy differ after area-adjustment? Why?
#  How do the map-based area estimates differ from those obtained using the reference data?
  


#############################################################################
# 3) Producing reference data
#############################################################################

#############################################################################
# Advanced assignment: Producing reference data
#############################################################################