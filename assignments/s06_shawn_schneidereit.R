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

writeOGR(sample, dsn = "data/gcg_eo_s06/sample_stratified",
         driver = "ESRI Shapefile", layer = "ref")

#############################################################################
# 2) Area-adjusted accuracy assessment
#############################################################################
library(caret)
freq(forest_map) %>% na.omit()


reference_data <- readOGR("data/gcg_eo_s06/sample_stratified_recovered/ref_recovered.shp")

reference_data <- as.data.frame(reference_data)

table(reference_data[2:3])

confusionMatrix(data = as.factor(reference_data$RF_prdc),
                reference = as.factor(reference_data$ref_id))




#  Which class has the highest / lowest user´s accuracy?
# Non-forest has the highest and mixed has the lowest user accuracy

#  Which class has the highest / lowest producer´s accuracy?
# Deciduous has the highest and coniferous has the lowest producer accuracy

#  How does the overall accuracy differ after area-adjustment? Why?
#  The user error remains unaffected, as the proportion between the weighted
#  counts and sum remain constraint.

## The producer acuracies are modified, with especially large deviations
## for the non forest accuracy. This is due to the large size of the class
## and the errors made being amplified with a greater weighting factor. 

#  How do the map-based area estimates differ from those obtained using 
#  the reference data?
  # there is a 5-7% deviation between the map based area estimates and 
 # the reference data based estimates. 


#############################################################################
# 3) Knowledge transfer
#############################################################################

# Generate the confusion matrix containing probabilities. In this matrix, 
# each cell value represents the probability of occurrence on map class i
# and reference class j

# Apologies for the woefully in-eloquent loops

reference_prob <- table(reference_data[2:3]) 
weights <- c(.215, .16, .2077, .4173)
for (i in 1:4) {
reference_prob[i,] <- (reference_prob[i,]/sum(reference_prob[i,])) *weights[i]
}


# Calculate overall accuracy and class-wise user´s and producer´s accuracy 
# from the confusion matrix.

overall_accuracy <- reference_prob[1,1] + reference_prob[2,2] +
  reference_prob[3,3]+ reference_prob[4,4]

producer_accuracy =c(1.4)
for (i in 1:4) {
producer_accuracy[i] <- reference_prob[i,i] / sum(reference_prob[,i])
}

user_accuracy =c(1:4)
for (i in 1:4) {
  user_accuracy[i] <- reference_prob[i,i] / sum(reference_prob[i,])

}

# Produce error-adjusted area estimates from the confusion matrix.

area = 256407

error_adj_area <- c(1:4)
for (i in 1:4) {
  error_adj_area[i] = area *sum(reference_prob[,i])
}


#############################################################################
# Advanced assignment: Producing reference data
#############################################################################

# load devtools package
library(devtools)

# install mapac package from gitlab
#install_gitlab(repo='pflugmad/mapac', host='scm.cms.hu-berlin.de', quiet=F, force=T)
library(mapac)

# compare overall and class-wise user´s and producer´s accuracies of the two 
# maps. Can you find a notable difference in the performance of RF versus 
# SVM classification? If so, which?


a <- aa_stratified(
  stratum = reference_data$ref_id,
  reference = reference_data$RF_prdc,
  map = reference_data$RF_prdc,
  h = c("1", "2", "3", "4"),
  N_h <- c(55126.80, 41048.28, 53244.81, 106987.32))









