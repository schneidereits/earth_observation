# ==================================================================================================
# MSc Earth Observation Exercise [6]: Accuracy assessment and area estimation
# ==================================================================================================
library(raster)
library(rgdal)


# Define the folder that contains your data
dir_data <- ""
setwd(dir_data)


# ==================================================================================================
# 1) Producing reference data
# ==================================================================================================
# input classification map
cfc <- raster("classification/LND_multitemp_RF_classification.tif")

# create stratified random sample
strat_rand_sample <- sampleStratified(cfc, size=25, sp=TRUE, na.rm=T)
writeOGR(strat_rand_sample, dsn='', layer='reference_100', driver='ESRI Shapefile')

# --> QGIS


# ==================================================================================================
# 2) Area-adjusted accuracy assessment
# ==================================================================================================
class_freq <- as.data.frame(freq(cfc, useNA = "no"))
class_freq$w_i <- class_freq$count/sum(class_freq$count)
sum(class_freq$w_i)

val_points <- readOGR("samples_prep.shp")
map_class <- extract(cfc, val_points)

ref_class <- val_points@data$classID
confusion_matrix <- table(map_class, ref_class)
confusion_matrix


# ==================================================================================================
# 3) Knowledge transfer
# ==================================================================================================
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
print(data.frame(class = 1:4, area_unadjusted, area_adjusted))


# ==================================================================================================
# Advanced assignment: Compare the accuracy of SVM and RF classification
# ==================================================================================================
library(mapac)

# get stratum labels
svm <- raster("classification/LND_multitemp_SVM_classification.tif")
strat_class <- map_class  # our strata was based on the RF (current map_class)
map_class <- extract(svm, val_points)

N_h <- ni  # samples per class
h <- c("1", "2", "3", "4")  # unique classes N_h corresponds to

result <-  aa_stratified(s=strat_class, r=ref_class, m=map_class, 
                         h=h, 
                         N_h=N_h)
print(result)

