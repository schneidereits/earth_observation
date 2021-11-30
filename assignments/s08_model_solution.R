# ==================================================================================================
# MSc Earth Observation Exercise [8]: Machine learning classification
# ==================================================================================================

# load packages:
library(raster)
library(rgdal)
library(randomForest)
library(ggplot2)
library(tidyr)
library(parallel)

# Change raster options to store large rasters in temp files on disk
rasterOptions(maxmemory = 1e6)

setwd('')

# ==================================================================================================
# 2) Assess suitability of spectral-temporal metrics
# ==================================================================================================

# read training .shp:
train <- readOGR('training/training_data_50.shp')
train$classname[train$classID == 1] <-  "pasture"
train$classname[train$classID == 2] <-  "cropland"
train$classname[train$classID == 3] <-  "other"


#--------------------------------------------------
# 2.1) Raster Stacks
#--------------------------------------------------

# read TC-stacks:
TCg <- brick('ts_stacks/TCG_stack.tif')
TCb <- brick('ts_stacks/TCB_stack.tif')
TCw <- brick('ts_stacks/TCW_stack.tif')

# read band specific acquisition dates:
sensor_date <- read.table('ts_stacks/Landsat_2010_Sensor_Date.csv', sep=";", header=T)

# extract raster values at training locations:
TCg.train <- raster::extract(TCg, train)
TCg.train <- data.frame(response = train$classname, TCg.train)

TCb.train <- raster::extract(TCb, train)
TCb.train <- data.frame(response = train$classname, TCb.train)

TCw.train <- raster::extract(TCw, train)
TCw.train <- data.frame(response = train$classname, TCw.train)

# format data frames
names(TCg.train) <- c("class", sensor_date$date)
names(TCb.train) <- c("class", sensor_date$date)
names(TCw.train) <- c("class", sensor_date$date)

# gather:
TCg.gather <- gather(TCg.train, key = date, value = TCg, -class)
TCg.gather$date <- as.Date(as.character(TCg.gather$date), "%Y%m%d")
TCb.gather <- gather(TCb.train, key = date, value = TCb, -class)
TCb.gather$date <- as.Date(as.character(TCb.gather$date), "%Y%m%d")
TCw.gather <- gather(TCw.train, key = date, value = TCw, -class)
TCw.gather$date <- as.Date(as.character(TCw.gather$date), "%Y%m%d")

# plot:
TCg.plot <- ggplot(data = TCg.gather) + geom_boxplot(aes(x = date, y = TCg, col = class, group=interaction(class, date))) + scale_x_date()
TCb.plot <- ggplot(data = TCb.gather) + geom_boxplot(aes(x = date, y = TCb, col = class, group=interaction(class, date))) + scale_x_date()
TCw.plot <- ggplot(data = TCw.gather) + geom_boxplot(aes(x = date, y = TCw, col = class, group=interaction(class, date))) + scale_x_date()

#--------------------------------------------------
# 2.2) Spectral-temporal metrics
#--------------------------------------------------

# TC-greenness
TCg.stm <- list.files('spectemp/TCG')
names <- list()

for (i in 1:length(TCg.stm)) {
  file <- raster(paste0('spectemp/TCG/', TCg.stm[i]))
  names[[i]] <- substring(TCg.stm[i], 1, nchar(TCg.stm[i])-4)
  assign(substring(TCg.stm[i], 1, nchar(TCg.stm[i])-4), file)
  extracted <- raster::extract(file, train)
  if (i == 1){
    TCg.df <- data.frame(filename = extracted)
  } else {
    TCg.df <- cbind(TCg.df, data.frame(filename = extracted))
  }
  names(TCg.df) <- names
}

TCg.df <- data.frame(class = train$classname, TCg.df)

# TC-brightness
TCb.stm <- list.files('spectemp/TCB')
names <- list()

for (i in 1:length(TCb.stm)) {
  file <- raster(paste0('spectemp/TCB/', TCb.stm[i]))
  names[[i]] <- substring(TCb.stm[i], 1, nchar(TCb.stm[i])-4)
  assign(substring(TCb.stm[i], 1, nchar(TCb.stm[i])-4), file)
  extracted <- raster::extract(file, train)
  if (i == 1){
    TCb.df <- data.frame(filename = extracted)
  } else {
    TCb.df <- cbind(TCb.df, data.frame(filename = extracted))
  }
  names(TCb.df) <- names
}

TCb.df <- data.frame(class = train$classname, TCb.df)

# # TC-wetness
TCw.stm <- list.files('spectemp/TCW')
names <- list()

for (i in 1:length(TCw.stm)) {
  file <- raster(paste0('spectemp/TCW/', TCw.stm[i]))
  names[[i]] <- substring(TCw.stm[i], 1, nchar(TCw.stm[i])-4)
  assign(substring(TCw.stm[i], 1, nchar(TCw.stm[i])-4), file)
  extracted <- raster::extract(file, train)
  if (i == 1){
    TCw.df <- data.frame(filename = extracted)
  } else {
    TCw.df <- cbind(TCw.df, data.frame(filename = extracted))
  }
  names(TCw.df) <- names
}

TCw.df <- data.frame(class = train$classname, TCw.df)

# plot:
TCg.gather <- gather(TCg.df, key = metric, value = values, -class)
TCg.plot <- ggplot(data = TCg.gather) + geom_boxplot(aes(x = class, y = values, col = class)) + facet_wrap(~ metric)

TCb.gather <- gather(TCb.df, key = metric, value = values, -class)
TCb.plot <- ggplot(data = TCb.gather) + geom_boxplot(aes(x = class, y = values, col = class)) + facet_wrap(~ metric)


TCw.gather <- gather(TCw.df, key = metric, value = values, -class)
TCw.plot <- ggplot(data = TCw.gather) + geom_boxplot(aes(x = class, y = values, col = class)) + facet_wrap(~ metric)


# Summarize the main characteristics of the three classes over the growing season and identify major differences.
#====================
# Temporal stacks
#====================

# "Others" exhibit a generally greater variability of TC-components;
# pasture generally shows higher TCg values than cropland with an inversion in the early months (Jan-Feb);
# TCb: relatively homogenous over time with an excpetion from Jul to Oct
# Overall Jul to Oct may be of most interest to seperate classes using TC transformations

#====================
# STMs
#====================

# TCg: generally not too well seperated with P25, STD indicating the most pronounced difference
# TCb: MAX, P75 and MEAN 
# TCw: MIN, P50, P25, MEAN

# SELECTED: TCW_min, TCW_50, TCB_P75, TCB_max, TCG_std, TCG_max
# Selection was based mostly on seperating cropland and pastures rather than others

# ==================================================================================================
# 3) Compute additional spectral-temporal metrics
# ==================================================================================================

# Overall Jul to Oct may be of most interest to seperate classes using TC transformations
TCg.subset <- TCg[[20:26]]
TCb.subset <- TCb[[20:26]]
TCw.subset <- TCw[[20:26]]

# max, min & std:
TCg.val <- getValues(TCg.subset)
TCb.val <- getValues(TCb.subset)
TCw.val <- getValues(TCw.subset)

TC.list <- list(TCg.val, TCb.val, TCw.val)

# max:
max.list <- mclapply(TC.list, FUN=apply, 1, max, na.rm=T, mc.cores = 7)
max.list <- lapply(max.list, FUN=setValues, x=raster(TCg.subset))

# min:
min.list <- mclapply(TC.list, FUN=apply, 1, min, na.rm=T, mc.cores = 7)
min.list <- lapply(min.list, FUN=setValues, x=raster(TCg.subset))

# std:
std.list <- mclapply(TC.list, FUN=apply, 1, sd, na.rm=T, mc.cores = 7)
std.list <- lapply(std.list, FUN=setValues, x=raster(TCg.subset))

# create final stack:
masterstack <- brick(c(max.list, min.list, std.list))
masterstack <- brick(c(masterstack, TCW_min, TCW_p50, TCB_p75, TCB_max, TCG_std, TCG_max))


# ==================================================================================================
# 4) Train and apply Random Forest Model
# ==================================================================================================
# create RF model:
masterstack.train <- raster::extract(masterstack, train)
masterstack.train <- data.frame(class = train$classname, masterstack.train)

# build random forest model
masterstack.train$class <- as.character(masterstack.train$class)
masterstack.train$class[masterstack.train$class == "pasture"] <-  1
masterstack.train$class[masterstack.train$class == "cropland"] <-  2
masterstack.train$class[masterstack.train$class == "other"] <-  3
masterstack.train$class <- as.factor(masterstack.train$class)

RF <- randomForest(class~., data = masterstack.train, na.action = na.omit, confusion = T, ntrees=750)


# prediction

beginCluster()
classification <- clusterR(masterstack, raster::predict, args = list(RF))
endCluster()

# mask
class_forest <- raster('data/CLASS_FOREST_multitemporal_.tif')

# class 1 & 4 have to be masked:
classification <- mask(classification, class_forest, maskvalue=1)
classification <- mask(classification, class_forest, maskvalue=4)
plot(classification)
writeRaster(classification, 'CLASS_CROP_PASTURE_masked.tif', format="GTiff", overwrite=T)

# ==================================================================================================
# 5) Accuracy assessment and area estimates
# ==================================================================================================

vali <- readOGR('validation/validation_sample.shp')

classification.val <- raster::extract(classification, vali)
vali.df <- data.frame(MAP = classification.val, REF = vali$ID)

confusion.matrix <- table(vali.df)

#overall accuracy
sum(diag(confusion.matrix)) / sum(confusion.matrix) 

# User's accuracy/error of comission
diag(confusion.matrix) / rowSums(confusion.matrix)

#Producer's accuracy/error of omission
diag(confusion.matrix) / colSums(confusion.matrix)  

# fractions:
# (1) stable non-forest
non.forest <- mask(classification, class_forest, maskvalue=5, inverse=T)
non.forest.freq <- data.frame(freq(non.forest, useNA="no"))
non.forest.freq$perc <- non.forest.freq$count/sum(non.forest.freq$count)

# (2) Deforestation 2001-2005
deforested.2005 <- mask(classification, class_forest, maskvalue=2, inverse=T)
deforested.2005.freq <- data.frame(freq(deforested.2005, useNA="no"))
deforested.2005.freq$perc <- deforested.2005.freq$count/sum(deforested.2005.freq$count)

# (3) Deforestation 2006-2010
deforested.2010 <- mask(classification, class_forest, maskvalue=3, inverse=T)
deforested.2010.freq <- data.frame(freq(deforested.2010, useNA="no"))
deforested.2010.freq$perc <- deforested.2010.freq$count/sum(deforested.2010.freq$count)

# Was forest primarily replaced by pastures or by cropland?
# Did this process change over the periods 2001-2005 and 2006-2010?
# Primarily pastures and it did change indeed yet only the magnitude not the LU type:
# (~50 vs. 25 cropland between 2001-2005) and (52 vs. 10 cropland between 2006-2010)