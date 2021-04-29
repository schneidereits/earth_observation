##############################################################################
# MSc Earth Observation Assignment [Session 2]
# [Shawn Schneidereit]
#############################################################################

# Load the package ----
library(raster)
library(rgdal)
library(tidyverse)



##############################################################################

# Vegetation indices and data transformations


#############################################################################



##############################################################################
# 1) Compute vegetation indices
#############################################################################

stack_march <- stack("data/gcg_eo_s03/sr_data/LC081890252014031001T1-SC20170927101754/LC081890252014031001T1-SC20170927101754_sr_masked_crop.tif")
stack_march[stack_march>10000] = NA
stack_march[stack_march<0] = NA

plot(stack_march)

blue <- stack_march[[1]]
green <- stack_march[[2]]
red <- stack_march[[3]]
nIR <- stack_march[[4]]
SWIR <- stack_march[[5]]

# NDVI

ndvi <- ((nIR - red) / (nIR + red)) 

print(ndvi)
plot(ndvi)

# EVI 

# The correction factors for calculating EVI from Landsat data
G=2.5
C1=6
C2=7.5
L=1

evi <- G * (nIR - red) / (nIR + C1 * red - C2 * blue + (L*10000))
evi[evi<0] = 0

plot(evi)

stack_ndvi <-  writeRaster(ndvi, "~/Documents/humbolt/semester_02/earth_observation/data/gcg_eo_s03/sr_data/LC081890252014031001T1-SC20170927101754/LC081890252014031001T1-SC20170927101754_ndvi",
                           format = "GTiff", 
                           datatype = "INT2S",
                           overwrite = T)

stack_evi <-  writeRaster(evi, "~/Documents/humbolt/semester_02/earth_observation/data/gcg_eo_s03/sr_data/LC081890252014031001T1-SC20170927101754/LC081890252014031001T1-SC20170927101754_evi",
                          format = "GTiff", 
                          datatype = "INT2S",
                          overwrite = T)

##############################################################################
# 2) Perform a Tasseled Cap transformation
#############################################################################

tcc <- matrix(c( 0.2043,  0.4158,  0.5524, 0.5741,  0.3124,  0.2303, 
                 -0.1603, -0.2819, -0.4934, 0.7940, -0.0002, -0.1446,
                 0.0315,  0.2021,  0.3102, 0.1594, -0.6806, -0.6109), 
              dimnames = list(c('blue', 'green', 'red', 'nIR', 'swIR1', 'swIR2'), c('brightness', 'greenness', 'wetness')),
              ncol = 3)

print(tcc)

brightness <- stack_march[[1]] * tcc[1,1] + 
  stack_march[[2]] * tcc[2,1] + 
  stack_march[[3]] * tcc[3,1] + 
  stack_march[[4]] * tcc[4,1] + 
  stack_march[[5]] * tcc[5,1] + 
  stack_march[[6]] * tcc[6,1]


greenness <- stack_march[[1]] * tcc[1,2] + 
  stack_march[[2]] * tcc[2,2] + 
  stack_march[[3]] * tcc[3,2] + 
  stack_march[[4]] * tcc[4,2] + 
  stack_march[[5]] * tcc[5,2] + 
  stack_march[[6]] * tcc[6,2]


wetness <- stack_march[[1]] * tcc[1,3] + 
  stack_march[[2]] * tcc[2,3] + 
  stack_march[[3]] * tcc[3,3] + 
  stack_march[[4]] * tcc[4,3] + 
  stack_march[[5]] * tcc[5,3] + 
  stack_march[[6]] * tcc[6,3]

tcc_stack <- stack(brightness, greenness, wetness)

stack_tcc <-  writeRaster(tcc_stack, "~/Documents/humbolt/semester_02/earth_observation/data/gcg_eo_s03/sr_data/LC081890252014031001T1-SC20170927101754/LC081890252014031001T1-SC20170927101754_tcc",
                          format = "GTiff", 
                          datatype = "INT2S",
                          overwrite = T)

##############################################################################

#Training data

#############################################################################



##############################################################################
# 1) Prepare the training data collection
#############################################################################


#Which genera are dominant in the study area?
#Ash, Beech, Fir, Spruce


##############################################################################
# 2) Collect training data
#############################################################################


##############################################################################
# 3) Explore your training data
#############################################################################

####################################################################
# Load required packages
library(raster)
library(rgdal)
library(hexbin)
library(ggplot2)
library(reshape2)


####################################################################
### Get the data ready

# Read march image as stack and rename spectral bands
img <- stack('data/gcg_eo_s03/sr_data/LC081890252014031001T1-SC20170927101754/LC081890252014031001T1-SC20170927101754_sr_masked_crop.tif')

names(img) <- c("blue", "green", "red", "nir", "swir1", "swir2")

# Read training points, the following code assumes that it contains only the class attribute
# in readOGR, dsn specifies the path to the folder containing the file (may not end with /), 
# layer specifies the name of the shapefile without extension (.shp)
train <- readOGR(dsn='course.dir', layer='training_points')

# Extract image values at training point locations
train.sr <- extract(img, train, sp=T)

# Convert to data.frame and convert classID into factor
train.df <- as.data.frame(train.sr)
train.df$classID <- as.factor(train.df$classID)

####################################################################
### Create boxplots of reflectance grouped by land cover class

# Melt dataframe containing point id, classID, and 6 spectral bands
spectra.df <- melt(train.df, id.vars='classID', 
                   measure.vars=c('blue', 'green', 'red', 'nir', 'swir1', 'swir2'))

# Create boxplots of spectral bands per class
ggplot(spectra.df, aes(x=variable, y=value, color=classID)) +
  geom_boxplot() +
  theme_bw()

####################################################################
### Create 2D scatterplot of image data and locations of training points

# Convert image to data.frame and remove missing values
sr.march.val <- data.frame(getValues(img))
sr.march.val <- na.omit(sr.march.val)

# Randomly sub-sample 100,000 to speed up visualisation
sr.march.val <- sr.march.val[sample(nrow(sr.march.val), 100000),]  

# Specify which bands to use for the x and y axis of the plot
xband <- "red"
yband <- "nir"

# Create plot of band value density and training data
ggplot() + 
  geom_hex(data = sr.march.val, aes(x = get(xband), y = get(yband)), bins = 100) + 
  geom_point(data = train.df, aes(x = get(xband), y = get(yband), color=classID, shape=classID), 
             size = 2, inherit.aes = FALSE, alpha=1) + 
  
  scale_fill_gradientn(colours=c("black","white"), na.value=NA) + 
  scale_x_continuous(xband, limits=c(-10, quantile(sr.march.val[xband], 0.98, na.rm=T))) +
  scale_y_continuous(yband, limits=c(-10, quantile(sr.march.val[yband], 0.98, na.rm=T))) +
  scale_color_manual(values=c("red", "blue", "green", "purple")) +
  theme_bw()