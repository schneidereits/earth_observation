##############################################################################
# MSc Earth Observation Assignment [Session 2]
# [Shawn Schneidereit]
#############################################################################

# Load the package ----
library(raster)
library(rgdal)
library(tidyverse)



##############################################################################
# 1) DN to TOA conversion
#############################################################################

# Define the folder that contains your data...
data.path <- 'data/gcg_eo_s02_sub/DN/LC08_L1TP_189025_20140716_20170421_01_T1_SUBSET/'

# Read MTL file
mtl <- list.files(data.path, pattern='MTL.txt$', recursive=T, full.names=T)
mtl.txt <- read.delim(mtl, sep = '=', stringsAsFactors = F)

# Extract numeric values
REFLECTANCE_MULT_BAND <- as.numeric(mtl.txt[grep('REFLECTANCE_MULT_BAND',mtl.txt$GROUP),][2:7,2])
REFLECTANCE_ADD_BAND <- as.numeric(mtl.txt[grep('REFLECTANCE_ADD_BAND',mtl.txt$GROUP),][2:7,2])
SUN_ELEVATION <- as.numeric(mtl.txt[grep('SUN_ELEVATION',mtl.txt$GROUP),][2])


data <- stack(paste0("data/gcg_eo_s02_sub/DN/LC08_L1TP_189025_20140716_20170421_01_T1_SUBSET/LC08_L1TP_189025_20140716_20170421_01_T1_SUBSET_B", c(2:7), ".TIF"))

TOA <- (REFLECTANCE_MULT_BAND * data + REFLECTANCE_ADD_BAND)* 10000


# Helper-function to convert degrees to radians
deg2rad <- function(deg){ (deg * pi) / (180) }

TOA_rad <- TOA/sin(deg2rad(SUN_ELEVATION))
TOA_rad <- as.integer(TOA_rad)


raster_image.TOA <-  writeRaster(TOA_rad, "data/gcg_eo_s02_sub/DN/LC08_L1TP_189025_20140716_20170421_01_T1_SUBSET/LC08_L1TP_189025_20140716_20170421_01_T1_SUBSET_TOA.GTiff",
                                 format = "GTiff", overwrite = T)

# creat a stack of the provided BOA to then plot in GIS
data_BOA <- stack(paste0("data/gcg_eo_s02_sub/SR/LC081890252014071601T1-SC20171024094741/LC08_L1TP_189025_20140716_20170421_01_T1_subset_sr_band", c(2:7), ".TIF"))

raster_image.BOA <-  writeRaster(data_BOA, "data/gcg_eo_s02_sub/SR/LC081890252014071601T1-SC20171024094741/LC08_L1TP_189025_20140716_20170421_01_T1_subset_sr_BOA.tif",
                                   format = "GTiff", overwrite = T)

##############################################################################
# 2) Compare TOA with BOA
#############################################################################


# A. Briefly summarize the spectral appearance of the different land cover types.

# Forest - When plotting the spectral signature of forest pixels, I see that short wave spectral regions tend to have lower reluctance values, 
# followed by a sharp peak in the NIR/IR, and a gradually declining tail in the long wave regions
#

# Open soil - Overall open soil looks somewhat similar to the Forest spectral signature, with low reluctance values in the visible spectrum.
# Thus is followed by a far more gradual increase in reluctance values in the NIR/IR that tend to stabilize in the long wave region

# Water - Open waters spectral signature show comparatively much lower reluctance values across all regions of the spectrum. There is 
# a slight increase in the IR, but reluctance here is still quite low in general. 

# B. What are the most evident differences between TOA and SR reluctance spectra?
  
# The most evident difference in the OA and SR reluctance spectra, is that TOA spectra have higher 
# reluctance values and appear brighter. 






  
##############################################################################
# 2) Data quality
#############################################################################

# Background

# Some issues that affect the data quality of optical satellite images are cloud obstruction, backscatter from bright reflective objects, and
# capturing the spectral signatures of objects that occur at a sub pixel level, to name a few.

# The answer find out if a pixel is affected by any of these issues seems to be to use the provided quality bands!

#############################################################################
# 1) Investigating the QA band
#############################################################################

BQA <- raster("data/gcg_eo_s02_sub/DN/LC08_L1TP_189025_20140716_20170421_01_T1_SUBSET/LC08_L1TP_189025_20140716_20170421_01_T1_SUBSET_BQA.tif") 
as.data.frame(freq(BQA)) %>% 
  arrange(-count)
# The three most common values are 2720, 62800, & 2976
values <- c(2720, 62800, 2976)


for (i in values) {
  i = i
 # rev(as.numeric(intToBits(i)))
  print(i)
  return(i)
}


a <- rev(as.numeric(intToBits(2720)))
b <- rev(as.numeric(intToBits(62800)))
c <- rev(as.numeric(intToBits(2976)))

# What do the most frequent values mean? Decode each integer value into bits and 
# describe their meaning here: 

# 2720 - # Algorithm has low Confidence in cloud, cloud shadow, snow/ice, and cirrus existence in pixel  (probability of 0-33%).

# 62800 - # yes clouds exist in pixel
          # Algorithm has medium Confidence in cloud, cloud shadow, snow/ice, and cirrus existence in pixel  (probability of 34-66%).
          # Dont know what the 1 "yes" in bit position 13, 14,15 means!?

# 2976 - # Algorithm has low Confidence in cloud, snow/ice, and cirrus existence in pixel  (probability of 0-33%).
         # Algorithm has high Confidence in ccloud shadow, existence in pixel  (probability of 67-100%).

# Most frequent value: 2720
# Second most frequent value: 62800
# Third most frequent value: 2976

#############################################################################
# 2) Creating a cloud mask
#############################################################################

# Define function to find fill values from Landsat BQA
fill_pixels <- function(x) {intToBits(x)[1] == T}

# high confidence clouds or high confidence cloud shadows or fill values
a_pixels <- function(x) {
    all(intToBits(x)[c(1)] == T) | 
    all(intToBits(x)[c(6,7)] == T) | 
    all(intToBits(x)[c(8,9)] == T)}

# high and medium confidence clouds or high and medium confidence cloud shadows or fill values 
b_pixels <- function(x) {
    all(intToBits(x)[c(1)] == T) | 
    all(intToBits(x)[c(6,7)] == T) | 
    intToBits(x)[c(7)] == T |
    all(intToBits(x)[c(8,9)] == T) | 
    intToBits(x)[c(9)] == T}
  
mask_a <- calc(BQA, fun = a_pixels)
mask_b <- calc(BQA, fun = b_pixels)

plot(mask_a)
plot(mask_b)



raster_BOA_mask_A <-  writeRaster(mask_a, "data/gcg_eo_s02_sub/SR/LC081890252014071601T1-SC20171024094741/LC08_L1TP_189025_20140716_20170421_01_T1_subset_sr_BOA_mask_A.tif",
                                 format = "GTiff", overwrite = T)
raster_BOA_mask_B <-  writeRaster(mask_b, "data/gcg_eo_s02_sub/SR/LC081890252014071601T1-SC20171024094741/LC08_L1TP_189025_20140716_20170421_01_T1_subset_sr_BOA_mask_B.tif",
                                 format = "GTiff", overwrite = T)

# After some basic visual assessment and zooming into some particularly cloudy patches it seems like mask A is sufficient 
# at masking clouds. In the select regions I zoomed into, it seemed like mask B masked regions of the image that did 
# not appear to be clouds in the RBG image. 


#############################################################################
# 3) Masking images
#############################################################################

BOA_masked <- mask(data_BOA, 
                   mask      = raster_BOA_mask_A,
                   maskvalue = 1, # cuts out masked region from image
                   filename  = "data/gcg_eo_s02_sub/SR/LC081890252014071601T1-SC20171024094741/LC08_L1TP_189025_20140716_20170421_01_T1_subset_sr_BOA_masked.tif",
                   format    = "GTiff", 
                   overwrite = T)
plot(BOA_masked)
