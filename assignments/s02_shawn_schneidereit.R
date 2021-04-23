##############################################################################
# MSc Earth Observation Assignment [Session 2]
# [Shawn Schneidereit]
#############################################################################

# Load the package ----
library(raster)
library(rgdal)



##############################################################################
# Data Import
#############################################################################

# Define the folder that contains your data...
data.path <- '/Users/shawn/Documents/humbolt/semester_02/Earth_observation/data/gcg_eo_s02_sub/DN/LC08_L1TP_189025_20140716_20170421_01_T1_SUBSET/'

# Read MTL file
mtl <- list.files(data.path, pattern='MTL.txt$', recursive=T, full.names=T)
mtl.txt <- read.delim(mtl, sep = '=', stringsAsFactors = F)

# Extract numeric values
REFLECTANCE_MULT_BAND <- as.numeric(mtl.txt[grep('REFLECTANCE_MULT_BAND',mtl.txt$GROUP),][2:7,2])
REFLECTANCE_ADD_BAND <- as.numeric(mtl.txt[grep('REFLECTANCE_ADD_BAND',mtl.txt$GROUP),][2:7,2])
SUN_ELEVATION <- as.numeric(mtl.txt[grep('SUN_ELEVATION',mtl.txt$GROUP),][2])


data <- stack(paste0("/Users/shawn/Documents/humbolt/semester_02/Earth_observation/data/gcg_eo_s02_sub/DN/LC08_L1TP_189025_20140716_20170421_01_T1_SUBSET/LC08_L1TP_189025_20140716_20170421_01_T1_SUBSET_B", c(2:7), ".TIF"))

TOA <- (REFLECTANCE_MULT_BAND * data + REFLECTANCE_ADD_BAND)* 10000


# Helper-function to convert degrees to radians
deg2rad <- function(deg){ (deg * pi) / (180) }

TOA_rad <- TOA/sin(deg2rad(SUN_ELEVATION))
TOA_rad <- as.integer(TOA_rad)


raster_image.TOA <-  writeRaster(TOA_rad, "/Users/shawn/Documents/humbolt/semester_02/Earth_observation/data/gcg_eo_s02_sub/DN/LC08_L1TP_189025_20140716_20170421_01_T1_SUBSET/LC08_L1TP_189025_20140716_20170421_01_T1_SUBSET_TOA.GTiff",
                                 format = "GTiff", overwrite = T)

data_BOA <- stack(paste0("/Users/shawn/Documents/humbolt/semester_02/Earth_observation/data/gcg_eo_s02_sub/SR/LC081890252014071601T1-SC20171024094741/LC08_L1TP_189025_20140716_20170421_01_T1_subset_sr_band", c(2:7), ".TIF"))

raster_image.BOA <-  writeRaster(data_BOA, "/Users/shawn/Documents/humbolt/semester_02/Earth_observation/data/gcg_eo_s02_sub/SR/LC081890252014071601T1-SC20171024094741/LC08_L1TP_189025_20140716_20170421_01_T1_subset_sr_BOA.tif",
                                   format = "GTiff", overwrite = T)
