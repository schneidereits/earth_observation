# ==================================================================================================
# MSc Earth Observation Exercise [2]: Preprocessing, data quality & cloud masking
# ==================================================================================================

# load required packages
library(raster)

# input directory
dir_data <- 'C:/Users/leonx/Desktop/gcg_eo_s02_sub'


# ==================================================================================================
# Part I - DN / TOA / BOA
# 1) DN to TOA conversion
# ==================================================================================================
# read metadata file
mtl <- list.files(paste0(dir_data, "/DN"), pattern='MTL.txt$', recursive=T, full.names=T)
mtl.txt <- read.delim(mtl, sep = '=', stringsAsFactors = F)

# extract numeric values
REFLECTANCE_MULT_BAND <- as.numeric(mtl.txt[grep('REFLECTANCE_MULT_BAND', mtl.txt$GROUP),][2:7, 2])
REFLECTANCE_ADD_BAND <- as.numeric(mtl.txt[grep('REFLECTANCE_ADD_BAND', mtl.txt$GROUP),][2:7, 2])
SUN_ELEVATION <- as.numeric(mtl.txt[grep('SUN_ELEVATION', mtl.txt$GROUP),][2])

# convert sun elevation to radians
deg2rad <- function(deg){ (deg * pi) / (180) }
SUN_ELEVATION_RAD <- deg2rad(SUN_ELEVATION)

# read images into stack object
DN <- stack(
  list.files(paste0(dir_data, "/DN"), pattern="B[2-7].tif$", full.names=T, recursive=T)
)

# convert DN to TOA (let's assume we want to apply this to many images and hence set up a small function)
dn_to_toa <- function(dn, gain, offset){
  return(gain*dn+offset)
}

# DN to TOA and correcting for solar angle
TOA <- dn_to_toa(DN, REFLECTANCE_MULT_BAND, REFLECTANCE_ADD_BAND) / sin(SUN_ELEVATION_RAD)
print(TOA)

# great, we have our reflectance between 0-1; let's scale those by 10,000 to get values 
# between 0 and 10000 to be able to save the data in integer format:
TOA <- TOA*10000

# write TOA stack to disk
dir.create(file.path(dir_data, 'TOA'), showWarnings = FALSE)
writeRaster(TOA, filename = paste0(dir_data, '/TOA/', 'LC08_L1TP_189025_20141105_20170417_01_T1_TOA'), 
            format='GTiff', datatype = 'INT2S', progress='text')


# ==================================================================================================
# 2) Compare TOA with BOA
# ==================================================================================================
# a) Briefly summarize the spectral appearance of the different land cover types.
# Forest: medium values in VIS, high NIR reflectance, medium SWIR1 and SWIR2 reflectance
# Soil: low VIS values, increasing with wavelength
# Water: very low reflectance, decreasing with increasing wavelength

# b) What are the most evident differences between TOA and SR reflectance spectra?
# The TOA reflectance is generally greater than the BOA reflectance, especially in the shorter
# wavelengths (particularly blue region of electromagnetic spectrum). I.e. because processes in the 
# atmosphere (e.g. scattering) contribute to the signal in the TOA images and are corrected for in the BOA
# image.


# ==================================================================================================
# Part II - Data quality
# 1) Investigating the QA band
# ==================================================================================================

### Read QA band
qa.files <- list.files(dir_data, pattern="BQA.tif$", recursive=T, full.names=T)
qa.band <- raster(qa.files)

### What are the three most frequent values in the qa band?
value.frequencies <- freq(qa.band)
value.frequencies[order(value.frequencies[,2]),]  # ascending order based on count

# Reduced dataset
# Most frequent value: 2720
# Second most frequent value: 2800
# Third most frequent value: 2976

rev(as.numeric(intToBits(2720)[1:16]))  # Clear terrain (low cirrus, low snow, low shadow, low cloud, no sat, no terrain, no fill)
rev(as.numeric(intToBits(2800)[1:16]))  # Cloud (low cirrus, low snow, low shadow, high cloud, no sat, no terrain, no fill)
rev(as.numeric(intToBits(2976)[1:16]))  # clear, no saturation, low cl. prob., high cl. shadow prob., low snow, low cirrus

# Original dataset
# Most frequent value: 1
# Second most frequent value: 2720
# Third most frequent value: 2800

rev(as.numeric(intToBits(1)[1:16]))  # Fill / NA value
rev(as.numeric(intToBits(2720)[1:16]))  # Clear terrain (low cirrus, low snow, low shadow, low cloud, no sat, no terrain, no fill)
rev(as.numeric(intToBits(2800)[1:16]))  # Cloud (low cirrus, low snow, low shadow, high cloud, no sat, no terrain, no fill)



# ==================================================================================================
# 2) Creating a cloud mask
# ==================================================================================================

# function to get fill pixels
fill_pixels <- function(x) {((intToBits(x)[1] == T))} 

# Next, use indexing and Boolean expressions to define new functions which return TRUE for:

# a) high confidence clouds or high confidence cloud shadows or fill values
mask_highconf <- function(x){
  bs <- intToBits(x)
  return ( ((bs[1]) | (bs[6] & bs[7]) | (bs[8] & bs[9])) == T)
}

# b) high and medium confidence clouds or high and medium confidence cloud shadows or fill values
mask_medconf <- function(x){
  bs <- intToBits(x)
  return ( ((bs[1]) | (bs[7]) | (bs[9])) == T)
}

# Create a mask using the above functions in calc().
img_mask_highconf <- raster::calc(qa.band, fun=mask_highconf)
img_mask_medconf <- raster::calc(qa.band, fun=mask_medconf)

# Plot the mask and check if clouds, cloud shadows, and fill values are labeled as 1 and clear observations as 0.
plot(img_mask_highconf)
plot(img_mask_medconf)

# Discuss and decide on the appropriate data type for binary masks and write them to disk
# Given it is purely binary here we could opt for the LOG1S datatype, however GDAL
# which operates in the background does not support that, hence we use 8bit usigned instead
dir.create(file.path(dir_data, "CFmask"), showWarnings = FALSE)
writeRaster(img_mask_highconf, 
            filename = paste0(dir_data, '/CFmask/', 'LC08_L1TP_189025_20141105_20170417_01_T1_high_prob'), 
            format='GTiff', datatype = 'INT1U')
writeRaster(img_mask_medconf, 
            filename = paste0(dir_data, '/CFmask/', 'LC08_L1TP_189025_20141105_20170417_01_T1_med_prob_sat'), 
            format='GTiff', datatype = 'INT1U')

# Open both masks in QGIS, together with an RGB representation of the image. Which mask is more reliable in your opinion?
# They are both fairly similar in this case, but the medium confidence masks catches some pixels at the clouds' borders and we
# favor comission over omission errors here.


# ==================================================================================================
# 3) Masking images
# ==================================================================================================
# stack BOA bands
BOA <- stack(list.files(paste0(dir_data), pattern="sr_band[2-7].tif$", full.names=T, recursive=T))

# mask out clouds and cloud shadows
BOA_masked <- mask(BOA, img_mask_medconf, maskvalue=1)
plotRGB(BOA_masked, 6, 4, 3, stretch="hist")

writeRaster(BOA_masked, 
            filename = paste0(dir_data, '/SR/', 'LC08_L1TP_189025_20140716_20170421_01_T1_subset_sr_masked'), 
            format='GTiff', datatype = 'INT2S')

# EOF