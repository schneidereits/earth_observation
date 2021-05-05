# ==================================================================================================
# MSc Earth Observation Exercise [1]: Rasters in R
# ==================================================================================================

# Load packages, use install.packages('packagename') to install if needed
library(raster)
library(ggplot2)

# input directory 
dir_data <- ''  # path to 'gcg_eo_s01' folder



# ==================================================================================================
# 2) Reading Data
# ==================================================================================================

# list scene directories
dir_scenes <- list.files(dir_data, pattern="LC08", full.names = F)

# list band files and stack to image scenes
# only the following bands: blue, green, red, near infrared, shortwave infrared 1, shortwave infrared 2

# There are multiple ways of approaching this. We can list all files and assign them to a list object and filter out manually
# which items we want to have, e.g:
all_files <- list.files(paste0(dir_data, "/", dir_scenes[1]), full.names = TRUE) 
print(all_files)
bands_scene_one <- all_files[seq(5, 10)]  # 5th to 10th item

# alternatively we make use f the "pattern" argument inside the "list.files"-function and specify directly the "regular expression"
# we are looking for, i.e. only Band 2-7:
bands_scene_one <- list.files(paste0(dir_data, "/", dir_scenes[1]), pattern = "*_B[2-7].TIF$", full.names = TRUE) 
# this is more elegant and general because the indices in the list object above (seq(5, 10)) may be very different from time to time

# Have a look at the cheat sheet for all sorts of regular expressions used to maniulate string objects: 
# https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf

bands_scene_two <- list.files(paste0(dir_data, "/", dir_scenes[2]), pattern = "*_B[2-7].TIF$", full.names = TRUE) 

# stack the single .tif files into a raster stack by providing the list object to the raster::stack function
scene_one <- stack(bands_scene_one)
scene_two <- stack(bands_scene_two)



# ==================================================================================================
# 3) Manipulating data
# ==================================================================================================

# Investigate the stack. In which projection is the data delivered?
print(projection(scene_one))
print(projection(scene_two))
# -> Both are delivered in the UTM projection of Zone 34 using the WGS84 ellispoid

# trying to stack images... 
image.stack <- stack(image.one, image.two)
# ... results in error message. -> The two scenes are not fully aligned

# Find an efficient way to identify the common extent of the images, defined as common.extent <- c(xmin, xmax, ymin, ymax)
ext_scene_one <- extent(scene_one)
ext_scene_two <- extent(scene_two)

# option 1: calculate it manually ...
xmin <- max(ext_scene_one[1], ext_scene_two[1])
xmax <- min(ext_scene_one[2], ext_scene_two[2]) 
ymin <- max(ext_scene_one[3], ext_scene_two[3])
ymax <- min(ext_scene_one[4], ext_scene_two[4])

common.extent <- extent(xmin, xmax, ymin, ymax)  # ... and create an extent object out of it

# option 2
common.extent <- intersect(ext_scene_one, ext_scene_two)

# In order to reduce the amount of data for the next steps, please crop() the images to our region of interest, i.e.
# a part of the Western Beskids, defined by the following coordinates:
roi <- c(327945, 380325, 5472105, 5521095)

# crop to ROI
scene_one_roi <- crop(scene_one, roi)
scene_two_roi <- crop(scene_two, roi)



# ==================================================================================================
# 4) Writing data
# ==================================================================================================
# create directory in which to store results
dir.create(file.path(dir_data, "subset"), showWarnings = FALSE)

# helper for datatype:
dataType(scene_one)
# --> stored as INT2U, i.e. unsigned 16bit integer (0, 65534)

writeRaster(scene_one_roi, filename = paste0(dir_data, '/subset/', dir_scenes[1], '_subset'), format='GTiff',
            datatype = 'INT2U', overwrite=T)

writeRaster(scene_two_roi, filename = paste0(dir_data, '/subset/', dir_scenes[2], '_subset'), format='GTiff',
            datatype = 'INT2U', overwrite=T)



# ==================================================================================================
# 5) Plotting image data
# ==================================================================================================
# --> QGIS
plotRGB(scene_one_roi, r=4, g=3, b=2, stretch="hist")  # NIR-R-G
plotRGB(scene_one_roi, r=6, g=4, b=3, stretch="hist")  # SW2-NIR-R



# ==================================================================================================
# 6) Extracting spectral profiles
# ==================================================================================================
# create multitemporal stack, i.e. first 6 bands correspond to scene 1, the last 6 to scene 2
multitemp.stack <- stack(scene_one_roi, scene_two_roi)

# Define coordinate (units = m)
coordinate <- data.frame('x' = 355623, 'y' = 5486216) 

# Extract cell values from image_stack
cellvals <- extract(multitemp.stack, coordinate)

profile <- data.frame('wavelength'=rep(c(515, 600, 680, 885, 1660, 2300),2), 
                      'date'=as.factor(c(rep('10 March', 6), rep('10 July', 6))),
                      'values'=as.numeric(cellvals))

print(profile)

# Plot spectral profiles as line plot, grouped by factor date
ggplot(profile, aes(x=wavelength, y=values, group=date)) + 
  geom_line(aes(color=date)) + 
  # Add axis & legend names
  scale_y_continuous(name='DN') + 
  scale_x_continuous(name='Wavelength (nm)') + 
  scale_colour_manual(name='Acquisition DOY', values=c('darkgreen', 'brown')) +
  theme_bw() # Chose black/white layout


# Can you guess what type of surface we are looking at?
# Both spectral profiles show a distinct pattern common for vegetation, most prominently the sharp increase
# from red to nir (red-edge). The difference between the two profiles further suggest a strong seasonal component, e.g.
# observed for deciduous vegetation or even human interaction showing cropland.


# EOF