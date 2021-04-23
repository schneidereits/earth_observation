##############################################################################
# MSc Earth Observation Assignment [Session 1]
# [Shawn Schneidereit]
#############################################################################

# Load the package ----
library(raster)
library(rgdal)


# Reading data
#############################################################################
# create raster stack

data_20140310 <- stack(paste0("gcg_eo_s01/LC08_L1TP_189025_20140310_20170425_01_T1/LC08_L1TP_189025_20140310_20170425_01_T1_B", c(2:7), ".TIF"))

data_20140716 <- stack(paste0("gcg_eo_s01/LC08_L1TP_189025_20140716_20170421_01_T1/LC08_L1TP_189025_20140716_20170421_01_T1_B", c(2:7), ".TIF"))


#############################################################################


# 3) Manipulating data
#############################################################################


image.stack <- stack(data_20140310, data_20140716) # Here we see that our extents are different

roi.extent <- c(327945, 380325, 5472105, 5521095)

data_20140310 <- crop(data_20140310, roi.extent)

data_20140716 <- crop(data_20140716, roi.extent)

image.stack <- stack(data_20140310, data_20140716) # Now it works :)

#############################################################################



# 4) Writing data
#############################################################################

raster_20140310 <- writeRaster(data_20140310, "gcg_eo_s01/LC08_L1TP_189025_20140310_20170425_01_T1/raster_20140310.TIF",
                               format = "GTiff", overwrite = T)

raster_20140716 <- writeRaster(data_20140716, "gcg_eo_s01/LC08_L1TP_189025_20140310_20170425_01_T1/raster_20140716.TIF",
                               format = "GTiff", overwrite = T)

raster_image.stack <-  writeRaster(image.stack, "gcg_eo_s01/LC08_L1TP_189025_20140310_20170425_01_T1/raster_image.stackTIF",
                                   format = "GTiff", overwrite = T)
# When unspecified, write raster defaults to the native file formats (.grd/.gri files),
# which can influence how values are read from the file. There we need to specify
# format = "GTiff", as this this forces the file to be read via the rgdal package,
# ensuring that values will be consistent. 


#############################################################################


# 5) Plotting image data
#############################################################################

# single images
plotRGB(raster_20140310, r=5, g=4, b=3, stretch = "hist")
plotRGB(raster_20140716, r=5, g=4, b=3, stretch = "hist")

# combined stack
plotRGB(raster_image.stack, r=5, g=4, b=3, stretch = "hist")

#############################################################################


#6) Extracting spectral profiles
#############################################################################

# Define coordinate (units = m)
coordinate <- data.frame('x' = 355623, 'y' = 5486216) 

# Extract cell values from single images
cellvals_20140310 <- extract(raster_20140310, coordinate)
cellvals_20140716 <- extract(raster_20140716, coordinate)

# Extract cell values from image_stack
cellvals_image.stack <- extract(raster_image.stack, coordinate)


#############################################################################


# 7) Plotting a spectral profile
#############################################################################

# Load ggplot2 package (or install if needed)
library(ggplot2)

# Create a data frame combining extracted values with 
# band wavelengths and acquisition date 
profile <- data.frame('wavelength'=rep(c(482, 561, 655, 865, 1609, 2201),2), 
                      'date'=as.factor(c(rep('10 March', 6), rep('10 July', 6))),
                      'values'=as.numeric(cellvals_image.stack))

print(profile)

# Plot spectral profiles as line plot, grouped by factor date
ggplot(profile, aes(x=wavelength, y=values, group=date)) + 
  geom_line(aes(color=date)) + 
  # Add axis & legend names
  scale_y_continuous(name='DN') + 
  scale_x_continuous(name='Wavelength (nm)') + 
  scale_colour_manual(name='Acquisition DOY', values=c('darkgreen', 'brown')) +
  theme_bw() # Chose black/white layout


# Based on the stark contrast of the observed digital number values between 
# march and July. it is possible that we are looking at some form of vegetation?
# At march the leaf up phenological phase would have not started yet, meaning that 
# background surfaces such as ground, rocks, dead foliage would be visible that 
# have different spectral signatures to vegetation. In July, plants would have full foliage, which could
# explain the higher DN values? I would expect this to influence DN values more in the visible spectrum
# les in the NIR and IR, so overall I`m` only lukewarm on this idea...

#############################################################################

