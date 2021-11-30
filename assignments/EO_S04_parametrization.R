# ==================================================================================================
# MSc Earth Observation Exercise [4]: Pixel-based compositing
# ==================================================================================================
library(rgdal)
library(raster)
library(lubridate)
library(ggplot2)
source("") # path to the parametric_compositing function including filename.R


# Define the folder that contains your data...
dir_data <- ""
setwd(dir_data)


# ==================================================================================================
# 1) Parameterization
# ==================================================================================================

# list input files: surface reflectance, image masks and cloud distance layers
sr <- list.files('sr_data', pattern='.tif$', full.names=T, recursive=F)
fmask <- list.files('fmask', pattern='.tif$', full.names=T, recursive=F)
cd <- list.files('cloud_dist', pattern='.tif$', full.names=T, recursive=F)

# define the index positions at which the date information can be found in the file strings ...
sta <- 19
end <- sta + 6
# ... and extract and cast them to date objects
dates <- as.Date(substr(sr, sta, end), format='%Y%j')
print(dates)

# sort input files according to date
sr_sorted <- sr[order(dates)]
cd_sorted <- cd[order(dates)]

# create list that holds the input imagery and associated time information
img_list <- data.frame('image_files'=as.character(sr_sorted), 
                       'cloud_dist_files'=as.character(cd_sorted),
                       'date'=sort(dates), 
                       'DOY'=yday(sort(dates)), 
                       'year'=year(sort(dates)))
print(img_list)


# ==================================================================================================
# 2) Defining target dates and parameters
# ==================================================================================================
# define two target days of the year (DOY) to capture contrasting phenological stages of the different forest types

target_date_1 <- ymd('20150401')  # spring imagery 
target_date_2 <- ymd('20150715')  # peak summer 

# define weights 
W_DOY <- 0.6  # most important weight given we aim at seasonal consistency
W_year <- 0.2  # we add some weight to the year to accoutn for annual weather patterns and potential LC change
W_cloud_dist <- 0.2  # we assume our cloud masks to be rigorous, but nevertheless favor a larger distance

max_DOY_offset <- 45  # +- target_DOY that is still considered as valid imagery. Our DOY score is already relatively high to
                      # favor seasonal consistency; accordingly, we broaden our window to rather have a seamless composite 
                      # than many no data gaps (this highly depend upon data availability!)
max_year_offset <- 1  # we have imagery from 2014-2016, we incorporate all of them

min_cloud_dist <- 3  # 90m
max_cloud_dist <- 48  # 1440m

composite_1 <- parametric_compositing(img_list, target_date_1, 
                                      W_DOY, W_year, W_cloud_dist, 
                                      max_DOY_offset, max_year_offset, 
                                      min_cloud_dist, max_cloud_dist)

composite_2 <- parametric_compositing(img_list, target_date_2, 
                                      W_DOY, W_year, W_cloud_dist, 
                                      max_DOY_offset, max_year_offset, 
                                      min_cloud_dist, max_cloud_dist)

writeRaster(composite_1, paste0("composites/PBC_LND_", toString(format(target_date_1, "%Y%m%d")), "-", max_DOY_offset, ".tif"),
            format="GTiff", datatype='INT2S', overwrite=TRUE)

writeRaster(composite_2, paste0("composites/PBC_LND_", toString(format(target_date_2, "%Y%m%d")), "-", max_DOY_offset, ".tif"),
            format="GTiff", datatype='INT2S', overwrite=TRUE)

# ==================================================================================================
# 3) Exploring the script and adding documentation
# ==================================================================================================

# --> EO_S04_compositing-function.R


















