#############################################################################
# MSc Earth Observation Assignment 4
# [Shawn Schneidereit]
#############################################################################

#############################################################################
library(rgdal)
library(raster)
library(lubridate)
library(ggplot2)
source("assignments/s04_composite_function.R") #path to the parametric_compositing function including filename.R

# In case you run into memory issues
# change raster options to store large rasters in temp files on disk
# rasterOptions(maxmemory = 1e12)

######## Define the folder that contains your data...
data.path <- 'data/gcg_eo_s04/'



#############################################################################
# 1) Setting data source
#############################################################################

sr <- list.files(paste0(data.path, 'sr_data'), pattern='.tif$', full.names=T, recursive=F)
fmask <- list.files(paste0(data.path, 'fmask'), pattern='.tif$', full.names=T, recursive=F)
cd <- list.files(paste0(data.path, 'cloud_dist'), pattern='.tif$', full.names=T, recursive=F)

sta <- nchar(paste0(data.path,'sr_data/LT05228082')) + 1
end <- sta + 6

dates <- as.Date(substr(sr, sta, end), format='%Y%j')

sr.sorted <- sr[order(dates)]
cd.sorted <- cd[order(dates)]

img_list <- data.frame('image_files'=as.character(sr.sorted), 
                       'cloud_dist_files'=as.character(cd.sorted),
                       'date'=sort(dates), 
                       'DOY'=yday(sort(dates)), 
                       'year'=year(sort(dates)))

#############################################################################
# 2) User input of equation variables 
#############################################################################
target_date_1 <- ymd('YYYYMMDD')
target_date_1 <- ymd('20150605')
target_date_2 <- ymd('YYYYMMDD')
target_date_2 <- ymd('20151610')

W_DOY <- 0.2
W_year <- 0.3
W_cloud_dist <- 0.5

max_DOY_offset <- 30
max_year_offset <- 3

min_cloud_dist <- 50
max_cloud_dist <- 100

composite_1 <- parametric_compositing(img_list, target_date_1, 
                                      W_DOY, W_year, W_cloud_dist, 
                                      max_DOY_offset, max_year_offset, 
                                      min_cloud_dist, max_cloud_dist)
writeRaster(composite_1, "~/Documents/humbolt/semester_02/earth_observation/data/gcg_eo_s04/composite_1",
            format = "GTiff", 
            datatype = "INT2S",
            overwrite = T)

#############################################################################
# 4) Visual inspection and evaluation of results
#############################################################################

# 1) What worked out well. What did not? 

# Water bodies did not consistently get displayed, indicating an error selecting
# errors when selecting pixels in the image composting 

# 2) How could the quality of the composites be improved? 

# We could select more sensible target DOY/year and max offsets for our application 
# goals

# 


