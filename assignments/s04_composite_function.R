#############################################################################
# MSc Earth Observation Exercise 4
# Function for creating cloud-free composites of multiple Landsat images
# Requires an input data.frame (here img_list) and eight compositing 
# parameters. Please see exercise sheet for further details.
#############################################################################

#############################################################################
# Loading required packages here...
library(raster)
library(lubridate)
library(ggplot2)

# Change raster options to store large rasters in temp files on disk
# Only use if function fails due to memory issues!
# rasterOptions(maxmemory = 1e12)

#############################################################################
# Function definition starts here
parametric_compositing <- function(img_list, target_date, 
                                   W_DOY, W_year, W_cloud_dist, 
                                   max_DOY_offset, max_year_offset, 
                                   min_cloud_dist, max_cloud_dist) {
  #...
  tic <- Sys.time()
  print(paste('Start of compositing process: ', tic))
  print(paste('Target date: ', target_date))
  
  # Extract target DOY and year from target_date
  target_DOY <- yday(target_date)
  target_year <- year(target_date)
  
  #...
  if(sum(W_DOY, W_year, W_cloud_dist)!=1) { stop('Error: something wrong.') }
  
  #############################################################################
  # Calculate the scores for the DOY, year, and cloud distance criteria
  print('Calculating compositing scores')
  
  ### DOY
  #...
  img_list$DOY_score <- exp(-0.5 * ((img_list$DOY - target_DOY) / (max_DOY_offset/3))^2)
  
  # Remove DOYs outside valid range
  img_list$DOY_score[abs(img_list$DOY-target_DOY)>max_DOY_offset] <- NA
  
  # Create data frame with daily score values 
  score_fct <- data.frame('DOY' = c(1:365),
                          'offset' = abs(target_DOY - c(1:365)),
                          'score' = exp(-0.5 * ((c(1:365) - target_DOY) / (max_DOY_offset/3))^2))
  
  ### Year
  #...
  img_list$year_score <- 1-(abs(target_year - img_list$year) / max_year_offset)
  
  if (max_year_offset == 0) {img_list$year_score[img_list$year==target_year] <- 1}
  img_list$year_score[img_list$year_score < 0] <- NA
  
  # Visualize DOY scoring function and available images
  p <- ggplot(score_fct, aes(x=DOY, y=score)) +
    geom_line() +
    geom_point(data=img_list, aes(x=DOY, y=DOY_score, color=as.factor(year))) +
    scale_y_continuous('Score') +
    scale_x_continuous('DOY') +
    scale_color_discrete('Acquisition years') +
    theme_bw()
  print(p)  
  
  # Get candidate images within max_DOY_offset and max_year_offset
  ix <- which(!is.na(img_list$DOY_score) & !is.na(img_list$year_score))
  if (length(ix)>1) { print(paste(length(ix),  'candidate images selected, calculating scores.')) }
  if (length(ix)<2) { stop('Another error because something is wrong.') }
  
  # Stack cloud distance layers of candidate images and reclassify 
  # values < min_cloud_dist to NA, and values > max_cloud_dist to max_cloud_dist
  print('Reclassifying cloud distances')
  cloud_dist <- stack(as.character(img_list$cloud_dist_files[ix]))
  cloud_dist <- reclassify(cloud_dist, rcl=c(0, min_cloud_dist, NA), right=NA, datatype='INT2S')
  cloud_dist <- reclassify(cloud_dist, rcl=c(max_cloud_dist, sqrt(nrow(cloud_dist)^2 + ncol(cloud_dist)^2), max_cloud_dist), right=NA, datatype='INT2S')
  
  #...
  cloud_score <- (cloud_dist - min_cloud_dist) / (max_cloud_dist - min_cloud_dist)
  
  #...
  obs_score <- img_list$DOY_score[ix] * W_DOY + img_list$year_score[ix] * W_year + cloud_score * W_cloud_dist
  
  #...
  select <- which.max(obs_score)
  
  #...
  candidates <- unique(select)
  
  #############################################################################
  # Fill composite image with pixels from the candidate images
  for (i in candidates){
    
    #...
    fill_image <- brick(as.character(img_list$image_files[ix[i]]), datatype='INT2S')
    
    #...
    if (i == min(candidates)) { 
      composite <- brick(fill_image, values=FALSE) 
      dataType(composite) <- 'INT2S'
      values(composite) <- 0
    }
    
    print(paste0('Filling raster with acquisition from date ', img_list$date[ix[i]]))
    fill_image.masked <- mask(fill_image, select, maskvalue=i, inverse=T, updatevalue=0, datatype='INT2S')
    fill_image.masked[is.na(fill_image.masked)] <- 0
    composite <- composite + fill_image.masked
    
  }
  
  #...
  composite_na <- mask(composite, select, maskvalue=NA, datatype='INT2S')
  
  #############################################################################
  #...
  print(paste('Remaining NAs: ', round(freq(composite_na[[1]], value=NA)/ncell(composite_na[[1]])*100, digits=3), ' %'))
  
  #...
  rcl_DOY <- matrix(ncol=2, data=c(candidates, img_list$DOY[ix[candidates]]))
  select_DOY <- reclassify(select, rcl_DOY, datatype = 'INT2S')
  
  #...
  rcl_year <- matrix(ncol=2, data=c(candidates, img_list$year[ix[candidates]]))
  select_year <- reclassify(select, rcl_year)
  
  #...
  output <- stack(composite_na, select_DOY, select_year)
  print(paste('End of compositing process: ', Sys.time()))
  
  #...
  return(output)
  
}