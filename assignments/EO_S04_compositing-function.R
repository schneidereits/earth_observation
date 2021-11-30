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



#############################################################################
# Function definition starts here
parametric_compositing <- function(img_list, target_date, 
                                   W_DOY, W_year, W_cloud_dist, 
                                   max_DOY_offset, max_year_offset, 
                                   min_cloud_dist, max_cloud_dist) {
  # inform user about start of process, providing time and selected target date information
  tic <- Sys.time()
  print(paste('Start of compositing process: ', tic))
  print(paste('Target date: ', target_date))
  
  # Extract target DOY and year from target_date
  target_DOY <- yday(target_date)
  target_year <- year(target_date)
  
  # check that weighted sums are equal to 1
  if(sum(W_DOY, W_year, W_cloud_dist)!=1) { stop('Error: Specified weights != 1.') }
  
  #############################################################################
  # Calculate the scores for the DOY, year, and cloud distance criteria
  print('Calculating compositing scores.')
  
  ### DOY
  print('1/3: DOY score.')
  # calculate DOY score according to a Gaussian distribution with mean target_DOY and stdDev max_DOY_offset/3
  img_list$DOY_score <- exp(-0.5 * ((img_list$DOY - target_DOY) / (max_DOY_offset/3))^2)
  
  # Remove DOYs outside valid range and set to NA
  img_list$DOY_score[abs(img_list$DOY-target_DOY)>max_DOY_offset] <- NA
  
  # Create data frame with daily score values 
  score_fct <- data.frame('DOY' = c(1:365),
                          'offset' = abs(target_DOY - c(1:365)),
                          'score' = exp(-0.5 * ((c(1:365) - target_DOY) / (max_DOY_offset/3))^2))
  
  ### Year
  # calculate YEAR score 
  print('2/3: YEAR score.')
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
  if (length(ix)<2) { stop('Error: User input requirements (DOY & YEAR) do not allow for sufficient (>1) selection of images.') }
  
  # Stack cloud distance layers of candidate images and reclassify 
  # values < min_cloud_dist to NA, and values > max_cloud_dist to max_cloud_dist
  print('Reclassifying cloud distances')
  cloud_dist <- stack(as.character(img_list$cloud_dist_files[ix]))
  cloud_dist <- reclassify(cloud_dist, rcl=c(0, min_cloud_dist, NA), right=NA, datatype='INT2S')
  cloud_dist <- reclassify(cloud_dist, rcl=c(max_cloud_dist, sqrt(nrow(cloud_dist)^2 + ncol(cloud_dist)^2), max_cloud_dist), right=NA, datatype='INT2S')
  
  # calculate CLOUD score 
  print('3/3: CLOUD score.')
  cloud_score <- (cloud_dist - min_cloud_dist) / (max_cloud_dist - min_cloud_dist)
  
  # Calculate weighted sum of individual scores to retrieve overall image score
  print('Calculating overall image scores.')
  obs_score <- img_list$DOY_score[ix] * W_DOY + img_list$year_score[ix] * W_year + cloud_score * W_cloud_dist
  
  # Retrieve per pixel index of most suited image (i.e. maximum score)
  select <- which.max(obs_score)
  
  # Retrieve images (their index in list) that were chosen for compositing 
  candidates <- unique(select)
  print(paste("Using", length(candidates), "of", length(ix), "images initially selected"))
  
  #############################################################################
  # Fill composite image with pixels from the candidate images
  print('Filling raster with acquisition from date:')
  for (i in candidates){
    
    # load image into raster object
    fill_image <- brick(as.character(img_list$image_files[ix[i]]), datatype='INT2S')
    
    # initial iteration: create empty raster object to fill with values
    if (i == min(candidates)) { 
      composite <- brick(fill_image, values=FALSE) 
      dataType(composite) <- 'INT2S'
      values(composite) <- 0
    }
    
    print(paste0(i, ' of ', length(candidates), ': ', img_list$date[ix[i]]))
    fill_image.masked <- mask(fill_image, select, maskvalue=i, inverse=T, updatevalue=0, datatype='INT2S')
    fill_image.masked[is.na(fill_image.masked)] <- 0
    composite <- composite + fill_image.masked
    
  }
  
  # mask composite (0 -> NA) where no candidate image was selected
  composite_na <- mask(composite, select, maskvalue=NA, datatype='INT2S')
  
  #############################################################################
  # Provide information on the remaining NAs in the output composite
  print(paste('Remaining NAs: ', round(freq(composite_na[[1]], value=NA)/ncell(composite_na[[1]])*100, digits=3), ' %'))
  
  # create layer with respective DOY used per pixel
  rcl_DOY <- matrix(ncol=2, data=c(candidates, img_list$DOY[ix[candidates]]))
  select_DOY <- reclassify(select, rcl_DOY, datatype = 'INT2S')
  
  # create layer with respective YEAR used per pixel
  rcl_year <- matrix(ncol=2, data=c(candidates, img_list$year[ix[candidates]]))
  select_year <- reclassify(select, rcl_year)
  
  # create final raster object with composite and metadata (DOY; YEAR)
  output <- stack(composite_na, select_DOY, select_year)
  print(paste('End of compositing process: ', Sys.time()))
  
  # return composite image 
  return(output)
  
}

# EOF
