# ==================================================================================================
# MSc Earth Observation Exercise [3]: VIs & Transform
# ==================================================================================================

# load required packages
library(raster)

# input directory
dir_data <- ''

# ==================================================================================================
# 1) Compute vegetation indices
# ==================================================================================================
# list input images
l_imgs <- list.files(
  paste0(dir_data, '/sr_data'), pattern="sr_masked_crop.tif$", recursive=T, full.names=T
)
print(l_imgs)  # investigate which index corresponds to which scene

# define functions
norm_diff <- function(x, y) {
  return((x-y)/(x+y))
}

evi <- function(nir, red, blu, g=2.5, c1=6, c2=7.5, l=1) {
  return(g * ((nir - red) / (nir + c1 * red - c2 * blu + l)))
}

# prepare SR stack (including outlier removal)
stack_march <- stack(l_imgs[[1]])
hist(stack_march)  # there are a few single outliers, i.e. reflectance greater 1
stack_march[(stack_march > 10000) | (stack_march < 0)] <- NA

# calculate EVI
evi_march <- evi(stack_march[[4]], stack_march[[3]], stack_march[[1]], l=10000)
evi_march[(evi_march < -1) | (evi_march > 1)] <- NA  # outliers in the blue that can 
# be caused by over saturation for instance will affect the result, hence we exclude
# invalid values here once more
evi_march <- evi_march*10000

# calculate NDVI
ndvi_march <- norm_diff(stack_march[[4]], stack_march[[3]])
ndvi_march <- ndvi_march*10000

# write to disc
writeRaster(evi_march, gsub(".tif", "_EVI.tif", l_imgs[[1]]), datatype='INT2S')
writeRaster(ndvi_march, gsub(".tif", "_NDVI.tif", l_imgs[[1]]), datatype='INT2S')

# ==================================================================================================
# 2) Perform a Tasseled Cap transformation
# ==================================================================================================
tcc <- matrix(c( 0.2043,  0.4158,  0.5524, 0.5741,  0.3124,  0.2303, 
                 -0.1603, -0.2819, -0.4934, 0.7940, -0.0002, -0.1446,
                 0.0315,  0.2021,  0.3102, 0.1594, -0.6806, -0.6109),
              dimnames = list(c('blue', 'green', 'red', 'nIR', 'swIR1', 'swIR2'), 
                              c('bright', 'green', 'wet')),
              ncol = 3)

# apply element (band) wise multiplication (i.e. df columns x bands) and sum
tcb <- sum(stack_march * tcc[,1])
tcg <- sum(stack_march * tcc[,2])
tcw <- sum(stack_march * tcc[,3])

stack_tc_march <- stack(c(tcb, tcg, tcw))
plotRGB(stack_tc_march, stretch="hist")

# write to disc
writeRaster(stack_tc_march, gsub(".tif", "_TC.tif", l_imgs[[1]]), datatype='INT2S')


