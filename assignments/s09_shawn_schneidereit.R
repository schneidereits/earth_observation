#############################################################################
# MSc Earth Observation Assignment 9
# [Shawn Schneidereit]
#############################################################################

#############################################################################
# Library and data import
#############################################################################

pck_list <- c("assertthat","ggplot2", "tidyr", "dplyr","raster", "e1071", "rgdal", "viridisLite")
lapply(pck_list, require, character.only = TRUE)


#############################################################################
# 1) Linear spectral mixtures
#############################################################################

library(readr)
sli <- read.csv("data/gcg_eo_s09/S2_EM.txt")
sli_snythmix <- sli
head(sli)
sli$wavelength <- c(493, 560, 665, 704, 740, 783, 883, 865, 1610, 2190)
sli_long <- sli %>% 
  pivot_longer(names_to = "type", values_to = "reflectance", cols = c(2:5))


ggplot(sli_long, aes(x=wavelength, y= reflectance, color=type)) +
  geom_line() +
  theme_classic()
  
ggplot(sli_long, aes(x=type, y= reflectance, color=type)) +
  geom_boxplot()+
  theme_classic()

# How do the spectra differ between the classes and why?

# The spectral classes differ significantly between each other, the most 
# obvious being shade that has a very low reflectance of 100 across all wavelengths.
# soil has the lowest reflectance out of the remaining three classes
# PV has a highly variable spectral signature, with high reflectance values in
# the near IR (due to vegetative material reflectance in this region)


# Let´s now model a "mixed pixel". ----
# We can simply do this be defining proportions of the different surface components.
fraction_PV <- 0.1
fraction_NPV <- 0.8
fraction_soil <- 0.05
fraction_shade <- 0.05

# Do we violate the assumption that all surface components represent 100% of the surface area?
if((fraction_PV + fraction_NPV + fraction_soil+ fraction_shade) != 1) print('Fractions don´t sum to 1.')

# Create a linear mixture of the endmember spectra, based on the defined proportions.
model_spectrum <- fraction_PV * sli$PV + 
  fraction_NPV * sli$NPV +
  fraction_soil * sli$soil + 
  fraction_shade * sli$shade

# We could simulate imperfect measurements by adding random noise.
noise <- rnorm(10, mean=0, sd=0.02)

# Append the modeled spectrum to the endmembers data.frame
sli$model_spectrum <- model_spectrum + noise

# Convert the spectra into long format for plotting with ggplot2
sli_vis <- pivot_longer(sli, -c(band, wavelength)) 

# Visualize the modeled spectrum in comparison to the endmember spectra
ggplot(sli_vis) + 
  geom_line(aes(x=wavelength, y=value, color=name, linetype=name))+
  scale_color_manual(values=c("black", "steelblue", "darkgreen", "darkgrey","firebrick"), name="Spectrum")+
  scale_linetype_manual(values=c("dotdash", rep("solid", 4)), name="Spectrum")+
  scale_y_continuous(labels=seq(0,1,0.1), breaks=seq(0,10000,1000))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  labs(x="Wavelength (nm)", y="Reflectance")


#############################################################################
# 2) Generate synthetic training data using the spectral library
#############################################################################

synthMix <- function(
  sli, # spectral library dataframe containing 
  # spectra (columns) and reflectances (rows)
  n_mix, # desired number of mixtures
  mix_complexity, # numbers of spectra within a mixture
  # vector, same length as mix_likelihood
  mix_likelihood, # proportion of mixtures with respective complexity as
  # vector, same length as mix_complexity, must sum to 1
  target_class, # mixtures will be calculated for this class
  other_classes){ # other classes which should be included in the mixtures
  
  assert_that(sum(mix_likelihood)==1) 
  
  # total number of classes
  em_total <- length(c(target_class, other_classes)) 
  
  # empty dataframe to store training data
  ds_training <- setNames(data.frame(matrix(ncol = nrow(sli) + 1, nrow = 0)), 
                          c(paste("B", c(1:nrow(sli)), sep = ""), "fraction")) 
  
  # iterator for generating each synthetic mixture 
  for (i in 1:n_mix) { 
    
    if(length(mix_likelihood) ==1){
      n_em = mix_complexity
    }
    else{
      # sample mixing complexity based on mixing likelihoods
      n_em = sample(as.vector(mix_complexity), 
                    size = 1, 
                    prob = as.vector(mix_likelihood)) 
    }
    # select EMs which will be included in the mixture
    sample_em <- sample(other_classes, n_em - 1) 
    
    # get target EM and mixing EMs from SLI
    df <- sli[, c(target_class, sample_em)] 
    
    #randomly sample weight for target endmember including 0
    w1 <- (sample.int(10001, size = 1) - 1) 
    
    # calculate weight for remaining endmember 
    if (n_em == 2) {
      w2 <- 10000 - w1
      ws = c(w1, w2)
      
      assert_that(sum(ws)==10000)
      
    }
    
    # sample weights for two other endmembers
    if (n_em == 3) { 
      # remaining weight
      wr = (10000 - w1) 
      # randomly sample weight for second endmember including 0
      w2 = (sample.int(wr + 1, size = 1) - 1) 
      w3 = 10000 - (w1 + w2)
      ws = c(w1, w2, w3)
      
      w_sum <- sum(ws)
      
      assert_that(w_sum==10000)
      
    }
    # scale weights between 0 and 1
    ws <- ws/10000 
    # multiply spectra by their weight and 
    # calculate mixed spectrum as sum of weighted reflectances
    mixture = rowSums(t(t(df) * ws)) 
    
    # add weight (i.e. the cover fraction)
    train_mix <- c(mixture, ws[1]) 
    # turn  into dataframe
    train_mix <- as.data.frame(t(train_mix)) 
    colnames(train_mix) <- c(paste("B", c(1:nrow(sli)), sep = ""), "fraction") 
    
    # add mixed spectrum to training dataset
    ds_training <- rbind(ds_training, train_mix) 
    
  }
  # add original library spectra to the training data
  # vector of target class spectrum and cover fraction of 1
  em_target <- c(sli[, target_class], 1) 
  
  # all other class spectra get a cover value of 0 for the target class 
  em_rest <- data.frame((t(sli[, other_classes])), 
                        "fraction"=rep(0,length(other_classes))) 
  
  em_set <- rbind(em_target, em_rest)
  rownames(em_set) <- NULL
  colnames(em_set) <- c(paste("B", c(1:nrow(sli)), sep=""),"fraction")
  
  ds_training <- rbind(ds_training, em_set)
  
  return(ds_training)
  
}


mix_NPV <- synthMix(sli_snythmix, 
                    n_mix=1000, 
                    mix_complexity=(c(2,3)),
                    mix_likelihood=c(0.5, 0.5),
                    target_class = "NPV",
                    other_classes = c("PV", "soil","shade"))

mix_PV <- synthMix(sli_snythmix, 
                    n_mix=1000, 
                    mix_complexity=(c(2,3)),
                    mix_likelihood=c(0.5, 0.5),
                    target_class = "PV",
                    other_classes = c("NPV", "soil","shade"))

mix_soil <- synthMix(sli_snythmix, 
                   n_mix=1000, 
                   mix_complexity=(c(2,3)),
                   mix_likelihood=c(0.5, 0.5),
                   target_class = "soil",
                   other_classes = c("NPV", "PV","shade"))



#############################################################################
# 3) Modeling fractional cover
#############################################################################

# What do the parameters mean?

# Gamma defines the curvature allowed to be assumed by the vector line
# used by the SVM to fit the training data

# The cost parameter defines the weighted cost of missclassifing a point, 
# and thus determines the stringency/margin of error that can be tolerated. 

# Epsilon defines a "buffer" a range at which errors can be included in the
# SVM, before a cost penalty is applied

library(e1071)

# Define accuracy from 10-fold cross-validation as optimization measure
cv <- tune.control(cross = 10) 

# Use tune.svm() for a grid search of the gamma and cost parameters

svm_test_NPV <- svm(fraction~., 
           data = mix_NPV, 
           kernel = 'radial',
           gamma = 1, 
           cost = 1, 
           epsilon = 0.001,
           tunecontrol = cv)

svm_test_PV <- svm(fraction~., 
                    data = mix_PV, 
                    kernel = 'radial',
                    gamma = 1, 
                    cost = 1, 
                    epsilon = 0.001,
                    tunecontrol = cv)

svm_test_soil <- svm(fraction~., 
                    data = mix_soil, 
                    kernel = 'radial',
                    gamma = 1, 
                    cost = 1, 
                    epsilon = 0.001,
                    tunecontrol = cv)

svm.tune <- tune.svm(fraction~., 
                     data = mix_NPV, 
                     kernel = 'radial', 
                     gamma = (0.01:100), 
                     cost = 10^(-2:2), 
                     epsilon = 0.001,
                     tunecontrol = cv)

# Store the best model in a new object
svm.best <- svm.tune$best.model

# Which parameters performed best?
print(svm.best$gamma)
print(svm.best$cost)

s2 <- stack("data/gcg_eo_s09/2018-2019_091-319_HL_TSA_SEN2L_TSS_20190726_SEN2A_crop.tif")
names(s2) <- paste("B", seq(1,10), sep="")
plot(s2)

prediction_NPV <- predict(s2, svm_test_NPV)
plot(prediction_NPV)

prediction_PV <- predict(s2, svm_test_PV)
plot(prediction_PV)

prediction_soil <- predict(s2, svm_test_soil)
plot(prediction_soil)

writeRaster(prediction_NPV, "data/gcg_eo_s09/prediction_NPV.tif", 
            datatype="INT1S", 
            overwrite=T)

# Based on this, can you identify fields in different stages of the crop 
# phenology?

# Based on this we can only easy identify crops in the senescence phenological
# stage, or that have already been harvested. The other colored areas could be a 
# mix of soil or photosynthetic vegetation. 
  
  
#############################################################################
# 4) Evaluation of fractional cover
#############################################################################

reference_data <- readOGR("data/gcg_eo_s09/s09_validation/Validation_scaled_20190726.shp")
prediction_stack <-  (stack(c(prediction_NPV, prediction_PV, prediction_soil)))
plot(prediction_stack)

# extracting our reference data points from prediction_NPV
predictions_stack <- raster::extract(prediction_stack, reference_data, sp=TRUE)
# nothing is negative or above one, so no adjustemts are needed here
predictions_df <- as.data.frame(predictions_stack) %>% 
  rename(NPV_predict = layer.1,
         PV_predict = layer.2,
         soil_predict = layer.3) %>% 
  mutate(total_cover = NPV_predict + PV_predict + soil_predict)

p1 <- ggplot(predictions_df, aes(NPV_predict, NPV_val)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

p2 <- ggplot(predictions_df, aes(PV_predict, PV_val)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

p3 <- ggplot(predictions_df, aes(soil_predict, soil_val)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

library("gridExtra") 
grid.arrange(p1,p2,p3)

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

m1 <- lm(data = predictions_df, NPV_val ~ NPV_predict)
summary(m1)
# R2 0.6431 
error <- predictions_df$NPV_val - predictions_df$NPV_predict
error <-  na.omit(error)
unique(is.na(error))

(mae <- abs(mean(error))) #0.01277667

(rmse <- sqrt(mean(error^2))) # 0.1276346

m2 <- lm(data = predictions_df, PV_val ~ PV_predict)
summary(m2)
# R2 0.8933 
error <- predictions_df$PV_val - predictions_df$PV_predict
error <-  na.omit(error)
unique(is.na(error))

(mae <- abs(mean(error))) #0.03547932

(rmse <- sqrt(mean(error^2))) # 0.09200218

m3 <- lm(data = predictions_df, soil_val ~ soil_predict)
summary(m3)
# R2 0.6522 
error <- predictions_df$soil_val - predictions_df$soil_predict
error <-  na.omit(error)
unique(is.na(error))

(mae <- abs(mean(error))) #0.1728898

(rmse <- sqrt(mean(error^2))) # 0.2090371

# You can use this function to calculate the simple linear regression coefficients 
# of observed and predicted data and annotate it in your plots
lm_eqn <- function(pred_x,obs_y,df){
  m <- lm(obs_y ~ pred_x, df);
  eq <- substitute(y ==  a + b * x,
                   list(a = format(as.numeric(round(coef(m)[1]/100,2))),
                        b = format(as.numeric(round(coef(m)[2],2)))))
  as.character(as.expression(eq));
}

# For a better visualization, you can use this function to calculate the point
# density and display it as the color in your scatterplots:
get_density <- function(x, y,...) { # set x and y to predicted and observed values and n = 100
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

lm_eqn(predictions_df$NPV_predict, predictions_df$NPV_val, predictions_df)

get_density()


