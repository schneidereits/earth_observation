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


#############################################################################
# 3) Modeling fractional cover
#############################################################################


#############################################################################
# 4) Evaluation of fractional cover
#############################################################################

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