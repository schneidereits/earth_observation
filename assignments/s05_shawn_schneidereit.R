#############################################################################
# MSc Earth Observation Assignment 5
# [Shawn Schneidereit]
#############################################################################

#############################################################################
# Library and data import
#############################################################################



#############################################################################
# 1) Training a Random Forest model
#############################################################################

# Load the vector file containing your training data points using readOGR().

# Next, create a stack() / brick() of your favourite pixel-based composite (last week´s result).

# Use extract() to create a data.frame with training points as rows, and class labels 
# (classID) as well as the spectral bands of your composites as columns. 
# Remove the day of year and year flags (band 7 and 8) for the next steps.

# As we want to train a classification (and not a regression), the randomForest() 
# function expects the dependent variable to be of type factor. 
# Use as.factor() for conversion of the classID column. The RF algorithm cannot 
# deal with NoData (NA) values. Remove NAs from the data.frame.

# Train a randomForest() classification model with the data.frame created in the 
# prior step. Make sure to include only useful predictors.

# Repeat the RF training procedure and produce additional model objects. 
# Use i) the other pixel-based composite, 
# and ii) a stack of both composites as input features.



#############################################################################
# 2) Investigating model performance
#############################################################################


#############################################################################
# 3) Final model parametrization and variable importances
#############################################################################


#############################################################################
# 4) Classification
#############################################################################


#############################################################################
# Advanced assignment: Automated hyperparameter optimization
#############################################################################

#############################################################################
# Advanced assignment: Support-Vector Machines (SVMs)
#############################################################################

