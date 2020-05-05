# Assignment 1
# Brad Klassen

######################
##### Question 1 #####
######################

# Set my working directory
setwd('/Users/bradklassen/Brock_University/Grad/MATH5P87/Data/')

# Read in long data csv file
long_data = read.csv('sample_long_format.csv')

# Convert to wide
wide_data = reshape(long_data, idvar = 'observation', timevar = 'variable', v.names = 'value', direction = 'wide')

# Remove 'value.' from column names
names(wide_data) = gsub('value.', '', names(wide_data))

# Reset index
rownames(wide_data) = NULL

# Read in wide data to verify
wide_data_ex = read.csv('sample_wide_format.csv')

# Print data sets to ensure they are the same
print(wide_data)
print(wide_data_ex)

######################
##### Question 2 #####
######################

# Read in data for question 2
q2_data = read.csv('assignment1-q2.csv', stringsAsFactors = FALSE)

# a)

# Rename the variable `x' to `x1'
q2_data[q2_data$variable == 'x', 'variable'] = 'x1'

# b)

# Remove all rows corresponding to observation 2
q2_data = q2_data[(q2_data$observation != 2),]

# c)

# Add rows to the data frame for a new observation 4 (x1 = 3, y = 2)
q2_data[nrow(q2_data) + 1,] = c('4', 'x1', '3')
q2_data[nrow(q2_data) + 1,] = c('4', 'y', '2')

# d)

# Add rows to the data frame for a new variable 
# x2 (x2(observation = 1) = 3, x2(observation = 3) = 1, x2(observation = 4) = 5)
q2_data[nrow(q2_data) + 1,] = c('1', 'x2', '3')
q2_data[nrow(q2_data) + 1,] = c('3', 'x2', '1')
q2_data[nrow(q2_data) + 1,] = c('4', 'x2', '5')

# e)

# Create a new column named 'value-squared' containing the squared y, x1 and x2
# values for each observation
q2_data$value = as.numeric(q2_data$value)
q2_data$'value-squared' = q2_data$value ^ 2

# Sort by observation, variable in ascending order
q2_data = q2_data[order(q2_data$observation, q2_data$variable),]

# Reset index
rownames(q2_data) = NULL

# f)

# Output the data frame as a csv
write.csv(q2_data, file = 'q2_answer.csv')

######################
##### Question 3 #####
######################

# Read in csv data from week 1 example
week_1_ex = read.csv('week1-example.csv')

# a)

# Create Euclidean distance function
euclidean_distance = function(x1, x2){
  # x1 and x2 are 2 dimensional vectors (inputs)
  # returns the euclidean distance between x1 and x2
  return(sqrt((x1[1] - x2[1])^2 + (x1[2] - x2[2])^2))
}

# Create l1 distance function
l1_distance = function(x1, x2){
  # x1 and x2 are 2 dimensional vectors (inputs)
  # returns the l1 distance between x1 and x2
  return(sum(abs(x1-x2)))
}

# kNN classification
kNN = function(X, Y, x_new, k, distance = euclidean_distance){ 
  
  # Number of rows in Y
  n = length(Y)	
  
  # Creates empty distance matrix
  distances = matrix(0, nrow = n)
  
  # Loop from 1 to n and store the distance between data points in the distance matrix
  # Observation to classify is x_new
  for(i in c(1:n)){
    distances[i] = distance(X[i,], x_new)
  }
  
  # Sort distances from smallest to largest 
  sorted_distances = sort(distances)
  
  # Takes the kth element of sorted distance
  sorted_distances[k]
  
  # Stores values that are less than or equal to the kth element of sorted distance
  # Where k is the number of neighbours
  my_kNN = which(distances <= sorted_distances[k])
  
  # If the mean of the my_KNN is <= 0.5 classify as 0, otherwise classify as 1
  # Return the value predicted class (y_new)
  if(mean(Y[my_kNN]) <= 0.5){
    y_new = 0
  }else{
    y_new = 1
  }
  return(y_new)
}

# b)

### Apply kNN with k = 10 to data

# Y values from week 1 example data (Binary, 0 & 1 values)
Y = week_1_ex$y

# Creates a matrix of the input variables
X = matrix(c(week_1_ex$x1, week_1_ex$x2), nrow = length(Y), ncol = 2)

# Sets the number of nearest neighbours to consider
k = 10

# Creates a matrix of 0 values for output purposes
predicted_classes = matrix(0, nrow = length(Y))

# For 1 to the number of observations in Y, run through the kNN algorithm using l1 distance
for(i in c(1:length(Y))){
  predicted_classes[i] = kNN(X, Y, X[i,], k, distance = l1_distance)
}

# Display as integer for viewing purposes
as.integer(predicted_classes == Y)

# Print classfication accuracy
mean(predicted_classes == Y)

#######################
##### Question 4 ######
#######################

# Read in prostate data csv file
prostate_data = read.csv('prostate-data.csv')

# Select the subset of columns
prostate_data = prostate_data[, names(prostate_data) %in% c('lpsa', 'lcavol', 'lweight', 'age', 'lbph', 'lcp')]

# Set seed for reproducible simulations
set.seed(0)

# Randomly sample to create indices
n = dim(prostate_data)[1]

# Set size of training data
n_training = round(n * 0.75)

# Set size of testing data
n_testing = n - n_training

# Get indices of training data
training_index = sample(c(1:n), size = n_training, replace = FALSE)

# Create training data set
training_data = prostate_data[training_index,]

# Create testing data set
testing_data = prostate_data[-training_index,]

# a)

# Using sum of squares as the performance metric for evaluation

##### Perform forward selection with k = 1 #####

model = lm(lpsa ~ lcavol, data = training_data)
sum(residuals(model)^2) / n_training

model = lm(lpsa ~ lweight, data = training_data)
sum(residuals(model)^2) / n_training

model = lm(lpsa ~ age, data = training_data)
sum(residuals(model)^2) / n_training

model = lm(lpsa ~ lbph, data = training_data)
sum(residuals(model)^2) / n_training

model = lm(lpsa ~ lcp, data = training_data)
sum(residuals(model)^2) / n_training

##### Perform forward selection with k = 2 #####

model = lm(lpsa ~ lcavol + lweight, data = training_data)
sum(residuals(model)^2) / n_training

model = lm(lpsa ~ lcavol + age, data = training_data)
sum(residuals(model)^2) / n_training

model = lm(lpsa ~ lcavol + lbph, data = training_data)
sum(residuals(model)^2) / n_training

model = lm(lpsa ~ lcavol + lcp, data = training_data)
sum(residuals(model)^2) / n_training

##### Perform forward selection with k = 3 #####

model = lm(lpsa ~ lcavol + lweight + age, data = training_data)
sum(residuals(model)^2) / n_training

model = lm(lpsa ~ lcavol + lweight + lbph, data = training_data)
sum(residuals(model)^2) / n_training

model = lm(lpsa ~ lcavol + lweight + lcp, data = training_data)
sum(residuals(model)^2) / n_training

##### Perform forward selection with k = 4 #####

model = lm(lpsa ~ lcavol + lweight + age + lbph, data = training_data)
sum(residuals(model)^2) / n_training

model = lm(lpsa ~ lcavol + lweight + +age + lcp, data = training_data)
sum(residuals(model)^2) / n_training

##### Perform forward selection with k = 5 #####

model = lm(lpsa ~ lcavol + lweight + age + lbph + lcp, data = training_data)
sum(residuals(model)^2) / n_training

# Reorder columns in training_data & testing_data
training_data = training_data[,c('lpsa', 'lcavol', 'lweight', 'age', 'lbph', 'lcp')]
testing_data = testing_data[,c('lpsa', 'lcavol', 'lweight', 'age', 'lbph', 'lcp')]

# b)

# Set k value for maximum number of columns
k = 5

# Initialize mean-squared error (MSE) values
MSE = matrix(0, nrow = k)

# Use k + 1 to always include lpsa as first column (Needed because it is the output variable)
# Test for 1 input variable to 5 to find optimal MSE value
for(i in c(1:k+1)){
  
  # Creates new training data set using the number of columns indicated by i
  training_data_1 = training_data[,seq(1,i)]
  
  # Creates new testing data set using the number of columns indicated by i
  testing_data_1 = testing_data[,seq(1,i)]
  
  # Creates model using training data
  model = lm(lpsa ~ ., data = training_data_1)
  
  # Predicted values using testing data set
  yHat = predict(model, testing_data_1)
  
  # Store MSE value in the MSE matrix  
  MSE[i-1] = sum((yHat - testing_data$lpsa) ^ 2) / n_testing
}

# Which k value minimizes MSE?
which.min(MSE)

# What is the minimum MSE?
min(MSE)

######################
##### Question 5 #####
######################

# Read in prostate data csv file
prostate_data = read.csv('prostate-data.csv')

# Select the subset of columns 
prostate_data = prostate_data[, names(prostate_data) %in% c('lpsa', 'lcavol', 'lweight', 'age')]

# a)

# lcavol * lweight
prostate_data$lcavol_lweight = prostate_data$lcavol * prostate_data$lweight
# lcavol * age
prostate_data$lcavol_age = prostate_data$lcavol * prostate_data$age
# lweight * age
prostate_data$lweight_age = prostate_data$lweight * prostate_data$age
# lcavol * lweight * age
prostate_data$lcavol_lweight_age = prostate_data$lcavol * prostate_data$lweight * prostate_data$age

# b)

# Set seed for reproducible simulations
set.seed(0)

# Randomly sample to create indices
n = dim(prostate_data)[1]

# Set size of training data
n_training = round(n * 0.75)

# Set size of testing data
n_testing = n - n_training

# Get indices of training data
training_index = sample(c(1:n), size = n_training, replace = FALSE)

# Create training data set
training_data = prostate_data[training_index,]

# Create testing data set
testing_data = prostate_data[-training_index,]

# Create input and output data sets
Y = matrix(training_data$lpsa, nrow = n_training)
X = model.matrix(lpsa ~ ., data = training_data)

# For predictions, create test input and output variables
testing_Y = matrix(testing_data$lpsa, nrow = n_testing)
testing_X = model.matrix(lpsa ~ ., data = testing_data)

# Number of inputs (not including intercepts)
p = dim(testing_X)[2] - 1

# Initialize input means and standard deviations
# Need to save them so we can apply to both training and testing data
x_mean = matrix(0, nrow = p)
x_sd = matrix(0, nrow = p)

# Get mean and standard deviation from training data
for(i in c(1:p)){
  x_mean[i] = mean(X[,i+1])
  x_sd[i] = sd(X[,i+1])
}

# Apply transformation to training and testing data
for(i in c(1:p)){
  X[,i+1] = (X[,i+1] - x_mean[i]) / x_sd[i]
  testing_X[, i+1] = (testing_X[,i+1] - x_mean[i]) / x_sd[i]
}

# c)

# A sequence of lambda values to evaluate
lambda_values = seq(from = 0, to = 60, by = 0.01)
n_lambda_values = length(lambda_values)

# Initialize empty matrix for MSE values
MSE = matrix(0, nrow = n_lambda_values)

# Create input matrix without intercept term
X0 = X[,-1]

# Perform ridge regression for a range of lambda values
for(i in c(1:n_lambda_values)){ 
  
  # Set lambda equal to the ith element of lambda_values
  lambda = lambda_values[i]
  
  # Find beta values of ridge regression
  B0Hat = solve(t(X0) %*% X0 + diag(p) * lambda) %*% t(X0) %*% (Y - mean(Y))
  BHat = c(mean(Y), B0Hat)
  
  # Predicted values using testing_X data set and beta values
  yHat = testing_X %*% BHat
  
  # Store MSE value in the MSE matrix
  MSE[i] = sum((yHat - testing_Y)^2) / n_testing
}

# Plot MSE values as a function of lambda
plot(lambda_values, MSE, xlab = expression(lambda), ylab = 'MSE', main = 'Ridge Regression - MSE of testing data', bty = 'n')

# Index of lambda value that minimizes MSE
which.min(MSE)

# Lambda value that minimizes MSE
lambda_values[which.min(MSE)]

# Minimum MSE value
min(MSE)

#####################
####### Bonus #######
#####################

t_f_matrix = function(p, k){
  
  # Create list of TRUE, FALSE values
  values = c(TRUE, FALSE)
  
  # Repeats list of TRUE, FALSE values p times
  values_p = lapply(numeric(p), function(x) values)
  
  # Finds each possible combination of the TRUE, FALSE and store in a matrix
  mat = as.matrix(expand.grid(values_p))
  
  # Gets indices of rows where the number of TRUE values is equal to k
  inds = which(rowSums(mat) == k)
  
  # Creates matrix using indices from above
  mat = mat[inds,]
  
  # Output the created matrix
  return(mat)
}

# Test the function with p = 6 & k = 3
t_f_matrix(6, 3)
